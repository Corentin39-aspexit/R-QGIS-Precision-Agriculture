##Precision Agriculture=group 
##showplots
##Vector_input=Vector
##Variable=Field Vector_input
##Latence_value=output table

library(deldir)
library(rgeos)
library(rgdal)
library(doParallel)
library(robustbase)

Vector_input=as(Vector_input,Class="Spatial")
Vector_input=remove.duplicates(Vector_input)
Vector_input$Longitude=coordinates(Vector_input)[,1]
Vector_input$Latitude=coordinates(Vector_input)[,2]
Field=data.frame(Vector_input)
Field$Datage=c(1:nrow(Field))
GPS="Datage"
coordinates(Field)=~Longitude+Latitude

N_CORES=1

# On supprime les doublons
Field=remove.duplicates(Field,zero=0.0)
Field=data.frame(Field)

# On enlve les valeurs nulles
Zero_values=which(Field[[Variable]]==0)
if (length(Zero_values)!=0){
  Field=Field[-Zero_values,]
}

if (nrow(Field)!=0){
  
  # Detection des donnees aberrantes globales
  Robust_location=median(Field[[Variable]])
  Robust_scale=median(sapply(Field[[Variable]],function(i){abs(i-Robust_location)}))
  
  # On calcule les bornes au dessus et en dessous desquelles les donnees sont considerees aberrantes
  CUTOFF=function(Attrib){
    Robust_location=median(Field[[Attrib]])
    Robust_scale=median(sapply(Field[[Attrib]],function(i){abs(i-Robust_location)}))
    Medcouple=mc(Field[[Attrib]])
    
    if (Medcouple>0){ # Utilisation de deux methodes differentes (On enleve les donnees qui sont aberrantes dans les deux methodes)
      whisker_1=as.numeric(summary(Field[[Attrib]])[2])-1.5*exp((-4)*mc(Field[[Attrib]]))*(as.numeric(summary(Field[[Attrib]])[5])-as.numeric(summary(Field[[Attrib]])[2]))
      whisker_2=as.numeric(summary(Field[[Attrib]])[5])+1.5*exp(3*mc(Field[[Attrib]]))*(as.numeric(summary(Field[[Attrib]])[5])-as.numeric(summary(Field[[Attrib]])[2]))
    } else {
      whisker_1=as.numeric(summary(Field[[Attrib]])[2])-1.5*exp((-3)*mc(Field[[Attrib]]))*(as.numeric(summary(Field[[Attrib]])[5])-as.numeric(summary(Field[[Attrib]])[2]))
      whisker_2=as.numeric(summary(Field[[Attrib]])[5])+1.5*exp(4*mc(Field[[Attrib]]))*(as.numeric(summary(Field[[Attrib]])[5])-as.numeric(summary(Field[[Attrib]])[2]))
    }
    
    Field[[paste("AOI",Attrib,sep="_")]]=0
    # Calculating AOI values
    AOI_1=which(Field[[Attrib]]>=Robust_location)
    Field[AOI_1,paste("AOI",Attrib,sep="_")]=(Field[AOI_1,Attrib]-Robust_location)/(whisker_2-Robust_location)
    AOI_2=which(Field[[Attrib]]<Robust_location)
    Field[AOI_2,paste("AOI",Attrib,sep="_")]=(Robust_location-Field[AOI_2,Attrib])/(Robust_location-whisker_1)
    
    # Cutoff value for outliers
    cutoff=as.numeric(summary(Field[[paste("AOI",Attrib,sep="_")]])[5])+1.5*exp(3*mc(Field[[paste("AOI",Attrib,sep="_")]]))*(as.numeric(summary(Field[[paste("AOI",Attrib,sep="_")]])[5])-as.numeric(summary(Field[[paste("AOI",Attrib,sep="_")]])[2]))
    return(list(Field,cutoff))
  }
  
  # On supprime les donnees aberrantes (application de la fonction CUTOFF plus haut)
  Field=CUTOFF(Variable)[[1]]
  cutoff_yield=CUTOFF(Variable)[[2]]
  
  # Calculating angle
  angle <- function(x,y){
    dot.prod <- x%*%y 
    norm.x <- norm(x,type="2")
    norm.y <- norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
  }
  
  # DETECTION DES DEBUTS ET FINS DE RANGS 
  # On regarde les intervalles dans le temps et espace entre des observations consecutives pour detecter les bouts de rangs 
  GPS_shift=c(Field[[GPS]][2],Field[[GPS]][1:(nrow(Field)-1)])
  Distance_shift=rbind(Field[2,c("Longitude","Latitude")],Field[1:(nrow(Field)-1),c("Longitude","Latitude")])
  TIME_interval=abs(Field[[GPS]]-GPS_shift)
  Temporal_distance=sqrt((Distance_shift[["Longitude"]]-Field[["Longitude"]])^2+(Distance_shift[["Latitude"]]-Field[["Latitude"]])^2)
  Field$Time_interval=as.numeric(TIME_interval)   # Intervalle de temps entre deux observations
  Field$Temporal_distance=as.numeric(Temporal_distance)  # Distance entre deux observations
  
  # On detecte les bouts de rangs avec les observations qui ont des intervalles de temps et/ou espace aberrants
  Robust_location_GPS=median(Field[["Time_interval"]])
  Robust_scale_GPS=median(sapply(Field[["Time_interval"]],function(i){abs(i-Robust_location_GPS)}))
  Robust_location_temporal_distance=median(Field[["Temporal_distance"]])
  Robust_scale_temporal_distance=median(sapply(Field[["Temporal_distance"]],function(i){abs(i-Robust_location_temporal_distance)}))
  # Seuils de detection
  cutoff_GPS=Robust_location_GPS+2*Robust_scale_GPS
  cutoff_temporal_distance=Robust_location_temporal_distance+2*Robust_scale_temporal_distance
  # Detection des aberrants ==> Utilisation intervalle de temps entre deux observations. Si il y a trop de temps, observations
  # pas acquises dans le meme rang
  Far_neighbours=which(Field$Time_interval>10*cutoff_GPS)
  

  # ESTIMATION DE LA DISTANCE MOYENNE ENTRE RANGS ADJACENTS 
  # oCnstruction tesselation de Voronoi pour construire les relations de voisinage entre observations
  z <- deldir(x=Field$Longitude,y=Field$Latitude)
  # On selectionne un sous echantillon des observations pour accelerer le temps de traitement
  Subset_data=sample(unique(z$dirsgs$ind1),size=floor(0.20*length(unique(z$dirsgs$ind1))))
  
  # Pour chaque observation selectionnee, trouver les plus proches voisins spatiaux dans les rangs adjacents
  Neighbourood=list()
  for (ID in unique(Subset_data)){
    Num_neighbours=which(z$dirsgs$ind1==ID)  # Numeros des voisins 
    Neighbours=z$dirsgs[Num_neighbours,]$ind2 # Voisins spatiaux (dans les rangs adjacents) et temporels (dans le meme rang)
    
    # Correspondance entre chaque point et le jeu de donnees initial
    ID=z$ind.orig[ID]
    Neighbours=z$ind.orig[Neighbours]
    
    Neighbours=c(ID,Neighbours) # On inclue observation selectionnee dans son voisinage
    Neighbours=Neighbours[order(Neighbours)] # On ordonne les identifiants des points
    # Identifiants des points adjacents
    rr <- rle(Neighbours - seq_along(Neighbours))
    rr$values <- seq_along(rr$values)
    # Sequence des polygones adjacents pour trouver les voisins spatiaux et temporels
    Sequences=split(Neighbours, inverse.rle(rr))
    
    # Separer les voisins spatiaux des voisins temporels
    Spatial_neighbours=c()
    for (j in unique(inverse.rle(rr))){
      if (is.element(ID,Sequences[j][[1]])=="FALSE"){
        Spatial_neighbours=c(Spatial_neighbours,Sequences[j][[1]])
      }
    }
    if (length(Spatial_neighbours)!=0){
      # Si on veut rajouter les voisins temporels, on peut les rajouter ici
      Spatial_neighbourood=list(ID,Spatial_neighbours)
      Neighbourood=append(Neighbourood,list(Spatial_neighbourood))
    } else {
    }
  }
  
  # On estime la distance moyenne entre les rangs adjacents (Distance_spatial_neighbours)
  Spatial_distance=lapply(Neighbourood,function(spatial_point){
    Dist_moy_spatial=median(sqrt((Field[c(spatial_point[[2]]),"Longitude"]-Field[spatial_point[[1]],"Longitude"])^2+(Field[c(spatial_point[[2]]),"Latitude"]-Field[spatial_point[[1]],"Latitude"])^2))
    return(Dist_moy_spatial)
  })
  # On recupere la distance moyenne entre deux observations de rangs adjacents
  Distance_spatial_neighbours=median(na.exclude(do.call(rbind,Spatial_distance)))
  

  # DEFINITION DU VOISINAGE DE CHAQUE OBSERVATION
  
  cl <- makeCluster(N_CORES)
  registerDoParallel(cl,cores=N_CORES)
  
  Neighbourood=foreach(ID=unique(1:nrow(Field)-1),.packages = c('sp')) %dopar% {
    print(ID)
    # Definintion du voisinage
    Neighbours=which(Field$Longitude<Field[ID,"Longitude"]+2*Distance_spatial_neighbours & Field$Longitude>Field[ID,"Longitude"]-2*Distance_spatial_neighbours & Field$Latitude<Field[ID,"Latitude"]+2*Distance_spatial_neighbours & Field$Latitude>Field[ID,"Latitude"]-2*Distance_spatial_neighbours)
    Wrong_neighbours=which(Neighbours %in% Far_neighbours) #On ne prend pas en compte les observations des bouts de rang
    if (length(Wrong_neighbours)!=0){
      Neighbours=Neighbours[-Wrong_neighbours]
    }
    
    Neighbours=Neighbours[order(Neighbours)] # On ordonne les points dans le voisinage
    # We need at least one spatial neighbour to compare with.
    if (length(Neighbours)>3){ 
      # ID of adjacent points
      rr <- rle(Neighbours - seq_along(Neighbours))
      rr$values <- seq_along(rr$values)
      # On recupere les sequences de points qui se suiventdans le voisinage; chaque sequence correspond à un rang
      Sequences=split(Neighbours, inverse.rle(rr))
      
      # Si trop de sequences differentes dans le voisinage, on doit etre pres des bords de parcelle donc on en prend pas ces cas là;
      # On prend entre 2 et 7 sequences maximum
      if (length(Sequences)>2 & length(Sequences)<7) {
        
        # Differentiation des observations dans le meme rang que observation i (temporal neighbours) avec les observations dans un rang different (spatial neighbours)
        Spatial_neighbours=c()
        Temporal_neighbours=c()
        for (j in unique(inverse.rle(rr))){
          if (is.element(ID,Sequences[j][[1]])=="FALSE"){
            Spatial_neighbours=c(Spatial_neighbours,Sequences[j][[1]])
          } else {
            Temporal_sequence=j
          }
        }
        
        # Si pas de voisins spatiaux, on ne prend pas en compte observation i pour faire la correction            
        if (length(Spatial_neighbours)!=0){
          
          if (exists("Temporal_sequence")==TRUE){
            if (is.null(Temporal_sequence)==FALSE){
              # On supprime les donnees dans le même rang que i
              Sequences=Sequences[-Temporal_sequence]
            }
          }
          
          Temporal_sequence=c()
          # On ne garde ensuite que les observations dans des rangs recoltes dans une direction opposee a celui dans lequel est observation i
          # Si on prend des rangs adjacents recoltes dans le meme sens, les voisins vont rester les memes quand on va decaler les points
          # Avec des rangs recoltes dans des directions opposes, les voisins de chaque observation vont evoluer au fur et à mesure quon teste des decalages differents.
          True_spatial_neighbours=c()
          i=0
          
          Neighbours_in_space=cbind(subset(Field[unlist(Sequences),],select=c("Longitude","Latitude")),DIST=spDistsN1(as.matrix(Field[unlist(Sequences),c("Longitude","Latitude")]),as.matrix(Field[ID,c("Longitude","Latitude")]),longlat=FALSE))
          Neighbours_in_space$ID=unlist(Sequences)
          
          while (i<2 & length(Sequences)!=0){
            Nearest_neighbour=which(Neighbours_in_space$DIST==min(Neighbours_in_space$DIST))
            Nearest_neighbour=Nearest_neighbour[1] # in case multi occurences
            Find_sequence=lapply(Sequences,function(seq){unlist(Sequences)[Nearest_neighbour] %in% seq})
            # Selecting the pass
            Pass=Sequences[[which(Find_sequence==TRUE)]]
            # Removing the pass from the sequence
            Sequences=Sequences[-which(Find_sequence==TRUE)]
            if (min(Neighbours_in_space$DIST)<1.5*Distance_spatial_neighbours){
              if (length(Pass)>2){
                # Checking for colinearity
                Angle_0=180*(angle(c(Field[ID,"Longitude"]-Field[ID+1,"Longitude"],Field[ID,"Latitude"]-Field[ID+1,"Latitude"]),c(Field[Pass[1],"Longitude"]-Field[Pass[2],"Longitude"],Field[Pass[1],"Latitude"]-Field[Pass[2],"Latitude"])))/pi
                if (is.na(Angle_0)==FALSE){
                  if (abs(Angle_0)>157.5 & abs(Angle_0)<180){
                    True_spatial_neighbours=c(True_spatial_neighbours,Pass)
                    i=i+1
                  }
                } else {
                  i=i+1
                }
              }
            } else {
              i=i+1
            }
            
            Obs_to_remove=which(Neighbours_in_space$ID%in%Pass)
            Neighbours_in_space=Neighbours_in_space[-Obs_to_remove,]
            
          }
        }
        
        if (length(True_spatial_neighbours)!=0){
          # Looking for global outliers (point or neighbours)
          outliers=which(Field[True_spatial_neighbours,paste("AOI",Variable,sep="_")]>cutoff_yield)
          if (length(outliers)!=0){
            True_spatial_neighbours=True_spatial_neighbours[-outliers]
          }
          
          # If we want to include Temporal neighbours, add it to the list
          Spatial_neighbourood=list(ID,True_spatial_neighbours)
          return(Spatial_neighbourood)
        }
      }
    }
  }  
  
  stopCluster(cl)
  
  # Suppression des valeurs nulles
  Neighbourood=Neighbourood[!sapply(Neighbourood, is.null)] 
  
  # Suppression des voisinages nuls; ils ne serviront pas dans la decouverte du decalage
  Null_neighbourhood=lapply(Neighbourood,function(spatial_point){
    Nbr_neighours=length(spatial_point[[2]])
  })
  Count_neighbours=do.call(rbind,Null_neighbourhood)
  Zero_neighbours=which(Count_neighbours<1)
  if (length(Zero_neighbours)!=0){
    Neighbourood=Neighbourood[-Zero_neighbours]
  }
  
  # On ne fait pas le calcul pour tous les points pour simplifier le calcul
  if (length(Neighbourood)>1000){
    Neighbourood=Neighbourood[sample(c(1:length(Neighbourood)),size=length(Neighbourood)/3)]
  }
  
  if (length(Neighbourood)!=0){
    
    #CALCUL DU TEMPS DE LATENCE
    
    # Paralellisation
    cl <- makeCluster(N_CORES)
    registerDoParallel(cl,cores=N_CORES)
    
    # On cherche un decalage de 20 points vers avant jusqua 20 points vers arriere
    Calc_latence=foreach(offset=seq(-20,20,by=2),.combine=rbind) %dopar% {
      # Definition des voisinages si on decale les observations
      Spatial_difference=lapply(Neighbourood,function(spatial_point){
        lag=abs(offset)
        if (offset<0){
          Problem=seq(from=spatial_point[[1]], to=(spatial_point[[1]]+lag),by=1)
          Jumps=which(diff(spatial_point[[2]]) != 1)
          if (length(Jumps)==0){
            Problem_2=seq(from=spatial_point[[2]][1], to=(spatial_point[[2]][length(spatial_point[[2]])]+lag),by=1)
          } else {
            Problem_2=seq(from=spatial_point[[2]][1], to=(spatial_point[[2]][Jumps[1]]+lag),by=1)
            Jumps=c(Jumps,length(spatial_point[[2]]))
            for (i in 1:(length(Jumps)-1)){
              Problem_2=c(Problem_2,seq(from=spatial_point[[2]][(Jumps[i]+1)], to=(spatial_point[[2]][Jumps[i+1]]+lag),by=1))
            }
          }
          
        } else {
          Problem=seq(from=(spatial_point[[1]]-lag), to=spatial_point[[1]],by=1)
          Jumps=which(diff(spatial_point[[2]]) != 1)
          if (length(Jumps)==0){
            Problem_2=seq(from=(spatial_point[[2]][1]-lag), to=spatial_point[[2]][length(spatial_point[[2]])],by=1)
          } else {
            Problem_2=seq(from=(spatial_point[[2]][1]-lag), to=spatial_point[[2]][Jumps[1]],by=1)
            Jumps=c(Jumps,length(spatial_point[[2]]))
            for (i in 1:(length(Jumps)-1)){
              Problem_2=c(Problem_2,seq(from=(spatial_point[[2]][(Jumps[i]+1)]-lag), to=spatial_point[[2]][Jumps[i+1]],by=1))
            }
          }
        }
        
        # Dans chaque voisinage, on supprime les points problematiques (notamment les bouts de rangs)
        Wrong_points=which(spatial_point[[2]]%in%intersect(Far_neighbours,Problem_2))
        if (length(Wrong_points)!=0){
          spatial_point[[2]]=spatial_point[[2]][-Wrong_points]
        }
        
        if (length(intersect(Far_neighbours,Problem))==0 & length(spatial_point[[2]])>1){
          # On calcule la variance dans chaque voisinage
          if (min(spatial_point[[2]])>=abs(2*offset)){
            Difference=median(abs((Field[c(spatial_point[[2]]+2*(offset)),c(Variable)])-Field[spatial_point[[1]],c(Variable)]))
          } else {
            Difference=NA
          }
        } else {
          Difference=NA
        }
        return(Difference)
      })
      
      # Suppression des valeurs NA
      NA_values=which(is.na(Spatial_difference)==TRUE)
      if (length(NA_values)!=0){
        Spatial_difference=Spatial_difference[-NA_values]
      }
      # Pour un decalage donne, on fait la moyenne de la variance dans le voisinage de chaque observation
      Lag_times=mean(do.call(cbind,Spatial_difference))
      return(Lag_times)
    }
    
    # Stop cluster
    stopCluster(cl)
    
    Calc_latence=as.data.frame(Calc_latence)
    Calc_latence$offset=seq(from=-20,to=20,by=2)
    colnames(Calc_latence)=c("Ecart","Latence")
    
    NA_values=which(is.na(Calc_latence$Ecart)==TRUE)
    if(length(NA_values)!=0){
      Calc_latence=Calc_latence[-NA_values,]
    }
    
    Latence=Calc_latence$Latence[which(Calc_latence$Ecart==min(Calc_latence$Ecart))]
    
    
  }  else {
    Latence=0
  }
  
} else{
  Latence=0
}

Latence_value=Latence

plot(Calc_latence$Latence,Calc_latence$Ecart)