##Precision Agriculture=group
##Vector_input=Vector
##Variable=Field Vector_input
##Output=output vector

library(sp)
library(Hmisc)
library(rgeos)
library(moments)
library(raster)
library(deldir)

Vector_input=as(Vector_input,Class="Spatial")

Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]
Field=data.frame(Vector_input)
Field$Longitude=Longitude
Field$Latitude=Latitude

coordinates(Field)=~Longitude+Latitude

# Remove duplicates coordinates
Field=remove.duplicates(Field,zero=0.0)

Field=data.frame(Field)
Attribute=Variable

Field$Datage=c(1:nrow(Field))
GPS="Datage"


# Removing Zero values
Zero_values=which(Field[[Attribute]]==0)
if (length(Zero_values)!=0){
  Field=Field[-Zero_values,]
}

# DETECTING YIELD GLOBAL OUTLIERS 
# Robust estimates of location and scale
CUTOFF=function(Attrib){
  Robust_location=median(Field[[Attrib]])
  Robust_scale=median(sapply(Field[[Attrib]],function(i){abs(i-Robust_location)}))
  Medcouple=mc(Field[[Attrib]])
  
  if (Medcouple>0){
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

# ADDING DISTANCE IN TIME AND SPACE TO NEIGHBOURS
# Calculating time interval between consecutive points
GPS_shift=c(Field[[GPS]][2],Field[[GPS]][1:(nrow(Field)-1)])
Distance_shift=rbind(Field[2,c("Longitude","Latitude")],Field[1:(nrow(Field)-1),c("Longitude","Latitude")])
TIME_interval=abs(Field[[GPS]]-GPS_shift)
Temporal_distance=sqrt((Distance_shift[["Longitude"]]-Field[["Longitude"]])^2+(Distance_shift[["Latitude"]]-Field[["Latitude"]])^2)
Field$Time_interval=as.numeric(TIME_interval)
Field$Temporal_distance=as.numeric(Temporal_distance)

Robust_location_GPS=median(Field[["Time_interval"]])
Robust_scale_GPS=median(sapply(Field[["Time_interval"]],function(i){abs(i-Robust_location_GPS)}))

Robust_location_temporal_distance=median(Field[["Temporal_distance"]])
Robust_scale_temporal_distance=median(sapply(Field[["Temporal_distance"]],function(i){abs(i-Robust_location_temporal_distance)}))

cutoff_GPS=Robust_location_GPS+2*Robust_scale_GPS
cutoff_temporal_distance=Robust_location_temporal_distance+2*Robust_scale_temporal_distance

# Find not normal neighbours (far in distance and time)
Far_neighbours=which(Field$Time_interval>2*cutoff_GPS  & Field$Temporal_distance>2*cutoff_temporal_distance)


# Estimation distance entre rangs

# Construction tesselation de Voronoi pour construire les relations de voisinage entre observations
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
  
  Neighbours=c(ID,Neighbours) # On inclue observation selectionne dans son voisinage
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
    # Si on veut rajouter les voisins temporels - on peut les rajouter ici
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
Swath=median(na.exclude(do.call(rbind,Spatial_distance)))


#Iterating over all the attributes
Coords_poly=c()
Harvest=c() # Harvest polygons (merged)
Passages_complets=c()
Percent_swath=c()

Poly_area=c()
Poly_harvest=list()   #True Harvested polygons (one by one)
# Find heading direction between a point and the following point (centered line)
delta_X=Field[2,"Longitude"]-Field[1,"Longitude"]
delta_Y=Field[2,"Latitude"]-Field[1,"Latitude"]

head_dir=atan2(-delta_Y,-delta_X)

# Find perpendicular transect to centered line (90° to heading direction)
head_dir_perp=head_dir+pi/2
dx=(Swath/2)*cos(head_dir_perp)
dy=(Swath/2)*sin(head_dir_perp)

Coords_poly=data.frame(x0=Field[1,"Longitude"]+dx,
                       y0=Field[1,"Latitude"]+dy,
                       x1=Field[1,"Longitude"]-dx,
                       y1=Field[1,"Latitude"]-dy)

Percent_swath=c(Percent_swath,1) # First point cannot be corrected


# Initiate a new loop for each far neighbours to avoid relating two points in two
# different passes. Start at 2 because point number 1 is calculated before
for (point in 2:(nrow(Field)-1)){
  
  if (point %in% Far_neighbours){
    delta_X=Field[(point+1),"Longitude"]-Field[point,"Longitude"]
    delta_Y=Field[(point+1),"Latitude"]-Field[point,"Latitude"]
    
    head_dir=atan2(-delta_Y,-delta_X)
    
    # Find perpendicular transect to centered line (90° to heading direction)
    head_dir_perp=head_dir+pi/2
    dx=(Swath/2)*cos(head_dir_perp)
    dy=(Swath/2)*sin(head_dir_perp)
    # Add the coordinates of each perpendicular transect in the data frame
    Coords_poly=rbind(Coords_poly,data.frame(x0=Field[point,"Longitude"]+dx,
                                             y0=Field[point,"Latitude"]+dy,
                                             x1=Field[point,"Longitude"]-dx,
                                             y1=Field[point,"Latitude"]-dy))
    Percent_swath=c(Percent_swath,1)
  } else {
    if ((point+1) %nin% Far_neighbours){
      # Find heading direction between a point and the following point (centerline)
      delta_X=Field[point+1,"Longitude"]-Field[point,"Longitude"]
      delta_Y=Field[point+1,"Latitude"]-Field[point,"Latitude"]
      
      head_dir=atan2(-delta_Y,-delta_X)
      
      # Find perpendicular transect to centered line (90° to heading direction)
      head_dir_perp=head_dir+pi/2
      dx=(Swath/2)*cos(head_dir_perp)
      dy=(Swath/2)*sin(head_dir_perp)
      
      # Add the coordinates of each perpendicular transect in the data frame
      Coords_poly=rbind(Coords_poly,data.frame(x0=Field[point,"Longitude"]+dx,
                                               y0=Field[point,"Latitude"]+dy,
                                               x1=Field[point,"Longitude"]-dx,
                                               y1=Field[point,"Latitude"]-dy))
    } else {
      Coords_poly=rbind(Coords_poly,data.frame(x0=Field[point,"Longitude"]+dx,
                                               y0=Field[point,"Latitude"]+dy,
                                               x1=Field[point,"Longitude"]-dx,
                                               y1=Field[point,"Latitude"]-dy))
    }
    
    # Create a spatial polygon
    Size=nrow(Coords_poly)
    Pl <- Polygon(cbind(x=c(Coords_poly[Size-1,"x0"],Coords_poly[Size,"x0"],Coords_poly[Size,"x1"],Coords_poly[Size-1,"x1"],Coords_poly[Size-1,"x0"]),y=c(Coords_poly[Size-1,"y0"],Coords_poly[Size,"y0"],Coords_poly[Size,"y1"],Coords_poly[Size-1,"y1"],Coords_poly[Size-1,"y0"])))
    # Polygon data frame
    Pls <- Polygons(list(Pl), ID=point)
    # Spatial Polygon data frame
    SPls <- SpatialPolygons(list(Pls))
    # Solving self interesection issues.... (happens sometimes)
    SPls_correct <- gBuffer(SPls, byid=TRUE, width=0)
    
    
    if (length(Harvest)==0){ # for the first point
      Harvest=SPls_correct
      Percent_swath=c(Percent_swath,1)
      Poly_area=c(Poly_area,gArea(SPls_correct))
      Poly_harvest=c(Poly_harvest,SPls_correct)
    } else {
      # Check for intersection with previously formed polygons
      Inter=gIntersection(SPls_correct,Harvest)
      if (class(Inter)=="SpatialPolygons" | class(Inter)=="SpatialCollections"){
        Inter=gBuffer(Inter, byid=TRUE, width=0)  # solving self intersection
        if (!is.null(Inter)){
          if (class(gDifference(SPls_correct,Inter))!="NULL"){
            Percent_swath=c(Percent_swath,(1-(gArea(Inter)/gArea(SPls_correct))))
            Poly_area=c(Poly_area,gArea(SPls_correct))
            Poly_harvest=c(Poly_harvest,gDifference(SPls_correct,Inter))
          }
        } else {
          Percent_swath=c(Percent_swath,1)
          Poly_area=c(Poly_area,gArea(SPls_correct))
          Poly_harvest=c(Poly_harvest,SPls_correct)
        }
      } else {
        Percent_swath=c(Percent_swath,1)
        Poly_area=c(Poly_area,gArea(SPls_correct))
        Poly_harvest=c(Poly_harvest,SPls_correct)
      }
      # Merge new polygon to previous harvested polygons
      Harvest=gUnion(Harvest,SPls_correct)
      # Solving self interesection issues.... (happens sometimes)
      Harvest <- gBuffer(Harvest, byid=TRUE, width=0)
    }
  }
}
Percent_swath=c(Percent_swath,1) # Last point is not corrected either

Final_poly=do.call(bind,Poly_harvest)
Test=SpatialPolygonsDataFrame(Final_poly,data=data.frame(Recouvrement=100*(1-(Percent_swath[2:(length(Percent_swath)-1)]))))
Output=st_as_sf(Test)
