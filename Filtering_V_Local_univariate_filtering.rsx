##Precision Agriculture=group 
##Vector_input=Vector
##Variable=Field Vector_input
##Filtering_method=selection CV;IDW;Moran;Normal
##Maximum_CV=number 25
##Distance_neighbourhood=number 10
##Min_distance_neighbours=number 0
##Max_distance_neighbours=number 20
##Number_standard_deviations=number 2
##Outliers_handling=selection Remove;Keep
##Output=output vector

library(spdep)
library(robustbase)

Vector_input=as(Vector_input,Class="Spatial")
Vector_input=remove.duplicates(Vector_input)
Vector_input$Longitude=coordinates(Vector_input)[,1]
Vector_input$Latitude=coordinates(Vector_input)[,2]
Field=data.frame(Vector_input)


if (Filtering_method==0){

    Nbr_neighbours=c()   # Number of neighbours of each observation
    Final_CV=c()         # Coefficient of variation in the neighbourhood of each observation
    Nbr_High_CV=c()      # Number of CV higher than Max_CV in the neighbourhood

    for (ID in 1:nrow(Field)){
        Neighbours=which(Field$Longitude<Field[ID,"Longitude"]+Distance_neighbourhood & Field$Longitude>Field[ID,"Longitude"]-Distance_neighbourhood & Field$Latitude<Field[ID,"Latitude"]+Distance_neighbourhood & Field$Latitude>Field[ID,"Latitude"]-Distance_neighbourhood)
        CV_neighbours=100*sd(Field[Neighbours,Variable])/mean(Field[Neighbours,Variable])
        Nbr_neighbours=c(Nbr_neighbours,length(Neighbours))
        Final_CV=c(Final_CV,CV_neighbours)
      }
      
      # Adding the attributes to the dataset
      Field$Nb_neighbours=Nbr_neighbours
      Field$CV_Yield=Final_CV
      
      # Obtaining the number of points with CV higher than the threshold
      for (ID in 1:nrow(Field)){
        Neighbours=which(Field$Longitude<Field[ID,"Longitude"]+Distance_neighbourhood & Field$Longitude>Field[ID,"Longitude"]-Distance_neighbourhood & Field$Latitude<Field[ID,"Latitude"]+Distance_neighbourhood & Field$Latitude>Field[ID,"Latitude"]-Distance_neighbourhood)
        High_CV=which(Field[Neighbours,"CV_Yield"]>Maximum_CV)
        Nbr_High_CV=c(Nbr_High_CV,length(High_CV))
      }
      
      # Adding the attributes to the dataset
      Field$Nb_High_CV=Nbr_High_CV

      # Spotting spatial outliers
      Outliers=which(Field$Nb_High_CV>=(Field$Nb_neighbours-1))

} else if (Filtering_method==1){

    Local_outliers=c()
    for (num in 1:nrow(Field)){
        Neighbours=which(Field$Longitude<Field[num,"Longitude"]+Distance_neighbourhood & Field$Longitude>Field[num,"Longitude"]-Distance_neighbourhood & Field$Latitude<Field[num,"Latitude"]+Distance_neighbourhood & Field$Latitude>Field[num,"Latitude"]-Distance_neighbourhood)
        Neighbours=unique(Neighbours)
        Neighbours=Neighbours[! Neighbours %in% num]
            if(length(Neighbours)>1){
            Yield_interpol_numerateur=sum(sapply(Neighbours,function(i){Field[i,Variable]/(sqrt((Field[i,"Longitude"]-Field[num,"Longitude"])^2+(Field[i,"Latitude"]-Field[num,"Latitude"])^2))^2}))
            Yield_interpol_denominateur=sum(sapply(Neighbours,function(i){1/(sqrt((Field[i,"Longitude"]-Field[num,"Longitude"])^2+(Field[i,"Latitude"]-Field[num,"Latitude"])^2))^2}))
            Yield_interpol=Yield_interpol_numerateur/Yield_interpol_denominateur
              if (Field[num,Variable]<Yield_interpol+Number_standard_deviations*sd(Field[Neighbours,Variable]) & Field[num,Variable]>Yield_interpol-Number_standard_deviations*sd(Field[Neighbours,Variable])){
              } else {
                Local_outliers=c(Local_outliers,num)
              }
            } else {
              Local_outliers=c(Local_outliers,num)
            }
    }

    Outliers=Local_outliers

} else if (Filtering_method==2){

# Find the neighbours of each observations given a circular neighbour
  Neighbourhood <- dnearneigh(Vector_input, Min_distance_neighbours, Max_distance_neighbours)
# Find the distance of each neighbour to each observation
  dlist <- nbdists(Neighbourhood, coordinates(Vector_input))
# Apply a weigh to each neighbour given the distance to the observation
  idlist <- lapply(dlist, function(x) 1/sqrt(x))
# Create a list of weight for each neighbour
   Weights <- nb2listw(Neighbourhood, glist=idlist, style="W")
# Calculate local Moran Index
  Final_MORAN=data.frame(localmoran(Field[[Variable]],Weights))
# Spot outliers
  Outliers=which(Final_MORAN$Ii<0 | Final_MORAN$Ii>4.7)

} else if (Filtering_method==3){

    Local_outliers=c()

    for (ID in 1:nrow(Field)){
        Neighbours=which(Field$Longitude<Field[ID,"Longitude"]+Distance_neighbourhood & Field$Longitude>Field[ID,"Longitude"]-Distance_neighbourhood & Field$Latitude<Field[ID,"Latitude"]+Distance_neighbourhood & Field$Latitude>Field[ID,"Latitude"]-Distance_neighbourhood)
        if (length(Neighbours)>=2){ # calculation of variance needs at least 2 points
            Num=which(Neighbours %in% ID)
            Neighbours=Neighbours[-Num]
            if (Field[ID,Variable]<mean(Field[Neighbours,Variable])-Number_standard_deviations*sd(Field[Neighbours,Variable]) | Field[ID,Variable]>mean(Field[Neighbours,Variable])+Number_standard_deviations*sd(Field[Neighbours,Variable])){
                Local_outliers=c(Local_outliers,ID)
            }
        } else {
          Local_outliers=c(Local_outliers,ID)
        }
    Outliers=Local_outliers
    }
}

# Handling outliers
if (Outliers_handling==0){
    if (length(Outliers)!=0){
    Vector_input=Vector_input[-Outliers,]
    }
} else {
Vector_input$Outliers="NO"
    if (length(Outliers)!=0){
    Vector_input$Outliers[Outliers]="YES"
    }
}

Output=st_as_sf(Vector_input)

