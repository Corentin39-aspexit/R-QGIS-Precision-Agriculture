##Precision Agriculture=group 
##Vector_input=Vector
##Filtering_method=selection Spatial_Mahalanobis;Spatial_DBSCAN
##Percentile=number 97.5
##Distance_neighbourhood=number 10
##Espilon_neighbourhood=number 0.5
##Minimum_points_in_epsilon_region=number 50
##Outlying_borders=selection FALSE;TRUE
##Outliers_handling=selection Remove;Keep
##Output=output vector


library(robustbase)
library(dbscan)

List_borders=c(FALSE,TRUE)

Vector_input=as(Vector_input,Class="Spatial")
Vector_input=remove.duplicates(Vector_input)
Vector_input$Longitude=coordinates(Vector_input)[,1]
Vector_input$Latitude=coordinates(Vector_input)[,2]
Field=data.frame(Vector_input)

Proximity=list()

for (ID in 1:nrow(Field)){
  Neighbours=which(Field$Longitude<Field[ID,"Longitude"]+Distance_neighbourhood & Field$Longitude>Field[ID,"Longitude"]-Distance_neighbourhood & Field$Latitude<Field[ID,"Latitude"]+Distance_neighbourhood & Field$Latitude>Field[ID,"Latitude"]-Distance_neighbourhood)
  if (length(Neighbours)>2){
    Diff=c()
    for (i in c(1,2)){
      Diff=c(Diff,median(Field[ID,i]-Field[Neighbours,i]))
    } 
    Proximity[[ID]]=Diff
  } else {
    Proximity[[ID]]=NA
  }
}

Diff_final=data.frame(do.call(rbind,Proximity))


if (Filtering_method==0){
    p=ncol(Vector_input@data)-2
    chisqqu=Percentile/100
    qchi <- sqrt(qchisq(chisqqu,p))
      
    # Mahalanobis distance
    Compute_mahalanobis=function(Correlation,Names_attributes){
        Maha_distances=c()
        covr <- covMcd(Correlation[,Names_attributes])
        cinv <- solve(covr$cov) # inverse of the robust covariance matrix
        MDglobal <- sqrt(mahalanobis(Correlation[,Names_attributes], covr$center, cinv, inverted=TRUE))
        Maha_distances=cbind(Maha_distances,MDglobal)
        return(Maha_distances)
      }

      # First computation of Mahalanobis distance to identify Outliers
      test_outliers=Compute_mahalanobis(Diff_final,colnames(Diff_final))
      Outliers=which(test_outliers>qchi)

} else {

Vector_input.mat=as.matrix(Diff_final)
  
  # Applying DBSCAN
  Clustering_dbscan=dbscan(x=Vector_input.mat,eps=Espilon_neighbourhood,minPts=Minimum_points_in_epsilon_region,borderPoints=List_borders[Outlying_borders+1])
  Vector_input$DIST=Clustering_dbscan[[1]]
  # Extracting the group with maximum number of observations
  Nbr_points_cluster=data.frame()
  for (num in unique(Clustering_dbscan[[1]])){
    Nbr_points_cluster=rbind(Nbr_points_cluster,data.frame(Nbr_points=length(which(Clustering_dbscan[[1]]==num)),
                                                           Cluster=num))
  }
  Max_cluster=Nbr_points_cluster$Cluster[which(Nbr_points_cluster$Nbr_points==max(Nbr_points_cluster$Nbr_points))]
  
  # Retrieving outliers
  Outliers=which(Clustering_dbscan[[1]]!=Max_cluster)
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
