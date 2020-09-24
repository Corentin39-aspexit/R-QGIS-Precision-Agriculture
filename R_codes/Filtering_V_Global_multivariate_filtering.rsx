##Precision Agriculture=group 
##Vector_input=Vector
##Filtering_method=selection Mahalanobis;DBSCAN
##Percentile=number 97.5
##Espilon_neighbourhood=number 0.5
##Minimum_points_in_epsilon_region=number 50
##Outlying_borders=selection FALSE;TRUE
##Outliers_handling=selection Remove;Keep
##Vector_output=output vector

library(robustbase)
library(dbscan)

Vector_input=as(Vector_input,Class="Spatial")
List_borders=c(FALSE,TRUE)

if (Filtering_method==0){
    p=ncol(Vector_input@data)
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
      test_outliers=Compute_mahalanobis(Vector_input@data,colnames(Vector_input@data))
      Outliers=which(test_outliers>qchi)
} else {
    Vector_input.mat=as.matrix(Vector_input@data)
  
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

Vector_output=st_as_sf(Vector_input)



