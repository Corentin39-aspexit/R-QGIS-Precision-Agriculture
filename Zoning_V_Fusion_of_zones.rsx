##Precision Agriculture=group
##Zoning=vector polygon
##Variable=Field Zoning
##Method=selection Number_zones;Minimal_distance;Both
##Final_number_zones=number 3
##Threshold=number 0.5
##Final_zoning=output vector

library(rgeos)
library(maptools)

Zoning=as(Zoning,Class="Spatial")
Zones=Zoning
STOP="NON"

# Fusion until number of zones is reached
if (Method==0){

    while(length(Zones)>Final_number_zones){

    Zones@data$DN=c(1:length(Zones))
    Zones@data=subset(Zones@data,select=c("DN",Variable))

    Area_zones=c()
    for (i in 1:length(Zones)){
      Area_zones=c(Area_zones,gArea(Zones[i,]))
    }
    Zones@data$Area=Area_zones

    Neighbours_zones = gIntersects(Zones, byid =TRUE)
    diag(Neighbours_zones)=FALSE

    Difference_matrix=apply(Zones@data,1,function(x){abs((as.numeric(x[2])*as.numeric(x[3]))-((Zones@data[[Variable]])*Zones@data$Area))/(as.numeric(x[3])+Zones@data$Area)})
    Difference_matrix.true=Difference_matrix*Neighbours_zones
    Minimal_distance=apply(Difference_matrix.true,1,function(x){
                            Vecteur_no_0=x[!x %in% 0]
                            Zone=which(x%in%min(Vecteur_no_0))
                            return(data.frame(nearest_zone=Zone,distance=min(Vecteur_no_0)))
    })

    Dist_file=do.call(rbind,Minimal_distance)
    Row_merge=which.min(Dist_file$distance)

      Zones@data$DN[Row_merge]=Dist_file$nearest_zone[Row_merge]
      
      # Calculate new value
      New_value=((gArea(Zones[Row_merge,])*Zones@data[[Variable]][Row_merge])+(gArea(Zones[Dist_file$nearest_zone[Row_merge],])*Zones@data[[Variable]][Dist_file$nearest_zone[Row_merge]]))/(gArea(Zones[Row_merge,])+gArea(Zones[Dist_file$nearest_zone[Row_merge],]))
      Zones@data[[Variable]][c(Row_merge,Dist_file$nearest_zone[Row_merge])]=New_value
      
      New_df=aggregate(Zones@data[[Variable]],by=list(Zones@data$DN),FUN=mean)
      colnames(New_df)=c("DN",Variable)
      
      # Construct new polygon
      Merge_zone = unionSpatialPolygons(Zones, Zones$DN)
      row.names(New_df)=sapply(slot(Merge_zone, "polygons"), function(x) slot(x, "ID"))
      Final_merge_V2 <- SpatialPolygonsDataFrame(Merge_zone, New_df)
      Zones <- gBuffer(Final_merge_V2, byid = TRUE, width = 0)

    }

}


# Fusion until minimal difference between zones is reached
if (Method==1){
    while(STOP=="NON"){
      
      Zones@data$DN=c(1:length(Zones))
      Zones@data=subset(Zones@data,select=c("DN",Variable))
      
      Area_zones=c()
      for (i in 1:length(Zones)){
        Area_zones=c(Area_zones,gArea(Zones[i,]))
      }
      Zones@data$Area=Area_zones
      
      Neighbours_zones = gIntersects(Zones, byid =TRUE)
      diag(Neighbours_zones)=FALSE
      
      Difference_matrix=apply(Zones@data,1,function(x){abs(as.numeric(x[2])-(Zones@data[[Variable]]))})
      Difference_matrix.true=Difference_matrix*Neighbours_zones
      Minimal_distance=apply(Difference_matrix.true,1,function(x){
        Vecteur_no_0=x[!x %in% 0]
        Zone=which(x%in%min(Vecteur_no_0))
        return(data.frame(nearest_zone=Zone,distance=min(Vecteur_no_0)))
      })
      
      Dist_file=do.call(rbind,Minimal_distance)
      Row_merge=which.min(Dist_file$distance)
      
      if (Dist_file$distance[Row_merge]<Threshold){
        Zones@data$DN[Row_merge]=Dist_file$nearest_zone[Row_merge]
        
        # Calculate new value
        New_value=((gArea(Zones[Row_merge,])*Zones@data[[Variable]][Row_merge])+(gArea(Zones[Dist_file$nearest_zone[Row_merge],])*Zones@data[[Variable]][Dist_file$nearest_zone[Row_merge]]))/(gArea(Zones[Row_merge,])+gArea(Zones[Dist_file$nearest_zone[Row_merge],]))
        Zones@data[[Variable]][c(Row_merge,Dist_file$nearest_zone[Row_merge])]=New_value
        
        New_df=aggregate(Zones@data[[Variable]],by=list(Zones@data$DN),FUN=mean)
        colnames(New_df)=c("DN",Variable)
        
        # Construct new polygon
        Merge_zone = unionSpatialPolygons(Zones, Zones$DN)
        row.names(New_df)=sapply(slot(Merge_zone, "polygons"), function(x) slot(x, "ID"))
        Final_merge_V2 <- SpatialPolygonsDataFrame(Merge_zone, New_df)
        Zones <- gBuffer(Final_merge_V2, byid = TRUE, width = 0)
      } else {
        STOP="OUI"
      }
      
    }
}


# Fusion until number of zones is reached. Constraints on the minimal difference between zones
if (Method==2){

while(length(Zones)>Final_number_zones & STOP=="NON"){
  
  Zones@data$DN=c(1:length(Zones))
  Zones@data=subset(Zones@data,select=c("DN",Variable))
  
  Area_zones=c()
  for (i in 1:length(Zones)){
    Area_zones=c(Area_zones,gArea(Zones[i,]))
  }
  Zones@data$Area=Area_zones
  
  Neighbours_zones = gIntersects(Zones, byid =TRUE)
  diag(Neighbours_zones)=FALSE
  
  Difference_matrix=apply(Zones@data,1,function(x){abs(as.numeric(x[2])-(Zones@data[[Variable]]))})
  Difference_matrix.true=Difference_matrix*Neighbours_zones
  Minimal_distance=apply(Difference_matrix.true,1,function(x){
    Vecteur_no_0=x[!x %in% 0]
    Zone=which(x%in%min(Vecteur_no_0))
    return(data.frame(nearest_zone=Zone,distance=min(Vecteur_no_0)))
  })
  
  Dist_file=do.call(rbind,Minimal_distance)
  Row_merge=which.min(Dist_file$distance)
  
  if (Dist_file$distance[Row_merge]<Threshold){
    Zones@data$DN[Row_merge]=Dist_file$nearest_zone[Row_merge]
    
    # Calculate new value
    New_value=((gArea(Zones[Row_merge,])*Zones@data[[Variable]][Row_merge])+(gArea(Zones[Dist_file$nearest_zone[Row_merge],])*Zones@data[[Variable]][Dist_file$nearest_zone[Row_merge]]))/(gArea(Zones[Row_merge,])+gArea(Zones[Dist_file$nearest_zone[Row_merge],]))
    Zones@data[[Variable]][c(Row_merge,Dist_file$nearest_zone[Row_merge])]=New_value
    
    New_df=aggregate(Zones@data[[Variable]],by=list(Zones@data$DN),FUN=mean)
    colnames(New_df)=c("DN",Variable)
    
    # Construct new polygon
    Merge_zone = unionSpatialPolygons(Zones, Zones$DN)
    row.names(New_df)=sapply(slot(Merge_zone, "polygons"), function(x) slot(x, "ID"))
    Final_merge_V2 <- SpatialPolygonsDataFrame(Merge_zone, New_df)
    Zones <- gBuffer(Final_merge_V2, byid = TRUE, width = 0)
  } else {
    STOP="OUI"
  }
  
  
}
}

Final_zoning=st_as_sf(Zones)


