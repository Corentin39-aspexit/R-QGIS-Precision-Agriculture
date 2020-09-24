##Precision Agriculture=group
##Zoning=vector polygon
##Points=vector point
##Variable=Field Points
##Index=selection Reduction_variance;Zoning_opportunity
##Machine_width=number 10
##Step=number 0.1

##Output=output table

library(sp)
library(maptools)
library(rgeos)
library(plyr)
library(Hmisc)

Zoning=as(Zoning,Class="Spatial")
Points=as(Points,Class="Spatial")


if (Index==0){
    Over_Zones=over(Points,Zoning,byid=TRUE)
    Points$Zone=Over_Zones$DN
          
    # Suppression des donnees en dehors des zones
    NA_zone=which(is.na(Points$Zone)==TRUE)
    if (length(NA_zone)!=0){
        Points=Points[-NA_zone,]
    }
          
# Calcul de reduction de variance avec la carte des sols
Area_weighted_variance=0

for (zone_vigueur in unique(Points$Zone)){
  Subset=Points[which(Points$Zone%in%zone_vigueur),]
  Prop_variance=var(Subset[[Variable]])*(nrow(Subset)/nrow(Points))
  Area_weighted_variance=Area_weighted_variance+Prop_variance
}

RV=1-(Area_weighted_variance/var(Points[[Variable]]))
Output=data.frame(Reduc_variance=RV, Mean=mean(Points[[Variable]],na.rm=TRUE),CV=cv(Points[[Variable]],na.rm=TRUE), Nbre_zones=length(Zoning))

} else {


Points$Longitude=coordinates(Points)[,1]
Points$Latitude=coordinates(Points)[,2]
Field=data.frame(Points)
joined=Zoning
joined$id=1
Contour=unionSpatialPolygons(joined,joined$id)

# BUILD A GRID OVER THE FIELD ORIENTED IN THE DIRECTION OF POINTS
    Machine_length=Machine_width
    Fuzzy_width=Machine_width+Machine_width/5
    Fuzzy_length=Machine_length+Machine_length/5

    # Bouding box of contour
    Bounding_box=bbox(Contour)
    Max_length_bb=sqrt((Bounding_box[1,1]-Bounding_box[1,2])^2+(Bounding_box[2,1]-Bounding_box[2,2])^2)
    N_poly=1   # Number of polygons
    Poly_order=c()

    # Find principal header direction
    head_dir=c()
    for (point in 2:(nrow(Field)-1)){
      delta_X=Field[point+1,"Longitude"]-Field[point,"Longitude"]
      delta_Y=Field[point+1,"Latitude"]-Field[point,"Latitude"]
      Angle_degree=atan2(-delta_Y,-delta_X)*180/pi
      if (Angle_degree<0){
        Angle_degree=Angle_degree+180
      }
      head_dir=c(head_dir,Angle_degree)
    }
    # Take the median of all the header direction to have a single grid within the field
    Principal_headir=median(head_dir)
    Angle_header=-Principal_headir

    # Compute the first polygon of the grid
    Poly=list()
    # Translate the polygons
    Y_init_0=Bounding_box[2,1]-Max_length_bb/5

    INIT=data.frame(x0=Bounding_box[1,1]-Max_length_bb/5,
                    y0=Y_init_0,
                    x1=Bounding_box[1,1]-Max_length_bb/5+Fuzzy_length,
                    y1=Y_init_0+Fuzzy_width)

    # Set the rotation center of the data based on the bouding box of the contour
    X_rot=(Bounding_box[1,2]+Bounding_box[1,1])/2
    Y_rot=(Bounding_box[2,1]+Bounding_box[2,2])/2

    # Create all polygons within the grid. POlygons are created column by column
    for (j in 1:floor((Bounding_box[2,2]-Bounding_box[2,1]+Max_length_bb/2)/Fuzzy_width)){
      for (i in 1:floor((Bounding_box[1,2]-Bounding_box[1,1]+Max_length_bb/2)/Fuzzy_length)){
        # Create a polygon
        Coords_poly=data.frame(x0=INIT$x0+i*Fuzzy_length,
                               y0=INIT$y0,
                               x1=INIT$x1+i*Fuzzy_length,
                               y1=INIT$y1)
        # Rotate the polygon
        Pl <- Polygon(cbind(x=c(X_rot+(Coords_poly$x0-X_rot)*cos(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*sin(Angle_header*(pi/180)),
                                X_rot+(Coords_poly$x1-X_rot)*cos(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*sin(Angle_header*(pi/180)),
                                X_rot+(Coords_poly$x1-X_rot)*cos(Angle_header*(pi/180))+(Coords_poly$y1-Y_rot)*sin(Angle_header*(pi/180)),
                                X_rot+(Coords_poly$x0-X_rot)*cos(Angle_header*(pi/180))+(Coords_poly$y1-Y_rot)*sin(Angle_header*(pi/180)),
                                X_rot+(Coords_poly$x0-X_rot)*cos(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*sin(Angle_header*(pi/180))),
                            y=c(Y_rot-(Coords_poly$x0-X_rot)*sin(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*cos(Angle_header*(pi/180)),
                                Y_rot-(Coords_poly$x1-X_rot)*sin(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*cos(Angle_header*(pi/180)),
                                Y_rot-(Coords_poly$x1-X_rot)*sin(Angle_header*(pi/180))+(Coords_poly$y1-Y_rot)*cos(Angle_header*(pi/180)),
                                Y_rot-(Coords_poly$x0-X_rot)*sin(Angle_header*(pi/180))+(Coords_poly$y1-Y_rot)*cos(Angle_header*(pi/180)),
                                Y_rot-(Coords_poly$x0-X_rot)*sin(Angle_header*(pi/180))+(Coords_poly$y0-Y_rot)*cos(Angle_header*(pi/180)))))
        
        # Polygon data frame
        # Machine passes in adjacent lines in opposite directions
        # Give IDS to the polygons
        if (j%%2!=0){
          Pls <- Polygons(list(Pl), ID=N_poly)
          Poly[[N_poly]]=Pls
          Poly_order=c(Poly_order,N_poly)
          N_poly=N_poly+1
          
        } else {
          Pls <- Polygons(list(Pl), ID=N_poly-2*i+1+floor((Bounding_box[1,2]-Bounding_box[1,1]+Max_length_bb/2)/Fuzzy_length))
          Poly[[N_poly]]=Pls
          Poly_order=c(Poly_order,N_poly-2*i+1+floor((Bounding_box[1,2]-Bounding_box[1,1]+Max_length_bb/2)/Fuzzy_length))
          N_poly=N_poly+1
        }
      }
      # Initiate a new column in the grid to reconstruct polygons
      INIT=data.frame(x0=Bounding_box[1,1]-Max_length_bb/5,
                      y0=Y_init_0+j*Fuzzy_width,
                      x1=Bounding_box[1,1]-Max_length_bb/5+Fuzzy_length,
                      y1=Y_init_0+Fuzzy_width+j*Fuzzy_width)
    }
    # Spatial Polygon data frame
    SPls <- SpatialPolygons(Poly)
    SPls_data=SpatialPolygonsDataFrame(SPls,data=data.frame(Num=c(1:length(SPls))))
    SPls_data$Poly_order=Poly_order

    # == Retrieve only intersecting polygons 
    Poly_intersect=gIntersects(SPls_data,Contour,byid=TRUE)
    True_poly_intersect=which(Poly_intersect==TRUE)
    Poly_in_field=gIntersection(SPls_data,Contour,byid=TRUE)

    # == Rearrange polygons in right order
    ORDER=data.frame(Previous_order=Poly_order[True_poly_intersect],New_order=c(1:length(Poly_in_field)))
    ORDER_V1=arrange(ORDER,ORDER$Previous_order)
    Poly_ordered=Poly_in_field[ORDER_V1$New_order,]
    Poly_ordered=SpatialPolygonsDataFrame(Poly_ordered,data=data.frame(Order=c(1:length(Poly_ordered))),match.ID=FALSE)
    Poly_ordered$ORDER=c(1:length(Poly_ordered))

    # Attribute areas to each polygon created
    Area_intersect=c()
    for (j in 1:length(Poly_ordered)){
      Area_intersect=c(Area_intersect,gArea(Poly_ordered[j,]))
    }
    Poly_ordered$AREA=Area_intersect



    # == Clean the spatial grid of polygons (Some polygons near boundaries are very small) ===
    # Find neighbours of each polygons
    Neighbours_poly=gTouches(Poly_ordered,byid=TRUE)
    # Select those polygons that have an area inferior to 1/3 of the spatial footprint
    Small_poly=which(Poly_ordered$AREA<1/3*(Fuzzy_width*Fuzzy_length))
    Large_poly=which(c(1:length(Poly_ordered)) %nin% Small_poly)

    # For all the small polygons, merge them with neighbours
    for (poly in Small_poly){
      Neighbours=which(Neighbours_poly[poly,]==TRUE)
      Large_neighbours=which(Neighbours %nin% Small_poly)
      # If forward and backwards neighbours are small, then merge with neighbours in adjacent columns
      if (((poly-1) %in% Small_poly & (poly+1) %in% Small_poly)| poly==1 | poly==length(Poly_ordered)){
        if (length(Large_neighbours)==3){
          Merging_neighbours=Neighbours[Large_neighbours[2]]
          Poly_ordered@data$ORDER[poly]=Merging_neighbours
        } else {
          k=1
          # Otherwise, look for the neighbours that share a side with the polygon and merge with it
          while (k<=length(Large_neighbours)){
            Result_inter=gIntersection(Poly_ordered[poly,],Poly_ordered[Neighbours[Large_neighbours[k]],])
            if (class(Result_inter)=="SpatialLines"){
              Poly_ordered@data$ORDER[poly]=Neighbours[Large_neighbours[k]][[1]]
              k=length(Large_neighbours)+1
            } else {
              k=k+1
            }
          }
        }
        # if backwards poly is small but forward poly is not, then the small polygon is merged to the forward polygons
      } else if ((poly-1) %in% Small_poly & (poly+1) %nin% Small_poly){
        Poly_ordered@data$ORDER[poly]=poly+1
      } else {
        Poly_ordered@data$ORDER[poly]=poly-1
      }
    }


    # Merge the zones according to the new zones values
    # Associating new zones values
    Merge_Poly_ordered=unionSpatialPolygons(Poly_ordered,Poly_ordered$ORDER)
    # Convert SpatialPolygons to data frame
    Poly_ordered.df <- as(Poly_ordered, "data.frame")
    # Aggregate and sum desired data attributes by ID list
    Poly_ordered.df.agg <- aggregate(Poly_ordered.df[,c("Order","AREA")], list(Poly_ordered$ORDER), mean)
    row.names(Poly_ordered.df.agg) <- as.character(Poly_ordered.df.agg$Group.1)
    # Reconvert data frame to SpatialPolygons
    Final_merge_Poly_ordered<- SpatialPolygonsDataFrame(Merge_Poly_ordered, Poly_ordered.df.agg)
    colnames(Final_merge_Poly_ordered@data)=c("ORDER","Order","AREA")

    # Reordering polygons
    Final_merge_Poly_ordered=Final_merge_Poly_ordered[order(Final_merge_Poly_ordered$ORDER),] 
    Final_merge_Poly_ordered$ORDER=c(1:length(Final_merge_Poly_ordered))




# PREPARING DATASET FOR ZOI CALCULATION
    Data_test=SpatialPolygonsDataFrame(Sr=joined, data=data.frame(Num_zone=c(1:length(joined))),FALSE)
    Field_sp=Field
    coordinates(Field_sp)=~Longitude+Latitude
    proj4string(Field_sp)=proj4string(Data_test)
    Data_overlay=over(Field_sp,Data_test)
    Data_overlay$Yield_zone=0

# Remove points outside the zones
    NA_values=which(is.na(Data_overlay$Num_zone)==TRUE)
    if (length(NA_values)!=0){
      Data_overlay=Data_overlay[-NA_values,]
      Field=Field[-NA_values,]
      Field_sp=Field_sp[-NA_values,]
    }

    # Calculate median yield in each polygon
    for (i in c(1:length(joined))){
      Sub=which(Data_overlay$Num_zone==i)
      Data_overlay$Yield_zone[Sub]=median(Field[[Variable]][Sub])
    }

    Field_inter=data.frame(Field_sp)
    Field_inter=cbind(Field_inter,Data_overlay)

    colnames(Field_inter)[which(colnames(Field_inter)%in%"Num_zone")]="Zone"
    SP_Field=Field_inter
    coordinates(SP_Field)=~Longitude+Latitude


    # Retrieve the points belonging to each spatial footprint
    proj4string(SP_Field)=proj4string(Final_merge_Poly_ordered)
    Points_intersect=over(SP_Field,Final_merge_Poly_ordered)
    SP_Field@data=cbind(SP_Field@data,Points_intersect)

    # Create new dataset with the points IDs, the number of the rectangle and the zones...
    Final_points=data.frame(Point=c(1:nrow(SP_Field)),
                            Yield=SP_Field@data[[Variable]],
                            Num_footprint=SP_Field@data$ORDER,
                            Num_Zone=SP_Field@data$Zone,
                            Yield_Zone=as.numeric(paste(SP_Field@data$Yield_zone)))


    Final_points$Zone_footprint=1
    Final_points$Yield_footprint=1
    for (footprint in unique(Final_points$Num_footprint)){
      Num_line=which(Final_points$Num_footprint==footprint)
      Points_per_zone=table(Final_points[Num_line,"Num_Zone"])
      Zone_final=as.data.frame(Points_per_zone)$Var1[which.max(as.data.frame(Points_per_zone)$Freq)]
      Final_points$Zone_footprint[Num_line]=as.numeric(paste(Zone_final[1]))
      Yield_value_footprint=unique(Final_points$Yield_Zone[which(Final_points$Num_Zone==as.numeric(paste(Zone_final[1])))])
      Final_points$Yield_footprint[Num_line]=Yield_value_footprint
    }

    # Add a new column corresponding to the treatment level that the machine is able to reach
    Final_points$True_treatment=Final_points$Yield_footprint
    for (yield_foot in unique(Final_points$Yield_footprint)){
      Num_line=which(Final_points$Yield_footprint==yield_foot)
      Residuals_treatment=yield_foot%%Step
      if (Residuals_treatment<=Step/2){
        Final_points$True_treatment[Num_line]=yield_foot-Residuals_treatment
      } else {
        Final_points$True_treatment[Num_line]=yield_foot-Residuals_treatment+Step
      }
    }

    # Calculating the ZOI Index

    ZOI_calc=function(TAB){
      # Variable rate management
      Variable_rate_management=sum(sapply(c(1:nrow(TAB)),function(i){(TAB$Yield[i]-TAB$True_treatment[i])^2}))
      # Uniform management
      Mean_treatment=mean(TAB$Yield)
      Residuals_mean_treatment=Mean_treatment%%Step
      if (Residuals_mean_treatment<=Step/2){
        Mean_treatment=Mean_treatment-Residuals_mean_treatment
      } else {
        Mean_treatment=Mean_treatment-Residuals_mean_treatment+Step
      }
      
      Uniform_management=sum(sapply(c(1:nrow(TAB)),function(i){(TAB$Yield[i]-Mean_treatment)^2}))
      
      # Calculating ZOI
      ZOI=1-(Variable_rate_management/Uniform_management)
      return(ZOI)
    }

    Variable_rate_management=sapply(c(1:nrow(Final_points)),function(i){(Final_points$Yield[i]-Final_points$True_treatment[i])^2})
    # Uniform management
    Mean_treatment=mean(Final_points$Yield)
    Residuals_mean_treatment=Mean_treatment%%Step
    if (Residuals_mean_treatment<=Step/2){
      Mean_treatment=Mean_treatment-Residuals_mean_treatment
    } else {
      Mean_treatment=Mean_treatment-Residuals_mean_treatment+Step
    }
    Uniform_management=sum(sapply(c(1:nrow(Final_points)),function(i){(Final_points$Yield[i]-Mean_treatment)^2}))

#  ZOI INDEX
    Output=data.frame(Index=ZOI_calc(Final_points),Mean=mean(Points[[Variable]],na.rm=TRUE),CV=cv(Points[[Variable]],na.rm=TRUE), Nbre_zones=length(Zoning))

}