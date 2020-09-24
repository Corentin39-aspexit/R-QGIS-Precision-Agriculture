##Precision Agriculture=group 
##Vector_input=vector point
##Contour=vector polygon
##Mesh_length=number 10
##Mesh_width=number 10
##Final_grid=output vector

library(rgeos)
library(dplyr)

Machine_width=Mesh_length
Machine_length=Mesh_width

Vector_input=as(Vector_input,Class="Spatial")
Contour=as(Contour,Class="Spatial")

Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]
Field=data.frame(Vector_input)
Field$Longitude=Longitude
Field$Latitude=Latitude


head_dir = c()
for (point in 2:(nrow(Field) - 1)) {
  delta_X = Field[point + 1, "Longitude"] - Field[point, "Longitude"]
  delta_Y = Field[point + 1, "Latitude"] - Field[point, "Latitude"]
  Angle_degree = atan2(-delta_Y, -delta_X) * 180 / pi
  if (Angle_degree < 0) {
    Angle_degree = Angle_degree + 180
  }
  head_dir = c(head_dir, Angle_degree)
}


Principal_headir = median(head_dir)
Angle_header = -Principal_headir


Bounding_box=bbox(Contour)
Max_length_bb = sqrt((Bounding_box[1, 1] - Bounding_box[1, 2]) ^ 2 + (Bounding_box[2, 1] - Bounding_box[2, 2]) ^ 2)
N_poly=1   
Poly_order=c()


Poly=list()
Y_init_0 = Bounding_box[2, 1] - Max_length_bb / 5
INIT = data.frame(x0 = Bounding_box[1, 1] - Max_length_bb / 5,
                  y0 = Y_init_0,
                  x1 = Bounding_box[1, 1] - Max_length_bb / 5 + Machine_length, 
                  y1 = Y_init_0 + Machine_width)


X_rot=(Bounding_box[1,2]+Bounding_box[1,1])/2
Y_rot=(Bounding_box[2,1]+Bounding_box[2,2])/2


for (j in 1:round((Bounding_box[2,2]-Bounding_box[2,1]+ Max_length_bb / 2)/Machine_width)){
  for (i in 0:floor((Bounding_box[1,2]-Bounding_box[1,1]+ Max_length_bb / 2)/Machine_length)){
    
    Coords_poly=data.frame(x0=INIT$x0+i*Machine_length,
                           y0=INIT$y0,
                           x1=INIT$x1+i*Machine_length,
                           y1=INIT$y1)
   
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
    
    
    Pls <- Polygons(list(Pl), ID=N_poly)
    Poly[[N_poly]]=Pls
    Poly_order=c(Poly_order,N_poly)
    N_poly=N_poly+1
    
  }
  
  
  INIT = data.frame( x0 = Bounding_box[1, 1] - Max_length_bb / 5,
                     y0 = Y_init_0 + j * Machine_width,
                     x1 = Bounding_box[1, 1] - Max_length_bb / 5 + Machine_length,
                     y1 = Y_init_0 + Machine_width + j * Machine_width)
}


SPls <- SpatialPolygons(Poly)
SPls_data=SpatialPolygonsDataFrame(SPls,data=data.frame(Num=c(1:length(SPls))))
SPls_data$Poly_order=Poly_order
proj4string(SPls_data)=proj4string(Contour)

Poly_intersect = gIntersects(SPls_data, Contour, byid = TRUE)
True_poly_intersect = which(Poly_intersect == TRUE)
Poly_in_field = gIntersection(SPls_data, Contour, byid = TRUE)


ORDER = data.frame(Previous_order = Poly_order[True_poly_intersect],
                   New_order = c(1:length(Poly_in_field)))
ORDER_V1 = arrange(ORDER, ORDER$Previous_order)
Poly_ordered = Poly_in_field[ORDER_V1$New_order, ]
Poly_ordered = SpatialPolygonsDataFrame(Poly_ordered,
                                        data = data.frame(Order = c(1:length(Poly_ordered))),
                                        match.ID = FALSE)

Final_grid=st_as_sf(Poly_ordered)



