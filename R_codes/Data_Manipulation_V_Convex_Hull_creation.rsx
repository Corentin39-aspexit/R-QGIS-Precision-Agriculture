##Precision Agriculture=group
##Vector_input=Vector
##Contour_points=output vector

library(rgeos)
Vector_input=as(Vector_input,Class="Spatial")
Enveloppe_convexe=gConvexHull(Vector_input)
proj4string(Enveloppe_convexe)=proj4string(Vector_input)
Contour_points=st_as_sf(Enveloppe_convexe)