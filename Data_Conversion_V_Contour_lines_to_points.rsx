##Precision Agriculture=group
##Contour_lines=vector
##Output=output vector

library(rgeos)
Lignes_contour=as(Contour_lines,Class="Spatial")
Points_contour <- as(Lignes_contour, "SpatialPointsDataFrame")
Output=st_as_sf(Points_contour)
