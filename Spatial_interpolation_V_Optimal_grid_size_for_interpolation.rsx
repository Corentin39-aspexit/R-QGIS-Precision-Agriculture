##Precision Agriculture=group
##Contour=vector polygon
##Points=vector point
##Variable=Field Points
##Model=selection Spherique;Exponentiel
##Range=number 10
##Nugget_effect=number 1
##Partial_sill=number 5
##Grid_size=output table

library(lamW)
library(rgeos)

Contour=as(Contour,Class="Spatial")
Points=as(Points,Class="Spatial")

List_modeles=c("Sph","Exp")


sp_density=nrow(Points)/gArea(Contour)
C1P=Partial_sill

# Taille optimale de grille
if (List_modeles[Model+1]=="Sph"){
  Size=ceiling(2*Re(polyroot(c(-Range^3,0,0,3*sp_density*C1P*Range^2,0,3*sp_density*C1P))[1]))
} else {
  Size=ceiling(-6*Range*lambertW0((-1/6)*(2^(2/3))/((sp_density*Range^2)^(1/3))))
}

Grid_size=data.frame(Size=Size)