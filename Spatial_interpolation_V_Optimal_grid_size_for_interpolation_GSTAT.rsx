##Precision Agriculture=group
##showplots 
##Vector_input=Vector
##Variable=Field Vector_input
##Model=selection Linear;Spherical;Gaussian;Exponential
##Range=number 10
##Nugget_effect=number 1
##Partial_sill=number 5
##Min_spacing=number 2
##Max_spacing=number 10
##Min_block_size=number 1
##Max_block_size=number 30
##Max_number_neighbours=number 25
##BK_standard_errors=output table



library(gstat)
library(lattice)

Vector_input=as(Vector_input,Class="Spatial")
List_modeles=c("Lin","Sph","Gau","Exp")

Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]
Vector_input$Longitude=Longitude
Vector_input$Latitude=Latitude

# Analyse variographique
Vario_data=variogram(Vector_input[[Variable]]~Longitude+Latitude,data=Vector_input)
Vario_fit=fit.variogram(Vario_data,model=vgm(Partial_sill, List_modeles[Model+1], Range, Nugget_effect))

# Calcul des tailles de grille
Test_optim=ossfim(spacings = Min_spacing:Max_spacing,block.sizes = Min_block_size:Max_block_size,model=Vario_fit,nmax=Max_number_neighbours)
levelplot(kriging.se~spacing+block.size, Test_optim, main = "Ossfim results")
BK_standard_errors=Test_optim