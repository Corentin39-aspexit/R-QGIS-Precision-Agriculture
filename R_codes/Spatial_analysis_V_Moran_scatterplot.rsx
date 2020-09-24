##Precision Agriculture=group
##showplots
##Vector_input=Vector
##Variable=Field Vector_input
##Number_k_near_neighbours=number 10
##Influence_measures_values=output table
##Influence_measures_true_false=output table

library(spdep)
library(rgeos)

Vector_input=as(Vector_input,Class="Spatial")

# Construction voisinage
nlist <- knn2nb(knearneigh(coordinates(Vector_input),k=Number_k_near_neighbours))
W <- nb2listw(nlist,style="W",zero.policy=T)

Moran_scatterplot <- moran.plot(Vector_input[[Variable]],W,xlab="Y: detrended", ylab="WY: spatially lagged detrended")

Influence_measures_values=Moran_scatterplot$infmat
Influence_measures_true_false=Moran_scatterplot$is.inf