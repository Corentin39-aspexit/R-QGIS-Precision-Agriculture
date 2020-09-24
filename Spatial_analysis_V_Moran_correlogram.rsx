##Precision Agriculture=group
##showplots
##Vector_input=Vector
##Variable=Field Vector_input
##Number_k_near_neighbours=number 10
##Maximum_lag_order=number 5


library(spdep)
library(rgeos)

Vector_input=as(Vector_input,Class="Spatial")

# Construction voisinage
nlist <- knn2nb(knearneigh(coordinates(Vector_input),k=Number_k_near_neighbours))
W <- nb2listw(nlist,style="W",zero.policy=T)

Moran_correlogram <- sp.correlogram(nlist,Vector_input[[Variable]],order=Maximum_lag_order,method="I",style="W",zero.policy=T)
plot(Moran_correlogram)
