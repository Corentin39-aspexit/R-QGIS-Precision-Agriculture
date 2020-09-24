##Precision Agriculture=group
##Vector_input=Vector
##Variable=Field Vector_input
##Distance_neighbours=number 10
##Output=output table

library(spdep)
library(rgeos)

Vector_input=as(Vector_input,Class="Spatial")

# Construction voisinage
nlist <- dnearneigh(coordinates(Vector_input),0,Distance_neighbours)
W <- nb2listw(nlist, style="B", zero.policy=TRUE)

# Indice de Getis_ord
Stats_Getis=globalG.test(Vector_input[[Variable]], W, zero.policy=TRUE, alternative="two.sided")
Output=data.frame(observed_Getis_statistic=Stats_Getis$estimate[1],standard_deviate_Getis=Stats_Getis$statistic,p_value=Stats_Getis$p.value)
