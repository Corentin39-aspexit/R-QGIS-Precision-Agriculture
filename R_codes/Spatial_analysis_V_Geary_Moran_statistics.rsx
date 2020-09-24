##Precision Agriculture=group
##Vector_input=Vector
##Variable=Field Vector_input
##Number_k_near_neighbours=number 10
##Maximum_lag_order=number 5
##Index=selection Moran;Geary
##Method=selection Spatial_test;Monte_Carlo
##Output=output table

library(spdep)
library(rgeos)

Vector_input=as(Vector_input,Class="Spatial")

# Construction voisinage
nlist <- knn2nb(knearneigh(coordinates(Vector_input),k=Number_k_near_neighbours))
W <- nb2listw(nlist,style="W",zero.policy=T)

# Test de Moran
if (Index==0){
    if (Method==0){
    Stats_Moran=moran.test(Vector_input[[Variable]], listw=W,alternative="greater")
    Output=data.frame(observed_Moran_statistic=Stats_Moran$estimate[1],standard_deviate_Moran=Stats_Moran$statistic,p_value=Stats_Moran$p.value)
    } else {
    Stats_Moran=moran.mc(Vector_input[[Variable]],listw=W,nsim=1000,alternative="greater")
    Output=data.frame(Moran=Stats_Moran$statistic,observed_rank=Stats_Moran$parameter,p_value=Stats_Moran$p.value)
    }
} else {
if (Method==0){
    Stats_Geary=geary.test(Vector_input[[Variable]], listw=W,alternative="greater")
    Output=data.frame(observed_Geary_statistic=Stats_Geary$estimate[1],standard_deviate_Geary=Stats_Geary$statistic,p_value=Stats_Geary$p.value)
    } else {
    Stats_Geary=moran.mc(Vector_input[[Variable]],listw=W,nsim=1000,alternative="greater")
    Output=data.frame(Geary=Stats_Geary$statistic,observed_rank=Stats_Geary$parameter,p_value=Stats_Geary$p.value)
    }
}





