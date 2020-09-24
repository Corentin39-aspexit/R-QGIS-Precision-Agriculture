##Precision Agriculture=group
##showplots   
##Vector_input=Vector
##Variable=Field Vector_input
##Max_dist_vario=number 50
##Lag=number 5
##trend=selection constante;spatiale 
##Variogram_data=output table

library(gstat)

Vector_input=as(Vector_input,Class="Spatial")

if (trend==0){
Vario_vector=variogram(Vector_input[[Variable]]~1,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
Variogram_data=Vario_vector
plot(Vario_vector)
} else {
Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]
Vario_vector=variogram(Vector_input[[Variable]]~Longitude+Latitude,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
Variogram_data=Vario_vector
plot(Vario_vector)
}