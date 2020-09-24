##Precision Agriculture=group
##showplots   
##Vector_input=Vector
##Max_dist_vario=number 50
##Lag=number 5
##Variable=Field Vector_input
##trend=selection constante;spatiale
##Range=number 0
##Nugget=number 0
##Sill=number 0
##Model=selection Linear;Spherical;Gaussian;Exponential
##Variogram_fitting=output table
##Indices_SP=output table

library(gstat)

Vector_input=as(Vector_input,Class="Spatial")
List_modeles=

if (trend==0){
Vario_vector=variogram(Vector_input[[Variable]]~1,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
} else {
Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]
Vario_vector=variogram(Vector_input[[Variable]]~Longitude+Latitude,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
}

if (Model==0){
Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Lin", Range, Nugget))
} else if (Model==1){
Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Sph", Range, Nugget))
} else if (Model==2){
Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Gau", Range, Nugget))
} else if (Model==3){
Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Exp", Range, Nugget))
}

plot(Vario_vector,Vario_vector.model)

Variogram_fitting=data.frame(Modele=Model,Range=round(Vario_vector.model$range[2],2),Nugget=round(Vario_vector.model$psill[1],2),Sill=round(Vario_vector.model$psill[1]+Vario_vector.model$psill[2],2))
Indices_SP=data.frame(CV=round(sd(Vector_input[[Variable]])/mean(Vector_input[[Variable]]),2),
                      Cambardella = round(100*Vario_vector.model$psill[1]/(Vario_vector.model$psill[1]+Vario_vector.model$psill[2]),2) ,
                      MCD = round((3/8)*Vario_vector.model$psill[1]/(Vario_vector.model$psill[1]+Vario_vector.model$psill[2])*Vario_vector.model$range[2],2))
