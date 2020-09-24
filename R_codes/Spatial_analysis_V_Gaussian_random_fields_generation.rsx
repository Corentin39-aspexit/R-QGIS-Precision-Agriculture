##Precision Agriculture=group 
##Vector_input=vector polygon
##Sampling_method=selection Random;Regular;Stratified;NonAligned;Clustered
##Number_points=number 100
##Mean_variable=number 5
##Coefficient_of_variation=number 0.2
##Partial_sill=number 75
##Nugget_effect=number 25
##Range=number 30
##Model=selection Sph;Exp;Gau;Lin
##Number_of_simulations=number 1
##Simulated_data=output vector

library(classInt)
library(gstat)
library(sp)
library(rgdal)

Methodes=c("Random","Regular","Stratified","NonAligned","Clustered")
List_modeles=c("Sph","Exp","Gau","Lin")

Vector_input=as(Vector_input,Class="Spatial")

if (Methodes[Sampling_method+1]=="Random"){
Echantillons=spsample(Vector_input,n=Number_points,type="random")
} else if (Methodes[Sampling_method+1]=="Regular"){
Echantillons=spsample(Vector_input,n=Number_points,type="regular")
} else if (Methodes[Sampling_method+1]=="Stratified"){
Echantillons=spsample(Vector_input,n=Number_points,type="stratified")
} else if (Methodes[Sampling_method+1]=="NonAligned"){
Echantillons=spsample(Vector_input,n=Number_points,type="nonaligned")
} else if (Methodes[Sampling_method+1]=="Clustered"){
Echantillons=spsample(Vector_input,n=Number_points,type="clustered")
}


Coords.df=data.frame(coordinates(Echantillons))
colnames(Coords.df)=c("x","y")

# Spatial structure
Beta=Mean_variable
CV=Coefficient_of_variation 
Psill=(Partial_sill/100)*(Beta*CV)^2
Nugget=(Nugget_effect/100)*(Beta*CV)^2   


# Defining a spatial model for the yield
g.var <- gstat(formula=z~1, # Simple/Ordinary kriging
                 locations=~x+y, 
                 dummy=T, beta=Beta, # Here, beta stands for the simple kriging mean
                 model=vgm(psill=Psill, range=Range ,nugget=Nugget, model=List_modeles[Model+1]),
                 nmax=100) # nmax stands for the number of neighbors used to create the variogram
# Simulating spatial data
Simulation <- predict(g.var, newdata=Coords.df, nsim=Number_of_simulations) # nsim : number of simulations
coordinates(Simulation)=~x+y
Simulated_data=st_as_sf(Simulation)