##Precision Agriculture=group 
##Vector_input=Vector
##showplots
##Sampling_method=selection Conditioned Latin Hypercube Sampling
##Coordinates_included=selection yes;no
##Number_samples=number 5
##Output=output vector

library(clhs)

Vector_input=as(Vector_input,Class="Spatial")
Name_coords=colnames(coordinates(Vector_input))
X1=coordinates(Vector_input)[,1]
X2=coordinates(Vector_input)[,2]

if (Coordinates_included==0){
Vector_input=data.frame(Vector_input)
Echantillons_id=clhs(Vector_input,size=Number_samples, iter=5000, cost=NULL,simple=FALSE)
Echantillons=Vector_input[Echantillons_id$index_samples,]

colnames(Echantillons)[which(colnames(Echantillons)%in%Name_coords)]=c("x1","x2")
coordinates(Echantillons)=~x1+x2
Output=st_as_sf(Echantillons)

} else {
Vector_input_nocoords=Vector_input@data
Echantillons_id=clhs(Vector_input_nocoords,size=Number_samples, iter=5000, cost=NULL,simple=FALSE)
Vector_input=data.frame(Vector_input)
Echantillons=Vector_input[Echantillons_id$index_samples,]
colnames(Echantillons)[which(colnames(Echantillons)%in%Name_coords)]=c("x1","x2")
coordinates(Echantillons)=~x1+x2
Output=st_as_sf(Echantillons)
}

plot(Echantillons_id, mode=c("obj","box"))

