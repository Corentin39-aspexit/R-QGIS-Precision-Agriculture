##Precision Agriculture=group 
##Vector_input=vector polygon
##Sampling_method=selection Aleatoire;Regulier;Stratifie;NonAligne;Clustered
##Number_samples=number 1
##Output=output vector

library(classInt)

Methodes=c("Aleatoire","Regulier","Stratifie","NonAligne","Clustered")

Vector_input=as(Vector_input,Class="Spatial")

if (Methodes[Sampling_method+1]=="Aleatoire"){
Echantillons=spsample(Vector_input,n=Number_samples,type="random")
} else if (Methodes[Sampling_method+1]=="Regulier"){
Echantillons=spsample(Vector_input,n=Number_samples,type="regular")
} else if (Methodes[Sampling_method+1]=="Stratifie"){
Echantillons=spsample(Vector_input,n=Number_samples,type="stratified")
} else if (Methodes[Sampling_method+1]=="NonAligne"){
Echantillons=spsample(Vector_input,n=Number_samples,type="nonaligned")
} else if (Methodes[Sampling_method+1]=="Clustered"){
Echantillons=spsample(Vector_input,n=Number_samples,type="clustered")
}

Output=st_as_sf(Echantillons)