##Precision Agriculture=group 
##Vector_input=vector
##Variable=Field Vector_input
##Sampling_method=selection Aleatoire;Quantiles;K-means
##Number_groups=number 2
##Number_samples_in_each_group=number 3
##Output=output vector

library(classInt)

Methodes=c("Aleatoire","Quantiles","K-means")

Vector_input=as(Vector_input,Class="Spatial")

New_data=data.frame(Longitude=coordinates(Vector_input)[,1],
                    Latitude=coordinates(Vector_input)[,2],
                    Valeur=Vector_input@data[[Variable]])
coordinates(New_data)=~Longitude+Latitude

if (Methodes[Sampling_method+1]=="Aleatoire"){
Samples=sample(c(1:nrow(New_data)),size=Number_samples_in_each_group)
Echantillons=New_data[Samples,]
Output=st_as_sf(Echantillons)
} else {
        if (Methodes[Sampling_method+1]=="Quantiles"){
        zClass <- classIntervals(New_data$Valeur, n=Number_groups,style="quantile")
        test=cut(New_data$Valeur,breaks=zClass$brks,include.lowest=TRUE,labels=FALSE)
        } else if (Methodes[Sampling_method+1]=="K-means"){
        zClass <- classIntervals(New_data$Valeur, n=Number_groups,style="kmeans")
        test=cut(New_data$Valeur,breaks=zClass$brks,include.lowest=TRUE,labels=FALSE)
        }

        Samples=c()
        for (id_group in unique(test)){
            Points_in_group=which(test%in%id_group)
            Sampling=sample(c(1:length(Points_in_group)),size=Number_samples_in_each_group)
            if (length(Samples)!=0){
            Samples=c(Samples,Points_in_group[Sampling])
            } else {
            Samples=Points_in_group[Sampling]
            }
        }
        Echantillons=New_data[Samples,]
        Echantillons$Class=rep(unique(test),each=Number_samples_in_each_group)
        Output=st_as_sf(Echantillons)
    }



