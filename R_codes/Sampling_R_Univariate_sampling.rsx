##Precision Agriculture=group 
##Raster_input=raster
##Sampling_method=selection Random;Quantiles;K-means
##Number_groups=number 2
##Number_samples_in_each_group=number 3
##Output=output vector

library(classInt)

Methodes=c("Random","Quantiles","K-means")

New_data=as.data.frame(rasterToPoints(Raster_input))
colnames(New_data)=c("Longitude","Latitude","Value")
coordinates(New_data)=~Longitude+Latitude

if (Methodes[Sampling_method+1]=="Random"){
Samples=sample(c(1:nrow(New_data)),size=Number_samples_in_each_group)
Echantillons=New_data[Samples,]
Output=st_as_sf(Echantillons)
} else {
        if (Methodes[Sampling_method+1]=="Quantiles"){
        zClass <- classIntervals(New_data$Value, n=Number_groups,style="quantile")
        test=cut(New_data$Value,breaks=zClass$brks,include.lowest=TRUE,labels=FALSE)
        } else if (Methodes[Sampling_method+1]=="K-means"){
        zClass <- classIntervals(New_data$Value, n=Number_groups,style="kmeans")
        test=cut(New_data$Value,breaks=zClass$brks,include.lowest=TRUE,labels=FALSE)
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



