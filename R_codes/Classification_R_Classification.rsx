##Precision Agriculture=group
##Raster_input=Raster
##Number_classes=number 1
##Classification_method=selection equal;quantile;jenks;kmeans  
##Output=output raster


library (raster)
library(classInt)

Methodes=c("equal","quantile","jenks","kmeans")

zClass <- classIntervals(values(Raster_input), n=Number_classes,style=Methodes[Classification_method+1])
Output=cut(Raster_input,breaks=zClass$brks)