##Precision Agriculture=group 
##Raster_input=raster
##showplots
##Sampling_method=selection Conditioned Latin Hypercube Sampling
##Number_samples=number 5
##Output=output vector

library(clhs)

s <- rasterToPoints(Raster_input, spatial=TRUE)
s.clhs <- clhs(s, size = Number_samples, progress = FALSE, iter = 1000, simple = FALSE)

Echantillons=s[s.clhs$index_samples,]
Output=st_as_sf(Echantillons)

plot(s.clhs, mode=c("obj", "box"))