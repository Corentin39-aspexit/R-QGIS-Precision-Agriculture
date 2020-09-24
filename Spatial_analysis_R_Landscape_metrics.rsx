##Precision Agriculture=group
##Raster_input=Raster
##Neighbourhood=selection Rook;Queen
##Indexes=output table
##Geometry=output table

library (raster)
library(landscapemetrics)


if (Neighbourhood==0){
    dir=4
} else {
dir=8
}


Landscape_metrics=data.frame(Shannon=lsm_l_shei(Raster_input)$value,
                             Richness=lsm_l_pr(Raster_input)$value,
                             Cohesion=lsm_l_cohesion(Raster_input, directions = dir)$value,
                             Division=lsm_l_division(Raster_input, directions = dir)$value,
                             Contagion=lsm_l_division(Raster_input)$value,
                             Interpo_juxtapo=lsm_l_iji(Raster_input)$value)
Indexes=Landscape_metrics

Perimetre=data.frame(lsm_p_perim(Raster_input,directions = 4))
colnames(Perimetre)[ncol(Perimetre)]="Perimetre"
Perimetre=Perimetre[,c(3,4,6)]
Surface=data.frame(lsm_p_shape(Raster_input,directions = 4))
Geometry=cbind(Perimetre,data.frame(Surface=Surface$value))