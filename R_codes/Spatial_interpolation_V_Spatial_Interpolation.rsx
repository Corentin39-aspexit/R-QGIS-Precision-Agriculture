##Precision Agriculture=group
##Vector_input=Vector
##Contour=Vector
##Variable=Field Vector_input
##Method=selection IDW;Point-Kriging;Block-Kriging
##Size_grid_interpolation=number 5
##Dist_max_neighbours=optional number 50
##IDW_power=optional number 2
##Variogram_model=selection Linear;Spherical;Gaussian;Exponential
##Max_dist_vario=optional number 50
##Lag=optional number 5
##Range=optional number 50
##Nugget=optional number 0
##Sill=optional number 2
##Interpolation_map=output raster

library(gstat)
library(raster)

Vector_input=as(Vector_input,Class="Spatial")
Contour=as(Contour,Class="Spatial")

Name_coords=colnames(coordinates(Vector_input))
x1=coordinates(Vector_input)[,1]
x2=coordinates(Vector_input)[,2]
Vector_input=data.frame(Vector_input)
colnames(Vector_input)[which(colnames(Vector_input)%in%Name_coords)]=c("x1","x2")
coordinates(Vector_input)=~x1+x2
proj4string(Vector_input)=proj4string(Contour)


Grid_interpolation=spsample(Contour,size=floor(gArea(Contour)/(Size_grid_interpolation)^2),type="regular")
proj4string(Grid_interpolation)=proj4string(Contour)

if (Method==0){

Interpolation_output <- idw(formula = Vector_input[[Variable]] ~ 1,locations=Vector_input, newdata = Grid_interpolation,idp=IDW_power)

} else if (Method==1){

	Vario_vector=variogram(Vector_input[[Variable]]~x1+x2,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
	if (Variogram_model==0){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Lin", Range, Nugget))
	} else if (Variogram_model==1){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Sph", Range, Nugget))
	} else if (Variogram_model==2){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Gau", Range, Nugget))
	} else if (Variogram_model==3){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Exp", Range, Nugget))
	}
	
Interpolation_krige=krige(Vector_input[[Variable]]~x1+x2,Vector_input,newdata=Grid_interpolation,model=Vario_vector.model,maxdist=Dist_max_neighbours)
Interpolation_output=Interpolation_krige[,1]

} else if (Method==2){

	Vario_vector=variogram(Vector_input[[Variable]]~x1+x2,data=Vector_input,cutoff=Max_dist_vario,width=Lag)
	if (Variogram_model==0){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Lin", Range, Nugget))
	} else if (Variogram_model==1){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Sph", Range, Nugget))
	} else if (Variogram_model==2){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Gau", Range, Nugget))
	} else if (Variogram_model==3){
	Vario_vector.model = fit.variogram(Vario_vector, model = vgm((Sill-Nugget), "Exp", Range, Nugget))
	}

Interpolation_krige=krige(Vector_input[[Variable]]~x1+x2,Vector_input,newdata=Grid_interpolation,model=Vario_vector.model,block=c(Size_grid_interpolation,Size_grid_interpolation),maxdist=Dist_max_neighbours)
Interpolation_output=Interpolation_krige[,1]

}

gridded(Interpolation_output)=TRUE
Interpolation_map=raster(Interpolation_output)