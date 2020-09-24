##Precision Agriculture=group
##Zoning_1=vector polygon
##Zoning_2=vector polygon
##Output=output vector

library(raster)
library(sp)

# Check if fid field
Fid_1=which(colnames(data.frame(Zoning_1))%in%"fid")
if (length(Fid_1)!=0){
Zoning_1$fid=NULL
}

Fid_2=which(colnames(data.frame(Zoning_2))%in%"fid")
if (length(Fid_2)!=0){
Zoning_2$fid=NULL
}

#Intersecting zonings
Intersection=st_intersection(Zoning_1,Zoning_2)

Zoning_1=as(Zoning_1,Class="Spatial")
Zoning_2=as(Zoning_2,Class="Spatial")

Intersection=as(Intersection,Class="Spatial")

# Recuperation des zones en dehors de intersection
Out_zones_1=erase(Zoning_1,Zoning_2)
Out_zones_1=disaggregate(Out_zones_1)
Out_zones_2=erase(Zoning_2,Zoning_1)
Out_zones_2=disaggregate(Out_zones_2)

# Aggregation de toutes les zones
Bind1=bind(Intersection,Out_zones_1)
Bind2=bind(Bind1,Out_zones_2)

Output=st_as_sf(Bind2)