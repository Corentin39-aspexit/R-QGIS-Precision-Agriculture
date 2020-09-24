##Precision Agriculture=group
##Vector_input=Vector
##Contour=Vector
##Variable=Field Vector_input
##Step=number 0.2
##Final_zoning=output vector

library(geozoning)
library(maptools)
library(rgeos)

Points=as(Vector_input,Class="Spatial")
Contour=as(Contour,Class="Spatial")
boundary=list(x=Contour@polygons[[1]]@Polygons[[1]]@coords[,1],
              y=Contour@polygons[[1]]@Polygons[[1]]@coords[,2])
Points@data$x=coordinates(Points)[,1]
Points@data$y=coordinates(Points)[,2]
Points@data=subset(Points@data,select=c("x","y",Variable))
Points=data.frame(Points)
Points=Points[,1:3]

# Interpolation of the data on a grid
map=genMap(DataObj=Points,disp=0,krig=2,boundary=boundary)

# Search for zoning
ro=loopQ3(map,step=Step,disp=0,QUIET=T)

# Extract best zoning
bqProb=ro[1,5:7]
criti=correctionTree(bqProb,map,SAVE=TRUE)
res=searchNODcrit1(bqProb,criti)
b=res$ind[[1]][1]
K=criti$zk[[2]][[b]]
bestZoning=K$zonePolygone


# Merge zones
joined = SpatialPolygons(lapply(bestZoning, function(x){x@polygons[[1]]}))
Data_.spdf=data.frame(Num_zone=1:length(joined))
row.names(Data_.spdf)=sapply(slot(joined, "polygons"), function(x) slot(x, "ID"))
jdata = SpatialPolygonsDataFrame(Sr=joined, data=Data_.spdf,FALSE)


# Descaling and Decentering because coordinates were scaled between 0 and 1
scale_factor=max(c(max(boundary$x)-min(boundary$x),max(boundary$y)-min(boundary$y)))
jdata.descale=elide(jdata,scale=scale_factor)
jdata.decenter=elide(jdata.descale,shift=c(min(boundary$x),min(boundary$y)))

Final_zoning=st_as_sf(jdata.decenter)