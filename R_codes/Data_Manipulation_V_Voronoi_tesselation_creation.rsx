##Precision Agriculture=group
##Vector_input=vector
##Output_voronoi=output vector
##Output_neighbours=output table

library(rgeos)
library(deldir)
library(plyr)

Vector_input=as(Vector_input,Class="Spatial")

# Extraction longitude et latitude
Longitude=coordinates(Vector_input)[,1]
Latitude=coordinates(Vector_input)[,2]

Vector.df=data.frame(Vector_input)
Vector.df$Longitude=Longitude
Vector.df$Latitude=Latitude

# Construction des polygones de Voronoi
voronoipolygons = function(layer) {
      z = deldir(layer$Longitude, layer$Latitude,eps=1e-8)
      w = tile.list(z)
      polys = vector(mode = 'list', length =
                       length(w))
      Neighbours_voronoi = list()
      # Find neighbours of each voronoi polygons
      for (i in seq(along = polys)) {
        pcrds = cbind(w[[i]]$x, w[[i]]$y)
        pcrds = rbind(pcrds, pcrds[1, ])
        polys[[i]] = Polygons(list(Polygon(pcrds)), ID =
                                as.character(i))
        Neighbours_1 = which(z$dirsgs$ind1 %in% i)
        Neighbours_2 = which(z$dirsgs$ind2 %in% i)
        Neighbours_voronoi[[i]] = c(z$dirsgs$ind2[Neighbours_1], z$dirsgs$ind1[Neighbours_2])
      }
      # Convert to spatial polygons
      SP = SpatialPolygons(polys)
      voronoi = SpatialPolygonsDataFrame(SP,
                                         data = data.frame(
                                           id_poly=c(1:length(SP)),
                                           x = coordinates(SP)[,1],
                                           y = coordinates(SP)[,2],
                                           row.names = sapply(slot(SP, 'polygons'),
                                                              function(x)
                                                                slot(x, 'ID'))
                                         ))
      return(list(voronoi, Neighbours_voronoi))
      
      
    }
    
Polys_voronoi = voronoipolygons(Vector.df)
Output_voronoi=st_as_sf(Polys_voronoi[[1]])
Output_neighbours=plyr::ldply(Polys_voronoi[[2]], rbind)





