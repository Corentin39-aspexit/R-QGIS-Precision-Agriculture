##Precision Agriculture=group
##Vector_input=Vector
##Contour=Vector
##Variable=Field Vector_input
##IDW_smoothing=selection oui;non
##Size_grid_interpolation=number 1.5
##IDW_power=optional number 2
##Dist_border=number 5
##Final_zoning=output vector

library(tools)
library(gstat) 
library(sp)
library(abind)
library(ggplot2)
library(plyr)
library(maptools)
library(robustbase)
library(deldir)
library(rgeos)
library(rgdal)
library(moments)
library(Hmisc)
library(raster)
library(doParallel)
library(progress)
library(maps)


Contour=as(Contour,Class="Spatial")
Field_raw_0=as(Vector_input,Class="Spatial")

Name_coords=colnames(coordinates(Field_raw_0))
x1=coordinates(Field_raw_0)[,1]
x2=coordinates(Field_raw_0)[,2]
Field_raw_0=data.frame(Field_raw_0)
colnames(Field_raw_0)[which(colnames(Field_raw_0)%in%Name_coords)]=c("coords.x1","coords.x2")

Num_colonne=which(colnames(Field_raw_0)%in%Variable)
colnames(Field_raw_0)[Num_colonne]=Variable

set.seed(30)

Field_raw_0=data.frame(Field_raw_0)

coordinates(Field_raw_0)=~coords.x1+coords.x2
Field_raw_0=remove.duplicates(Field_raw_0, zero = 0.0)
Field_raw_0=data.frame(Field_raw_0)


Missing_values=which(is.na(Field_raw_0[[Variable]])==TRUE)
if (length(Missing_values)!=0){
  Field_raw_0=Field_raw_0[-Missing_values,]
}

Field=subset(Field_raw_0,select=c("coords.x1","coords.x2",Variable))
colnames(Field)=c("Longitude","Latitude","Yield")
Attribute="Yield"

# Interpolation on grid if needed
Field_init=Field
if (nrow(Field)!=0 & IDW_smoothing==0){
  
  Grille_interpolation=spsample(Contour,n=gArea(Contour)/(Size_grid_interpolation^2),type="regular")
  gridded(Grille_interpolation)=TRUE
  coordinates(Field)=~Longitude+Latitude
  Interpolated_values=idw0(Yield~1, data=Field, newdata=Grille_interpolation,idp=IDW_power)
  
  Field=data.frame(Longitude=coordinates(Grille_interpolation)[,1],
                   Latitude=coordinates(Grille_interpolation)[,2],
                   Yield=Interpolated_values[,1])
}

# Creation of voronoi tesselation
voronoipolygons = function(layer) {
  z = deldir(layer$Longitude, layer$Latitude,eps=1e-8)
  w = tile.list(z)
  polys = vector(mode = 'list', length =
                   length(w))
  Neighbours_voronoi = list()
  
  for (i in seq(along = polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1, ])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID =
                            as.character(i))
    Neighbours_1 = which(z$dirsgs$ind1 %in% i)
    Neighbours_2 = which(z$dirsgs$ind2 %in% i)
    Neighbours_voronoi[[i]] = c(z$dirsgs$ind2[Neighbours_1], z$dirsgs$ind1[Neighbours_2])
  }
  
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

Polys_voronoi = voronoipolygons(Field)
Polys_voronoi[[1]]=Polys_voronoi[[1]][1:nrow(Field),]
Polys_voronoi[[2]]=Polys_voronoi[[2]][1:nrow(Field)]
coordinates(Field)=~Longitude+Latitude

Over_Field_voronoi=over(Field,Polys_voronoi[[1]],byid=TRUE)
Field$Id_voronoi=Over_Field_voronoi[,1]
Field=data.frame(Field)
Field=arrange(Field,Field$Id_voronoi)

# Data aggregation over the tesselation
Field=aggregate(subset(Field,select=c(Yield,Longitude,Latitude)),by=list(Field$Id_voronoi),FUN="mean")
Field=subset(Field,select=-c(Group.1))

proj4string(Polys_voronoi[[1]])=proj4string(Contour)

# Small corrections of neighbourhood relationships
Poly_intersect = gIntersection(Polys_voronoi[[1]], Contour, byid =TRUE)
Neighbours_poly = gTouches(Poly_intersect, byid =TRUE)
for (j in 1:nrow(Field)) {
  True_neighbours = which(Neighbours_poly[j, ] == TRUE)
  Out_neighbours=which(True_neighbours>nrow(Field)) 
  if (length(Out_neighbours)>0){
    True_neighbours=True_neighbours[-Out_neighbours]
  }
  Polys_voronoi[[2]][j][[1]] = as.numeric(True_neighbours)
}


# Robust variance calculations in the neighbourhoods
Field$Loc_var_voronoi_first_neighbours =c()
for (i in 1:nrow(Field)) {
  Robust_location = median(c(Field[[Attribute]][i], Field[[Attribute]][Polys_voronoi[[2]][i][[1]]]))
  Field$Loc_var_voronoi_first_neighbours[i] = median(sapply(c(Field[[Attribute]][i], Field[[Attribute]][Polys_voronoi[[2]][i][[1]]]), function(i) {abs(i - Robust_location)}))
}


Field$Loc_var_voronoi_second_neighbours = c()
Field$Num_second_neighbours = c()
Polys_voronoi[[3]] = list()
for (i in 1:nrow(Field)) {
  Neighbours_first_order = Polys_voronoi[[2]][i][[1]]
  Neighbours_second_order = c()
  for (j in Neighbours_first_order) {
    Neighbours_second_order = c(Neighbours_second_order, Polys_voronoi[[2]][j][[1]])
  }
  
  Neighbours_second_order = unique(Neighbours_second_order)
  
  Polys_voronoi[[3]][[i]] = Neighbours_second_order
  
  Robust_location = median(Field[[Attribute]][Neighbours_second_order])
  Field$Loc_var_voronoi_second_neighbours[i] =median(sapply(Field[[Attribute]][Neighbours_second_order], function(i) {abs(i - Robust_location)}))
  Field$Num_second_neighbours[i] = length(Neighbours_second_order)
  Field$Num_first_neighbours[i] = length(Neighbours_first_order)
}

Field$TEST = Field$Loc_var_voronoi_second_neighbours


Field$Loc_var_voronoi_second_neighbours = c()
Field$Num_second_neighbours = c()
Polys_voronoi[[3]] = list()
for (i in 1:nrow(Field)) {
  Neighbours_first_order = Polys_voronoi[[2]][i][[1]]
  Neighbours_second_order = c()
  for (j in Neighbours_first_order) {
    Neighbours_second_order = c(Neighbours_second_order, Polys_voronoi[[2]][j][[1]])
  }
  
  Neighbours_second_order = unique(Neighbours_second_order)
  
  Polys_voronoi[[3]][[i]] = Neighbours_second_order
  
  Robust_location = median(Field[["TEST"]][Neighbours_second_order])
  
  Field$Loc_var_voronoi_second_neighbours[i] = sd(Field[["TEST"]][c(i, Neighbours_second_order)])
  Field$Num_second_neighbours[i] = length(Neighbours_second_order)
  Field$Num_first_neighbours[i] = length(Neighbours_first_order)
}


# Seed generation for the region growing algorithm
Field_SP = Field
coordinates(Field_SP) =  ~ Longitude +Latitude
proj4string(Field_SP)=proj4string(Contour)
Dist_to_bound = gDistance(Field_SP, as(Contour, "SpatialLines"), byid = TRUE)
Dist_to_bound_1 = data.frame(DIST = as.vector(Dist_to_bound))

Border_points = which(Dist_to_bound_1 < Dist_border)

for (j in Border_points) {
  Neighbours = Polys_voronoi[[2]][j][[1]]
  if (j < 3) {
    vec = seq(j + 2, j + 7, by = 1)
    Vec_remove = which(Neighbours %in% vec)
    if (length(Vec_remove) != 0) {
      Polys_voronoi[[2]][j][[1]] = Neighbours[-Vec_remove]
    }
  } else if (j > (nrow(Field) - 3)) {
    vec = seq(j - 7, j - 2, by = 1)
    Vec_remove = which(Neighbours %in% vec)
    if (length(Vec_remove) != 0) {
      Polys_voronoi[[2]][j][[1]] = Neighbours[-Vec_remove]
    }
  } else {
    vec_1 = seq(j - 7, j - 2, by = 1)
    vec_2 = seq(j + 2, j + 7, by = 1)
    vec_total = c(vec_1, vec_2)
    Vec_remove = which(Neighbours %in% vec_total)
    if (length(Vec_remove) != 0) {
      Polys_voronoi[[2]][j][[1]] = Neighbours[-Vec_remove]
    }
  }
}

Seed_points = c()
Unseed_points_1 = c()
Neighbours_remaining = c()


Field$TOT = Field$TEST + Field$Loc_var_voronoi_second_neighbours

Observation_not_checked = which(c(1:nrow(Field)) %nin% Border_points)
New_seed = Observation_not_checked[which(Field$TEST[Observation_not_checked] ==min(Field$TEST[Observation_not_checked]))][1]

kernel_dist = density(Field$Loc_var_voronoi_second_neighbours, kernel ="gaussian")


Noise = median(Field$Loc_var_voronoi_second_neighbours)
Vec_noise = Noise   

Seed_points = c(Seed_points, New_seed) 
Neighbours_remaining = c(Neighbours_remaining, New_seed)

Unseed_points_1 = c(Unseed_points_1, New_seed, Border_points)

STOP = "NON"
SEED_final = data.frame()
NBR_neighbours = 1

while (length(Unseed_points_1) < nrow(Field) & STOP == "NON") {
  Unseed_points_2 = c(New_seed)
  Unseed_points_3 = c(New_seed)
  
  Nbr_Correlated_neighbours = c()
  while (length(Neighbours_remaining) !=0) {
    Obs_to_add = c()
    
    for (i in 1:length(Neighbours_remaining)) {
      
      Neighbours = Polys_voronoi[[2]][Neighbours_remaining[i]][[1]]
      Neighbours_total = Neighbours
      
      
      Not_seeds = which(Neighbours_total %in% Unseed_points_2 |Neighbours_total %in% Unseed_points_3)
      if (length(Not_seeds) != 0) {
        Neighbours_total = Neighbours_total[-Not_seeds]
      }
      Neighbours_total = unique(Neighbours_total)
      
      
      if (length(Neighbours_total) != 0) {
        
        Sup_var = which(Field$TEST[Neighbours_total] >Field$TEST[Neighbours_remaining[i]])
        
        if (length(Sup_var) != 0) {
          Inf_neighbours = Neighbours_total[-Sup_var]
          Sup_neighbours = Neighbours_total[Sup_var]
          if (length(Inf_neighbours) !=0) {
            
            Noisy_obs = which(Field$TEST[Inf_neighbours] < (Field$TEST[Neighbours_remaining[i]] + Noise) & Field$TEST[Inf_neighbours] > (Field$TEST[Neighbours_remaining[i]] - Noise))
            
            
            if (length(Noisy_obs) != 0) {
              Sup_neighbours = c(Sup_neighbours, Inf_neighbours[Noisy_obs])
              Inf_neighbours = Inf_neighbours[-Noisy_obs]
            }
          }
          if (length(Sup_neighbours) > NBR_neighbours) {
            
            Obs_to_add = c(Obs_to_add, Sup_neighbours)
          }
        }
      }
      
      
      
      
    }
    
    Neighbours_remaining = c(Obs_to_add)
    Neighbours_remaining = unique(Neighbours_remaining)
    
    Unseed_points_3 = c(Unseed_points_3, Neighbours_remaining)
    Unseed_points_3 = unique(Unseed_points_3)
    
    Unseed_points_1 = c(Unseed_points_1, Neighbours_remaining)
    Unseed_points_1 = unique(Unseed_points_1)
    Unseed_points_2 = c(Unseed_points_2, Neighbours_remaining)
    Unseed_points_2 = unique(Unseed_points_2)
  }
  
  Nbr_Correlated_neighbours = c(Nbr_Correlated_neighbours, length(Unseed_points_3))
  SEED_final = rbind(SEED_final,data.frame(Obs = New_seed,
                                           Loc_variance =Field$TEST[New_seed],
                                           Nbr_corr_neighb = sum(Nbr_Correlated_neighbours))
  )
  
  Observation_not_checked = which(c(1:nrow(Field) %nin% Unseed_points_1 & c(1:nrow(Field) %nin% Seed_points)))
  if (length(Observation_not_checked) !=0) {
    
    Obs_Lowest_local_variance = which.min(Field$TEST[Observation_not_checked])
    New_seed = Observation_not_checked[Obs_Lowest_local_variance][1]
    Vec_noise = c(Vec_noise, Noise)
    Seed_points = c(Seed_points, New_seed)
    Neighbours_remaining = c(New_seed)
  } else {
    STOP = "OUI"
  }
}


New_obs = c()
for (k in SEED_final$Obs) {
  Neighbours_first_order = Polys_voronoi[[2]][k][[1]]
  Neighbours_second_order = c()
  for (j in Neighbours_first_order) {
    Neighbours_second_order = c(Neighbours_second_order, Polys_voronoi[[2]][j][[1]])
  }
  Neighbours_second_order = unique(Neighbours_second_order)
  
  
  Neigh_bound = which(Neighbours_second_order %in% Border_points)
  if (length(Neigh_bound) != 0) {
    Neighbours_second_order = Neighbours_second_order[-Neigh_bound]
  }
  
  Vec_diff = abs(Field$Yield[Neighbours_second_order] - median(Field$Yield[Neighbours_second_order]))
  New_obs = c(New_obs, Neighbours_second_order[which.min(Vec_diff)[1]])
}

Vec_neighbours = SEED_final$Nbr_corr_neighb

SEED_final = data.frame(Obs = New_obs,
                        Loc_variance = Field$Loc_var_voronoi_second_neighbours[New_obs],
                        Nbr_corr_neighb = Vec_neighbours
)


Seeds_dup = SEED_final$Obs[which(duplicated(SEED_final$Obs) == TRUE)]
if (length(Seeds_dup) != 0) {
  for (i in 1:length(Seeds_dup)) {
    Lines_seeds = which(SEED_final$Obs == Seeds_dup[i])
    Low_neighbours = which(SEED_final$Nbr_corr_neighb[Lines_seeds] == min(SEED_final$Nbr_corr_neighb[Lines_seeds]))
    if (length(Low_neighbours) > 1) {
      Line_final = Lines_seeds[1]
    } else {
      Line_final = Lines_seeds[Low_neighbours]
    }
    SEED_final = SEED_final[-Line_final, ]
  }
}

if (nrow(SEED_final)<=1){ # if no seed, then no zones
  jdata=Contour
  jdata@data=data.frame(DN=1)
  colnames(jdata@data)="DN"
  jdata@data$Aire=gArea(Contour)/10000
  jdata@data$Moyenne=mean(Field$Yield)
  jdata@data$CV=sd(Field$Yield)/mean(Field$Yield)
  Zoning_opportunity=0
  Number_zones=1
} else if (nrow(SEED_final)>1){# if seed, then zones are created
  # Expansion of zones with region growing algorithm
  Field$Yield_zone = Field$Yield
  
  ZONE_df_Zone = c(1:nrow(SEED_final))
  ZONE_df_Yield = Field$Yield[SEED_final$Obs]
  
  Points_labelled_Label = SEED_final$Obs
  Points_labelled_Zone = c(1:nrow(SEED_final))
  
  INIT=1
  Row_field = nrow(Field)
  Vec_label_total = c()
  Vec_neighbours_total = c()
  Vec_dist_total = c()
  Vec_label_2_total = c()
  
  STOP = "NON"
  
  while (length(Points_labelled_Label) < Row_field & STOP == "NON") {
    
    Vec_dist = c() 
    Vec_label = c()  
    Vec_neighbours = c()  
    
    for (i in Points_labelled_Label[INIT:length(Points_labelled_Label)]) {
      Vec_neighbours = c(Vec_neighbours, Polys_voronoi[[2]][[i]])
      Vec_label = c(Vec_label, rep(i, length(Polys_voronoi[[2]][[i]])))
      Vec_dist = c(Vec_dist, rep(NA, length(Polys_voronoi[[2]][[i]])))
    }
    
    INIT = length(Points_labelled_Label)
    
    
    Vec_label_total = c(Vec_label_total, Vec_label)
    Vec_neighbours_total = c(Vec_neighbours_total, Vec_neighbours)
    Vec_dist_total = c(Vec_dist_total, Vec_dist)
    
    Obs_labelled = which(Vec_neighbours_total %in% Points_labelled_Label)
    if (length(Obs_labelled) != 0) {
      Vec_label_total = Vec_label_total[-Obs_labelled]
      Vec_neighbours_total = Vec_neighbours_total[-Obs_labelled]
      Vec_dist_total = Vec_dist_total[-Obs_labelled]
    }
    
    Vec_label_2_total = Vec_label_total
    
    NA_dist = which(is.na(Vec_dist_total) ==TRUE)
    if (length(NA_dist) != 0) {
      Vec_dist_total[NA_dist] = abs(Field$Yield_zone[Vec_label_total[NA_dist]] - Field$Yield[Vec_neighbours_total[NA_dist]])
    }
    
    
    Closest_relation = which(Vec_dist_total == min(Vec_dist_total))
    
    if (length(Closest_relation) != 0) {
      for (k in 1:length(Closest_relation)) {
        Point_to_check = Vec_neighbours_total[Closest_relation[k]]
        Neighbours_point_to_check = Polys_voronoi[[2]][[Point_to_check]]
        Labelled_neighbours = which(Points_labelled_Label %in%
                                      Neighbours_point_to_check)
        temp <- table(as.vector(Points_labelled_Zone[Labelled_neighbours]))
        
        Num_zone = as.numeric(names(temp)[which(temp > (3 * temp[which(names(temp) == Points_labelled_Zone[which(Points_labelled_Label == Vec_label_total[Closest_relation[k]])])]))])
        if (length(Num_zone) == 0) {
          Num_zone = Points_labelled_Zone[which(Points_labelled_Label == Vec_label_total[Closest_relation[k]])]
        }
        
        if (length(Labelled_neighbours) == 0) {
          
        } else if (Points_labelled_Zone[which(Points_labelled_Label == Vec_label_total[Closest_relation[k]])] %in% Num_zone) {
          
          
        } else {
          
          Diff_yield = abs(ZONE_df_Yield[Num_zone] - Field[Point_to_check, "Yield"])
          Futur_zone = which(Diff_yield == min(Diff_yield))
          Vec_label_2_total[Closest_relation[k]] = Points_labelled_Label[which(Points_labelled_Zone == Num_zone[Futur_zone])[1]]
        }
      }
      
      if (length(Closest_relation) == 1) {
        Points_labelled_Label[INIT + 1] = Vec_neighbours_total[Closest_relation]
        Points_labelled_Zone[INIT + 1] = Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Closest_relation])]
        
        ZONE_df_Yield[Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Closest_relation])]] = median(Field$Yield[Points_labelled_Label[which(Points_labelled_Zone %in% Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Closest_relation])])]])
        Num_line = which(Points_labelled_Zone %in% Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Closest_relation])])
        Field$Yield_zone[Points_labelled_Label[Num_line]] = ZONE_df_Yield[Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Closest_relation])]]
        
      } else {
        Obs_inter = Closest_relation
        while (length(Obs_inter) != 0) {
          Obs_labelled = which(Vec_neighbours_total[Obs_inter] %in% Points_labelled_Label)
          if (length(Obs_labelled) != 0) {
            Obs_inter = Obs_inter[-Obs_labelled]
          }
          if (length(Obs_inter) != 0) {
            
            
            Points_labelled_Label[length(Points_labelled_Label) + 1] = Vec_neighbours_total[Obs_inter[1]]
            Points_labelled_Zone[length(Points_labelled_Zone) + 1] = Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Obs_inter[1]])]
            
            
            ZONE_df_Yield[Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Obs_inter[1]])]] = median(Field$Yield[Points_labelled_Label[which(Points_labelled_Zone %in%
                                                                                                                                                                    Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Obs_inter[1]])])]])
            Num_line = which(Points_labelled_Zone %in% Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Obs_inter[1]])])
            Field$Yield_zone[Points_labelled_Label[Num_line]] = ZONE_df_Yield[Points_labelled_Zone[which(Points_labelled_Label == Vec_label_2_total[Obs_inter[1]])]]
            
            Obs_inter = Obs_inter[-c(1)]
            
          }
          
        }
      }
    } else {
      STOP = "OUI"
    }
  }
  
# Reconstruction of spatial polygon data frame
  if (length(Points_labelled_Label) != nrow(Field)) {
    Points_to_remove = which(c(1:Row_field) %nin% Points_labelled_Label)
    Field = Field[-Points_to_remove, ]
  }
  
  
  Points_labelled = data.frame(Label = Points_labelled_Label,
                               Zone = Points_labelled_Zone)
  
  
  Points_labelled = arrange(Points_labelled, Points_labelled$Label)
  Field$ZONE = Points_labelled$Zone
  Field$ZONE = as.factor(Field$ZONE)
  
  Zones_clipped = list()
  for (k in 1:length(Polys_voronoi[[1]])) {
    
    if (gContains(Contour, Polys_voronoi[[1]][k, ]) == FALSE) {
      
      Zones_clipped[[k]] = SpatialPolygonsDataFrame( gIntersection(Contour, Polys_voronoi[[1]][k, ]), data = data.frame(x = Polys_voronoi[[1]][k, ]@data$x, 
                                                                                                                        y = Polys_voronoi[[1]][k, ]@data$y)
      )
      Zones_clipped[[k]] = spChFIDs(Zones_clipped[[k]], slot(slot(Polys_voronoi[[1]][k, ], 'polygons')[[1]], "ID"))
    } else {
      Zones_clipped[[k]] = Polys_voronoi[[1]][k, ]
    }
  }
  
  Points_labelled = arrange(Points_labelled, Points_labelled$Zone)
  Zoning_V1 = list()
  for (i in unique(Points_labelled$Zone)) {
    Subset_zone = Points_labelled[which(Points_labelled$Zone == i), ]
    if (nrow(Subset_zone) > 1) {
      Zoning_V1[[i]] = Zones_clipped[[Subset_zone$Label[1]]]
      for (j in 2:nrow(Subset_zone)) {
        Zoning_V1[[i]] = gUnion(Zoning_V1[[i]], Zones_clipped[[Subset_zone$Label[j]]], id = as.character(i))
      }
    } else {
      Zoning_V1[[i]] = Zones_clipped[[Subset_zone$Label]]
      Zoning_V1[[i]] = spChFIDs(Zoning_V1[[i]], as.character(i))
    }
  }
  
  Zoning_V1 <- Zoning_V1[!sapply(Zoning_V1, is.null)]
  
  joined = SpatialPolygons(lapply(Zoning_V1, function(x) { x@polygons[[1]]}))
  
  Area_zone = c()
  for (i in 1:length(Zoning_V1)) {
    Area_zone = c(Area_zone, gArea(Zoning_V1[[i]]) / 10000)
  }
  
  jdata = SpatialPolygonsDataFrame( Sr = joined, data = data.frame(Num_zone = c(1:length(ZONE_df_Zone)), 
                                                                   area = Area_zone, 
                                                                   Yield =ZONE_df_Yield), 
                                    FALSE)
  
  
  jdata <- gBuffer(jdata, byid = TRUE, width = 0)
  colnames(jdata@data)=c("DN","Area","Mean_Value")
}

  Final_zoning=st_as_sf(jdata)


