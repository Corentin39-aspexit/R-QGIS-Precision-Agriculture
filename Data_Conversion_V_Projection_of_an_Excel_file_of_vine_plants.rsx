##Precision Agriculture=group
##Ceps_vigne_csv= table
##Intra_rang=number 1
##Inter_rang=number 2
##Coord_first_X=number 447795.75385
##Coord_first_Y=number 6430865.64331
##Coord_last_X=number 447889.85724
##Coord_last_Y=number 6430833.54448
##Coord_first_bord_X=number 447796.45165
##Coord_first_bord_Y=number 6430881.59303
##Output=vector

library(rgdal)

Excel_data.df=Ceps_vigne_csv

# Calcul orientation des rangs (dans le sens des rangs)
  delta_X = Coord_last_X - Coord_first_X
  delta_Y = Coord_last_Y - Coord_first_Y
  Orientation_rangs = 180 + atan2(-delta_Y, -delta_X) * 180 / pi
  
# Calcul orientation bords de rangs (perpendiculaires aux rangs)
  delta_X = Coord_first_bord_X - Coord_first_X
  delta_Y = Coord_first_bord_Y - Coord_first_Y
  Orientation_bords = 180 + atan2(-delta_Y, -delta_X) * 180 / pi
  

# Initialisation de matrice pour coordonnees en X et en Y
Coords_points_X=matrix(nrow = nrow(Excel_data.df), ncol = ncol(Excel_data.df))
Coords_points_Y=matrix(nrow = nrow(Excel_data.df), ncol = ncol(Excel_data.df))

INIT_X=Coord_first_X
INIT_Y=Coord_first_Y

# En fonction orientation des rangs, on calcule les coordonnees
if (Orientation_rangs>=0 & Orientation_rangs<90){
  
    for (i in 1:ncol((Excel_data.df))){
      for (j in 1:nrow(Excel_data.df)){
        Coords_points_X[j,i]=INIT_X+cos(Orientation_rangs*pi/180)*(j-1)*Intra_rang
        Coords_points_Y[j,i]=INIT_Y+sin(Orientation_rangs*pi/180)*(j_1)*Intra_rang
      }
      
      if (Orientation_bords>=0 & Orientation_bords<90){
        INIT_X=INIT_X+cos(Orientation_bords*pi/180)*Inter_rang
        INIT_Y=INIT_Y+sin(Orientation_bords*pi/180)*Inter_rang
      } else if (Orientation_bords>=90 & Orientation_bords<180){
        INIT_X=INIT_X-cos((180-Orientation_bords)*pi/180)*Inter_rang
        INIT_Y=INIT_Y+sin((180-Orientation_bords)*pi/180)*Inter_rang
      } else if (Orientation_bords>=180 & Orientation_bords<270){
        INIT_X=INIT_X+cos((270-Orientation_bords)*pi/180)*Inter_rang
        INIT_Y=INIT_Y+sin((270-Orientation_bords)*pi/180)*Inter_rang
      } else if (Orientation_bords>=270 & Orientation_bords<360){
        INIT_X=INIT_X+cos((360-Orientation_bords)*pi/180)*Inter_rang
        INIT_Y=INIT_Y-sin((360-Orientation_bords)*pi/180)*Inter_rang
      }
  
    }
  
} else if (Orientation_rangs>=90 & Orientation_rangs<180){
  
  for (i in 1:ncol((Excel_data.df))){
    for (j in 1:nrow(Excel_data.df)){
      Coords_points_X[j,i]=INIT_X-cos((180-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
      Coords_points_Y[j,i]=INIT_Y+sin((180-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
    }
    
    if (Orientation_bords>=0 & Orientation_bords<90){
      INIT_X=INIT_X+cos(Orientation_bords*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin(Orientation_bords*pi/180)*Inter_rang
    } else if (Orientation_bords>=90 & Orientation_bords<180){
      INIT_X=INIT_X-cos((180-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((180-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=180 & Orientation_bords<270){
      INIT_X=INIT_X+cos((270-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((270-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=270 & Orientation_bords<360){
      INIT_X=INIT_X+cos((360-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y-sin((360-Orientation_bords)*pi/180)*Inter_rang
    }
    
  }
} else if (Orientation_rangs>=180 & Orientation_rangs<270){
  
  for (i in 1:ncol((Excel_data.df))){
    for (j in 1:nrow(Excel_data.df)){
      Coords_points_X[j,i]=INIT_X+cos((270-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
      Coords_points_Y[j,i]=INIT_Y+sin((270-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
    }
    
    if (Orientation_bords>=0 & Orientation_bords<90){
      INIT_X=INIT_X+cos(Orientation_bords*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin(Orientation_bords*pi/180)*Inter_rang
    } else if (Orientation_bords>=90 & Orientation_bords<180){
      INIT_X=INIT_X-cos((180-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((180-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=180 & Orientation_bords<270){
      INIT_X=INIT_X+cos((270-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((270-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=270 & Orientation_bords<360){
      INIT_X=INIT_X+cos((360-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y-sin((360-Orientation_bords)*pi/180)*Inter_rang
    }
    
  }
} else if (Orientation_rangs>=270 & Orientation_rangs<360){
  
  for (i in 1:ncol((Excel_data.df))){
    for (j in 1:nrow(Excel_data.df)){
      Coords_points_X[j,i]=INIT_X+cos((360-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
      Coords_points_Y[j,i]=INIT_Y-sin((360-Orientation_rangs)*pi/180)*(j-1)*Intra_rang
    }
    
    if (Orientation_bords>=0 & Orientation_bords<90){
      INIT_X=INIT_X+cos(Orientation_bords*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin(Orientation_bords*pi/180)*Inter_rang
    } else if (Orientation_bords>=90 & Orientation_bords<180){
      INIT_X=INIT_X-cos((180-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((180-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=180 & Orientation_bords<270){
      INIT_X=INIT_X+cos((270-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y+sin((270-Orientation_bords)*pi/180)*Inter_rang
    } else if (Orientation_bords>=270 & Orientation_bords<360){
      INIT_X=INIT_X+cos((360-Orientation_bords)*pi/180)*Inter_rang
      INIT_Y=INIT_Y-sin((360-Orientation_bords)*pi/180)*Inter_rang
    }
    
  }
}

# Initialisation de la matrice
Matrix_final=matrix(nrow = nrow(Excel_data.df)*ncol(Excel_data.df), ncol = 3)
count=1

# On remplit la matrice avec les coordonnees et les attributs
for (i in 1:ncol((Excel_data.df))){
  for (j in 1:nrow(Excel_data.df)){
    Matrix_final[count,1]=Coords_points_X[j,i]
    Matrix_final[count,2]=Coords_points_Y[j,i]
    Matrix_final[count,3]=Excel_data.df[j,i]
    count=count+1
    
  }
}

# Transformation en data frame
Ceps_finaux=data.frame(Matrix_final)
colnames(Ceps_finaux)=c("Longitude","Latitude","Type")

# Conversion des colonnes en numerique
Ceps_finaux$Longitude=as.numeric(as.character(Ceps_finaux$Longitude))
Ceps_finaux$Latitude=as.numeric(as.character(Ceps_finaux$Latitude))

# On supprime les trous dans les parcelles (points manquants ou non renseignes)
Trous=which(is.na(Ceps_finaux$Type)==TRUE)
if (length(Trous)!=0){
  Ceps_finaux=Ceps_finaux[-Trous,]
}

# Transformation en spatial point data frame
coordinates(Ceps_finaux)=~Longitude+Latitude
Output=st_as_sf(Ceps_finaux)








