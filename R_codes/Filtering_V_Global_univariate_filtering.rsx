##Precision Agriculture=group 
##Vector_input=Vector
##Variable=Field Vector_input
##Filtering_method=selection Normal;Tukey;Skewed
##Outliers_handling=selection Supprimer;Conserver
##Output=output vector

library(robustbase)

Vector_input=as(Vector_input,Class="Spatial")

if (Filtering_method==1){
whisker_1=as.numeric(summary(Vector_input[[Variable]])[2])-1.5*(as.numeric(summary(Vector_input[[Variable]])[5])-as.numeric(summary(Vector_input[[Variable]])[2]))
whisker_2=as.numeric(summary(Vector_input[[Variable]])[5])+1.5*(as.numeric(summary(Vector_input[[Variable]])[5])-as.numeric(summary(Vector_input[[Variable]])[2]))
Outliers=which(Vector_input[[Variable]]>whisker_2 | Vector_input[[Variable]]<whisker_1)
} else if (Filtering_method==0){
Outliers=which(Vector_input[[Variable]]>(mean(Vector_input[[Variable]])+3*sd(Vector_input[[Variable]])) | Vector_input[[Variable]]<(mean(Vector_input[[Variable]])-3*sd(Vector_input[[Variable]])))
} else if (Filtering_method==2){

  CUTOFF=function(Attrib){
    Robust_location=median(Vector_input[[Attrib]],na.rm=TRUE)
    Robust_scale=median(sapply(Vector_input[[Attrib]],function(i){abs(i-Robust_location)}),na.rm=TRUE)
    Medcouple=mc(Vector_input[[Attrib]],na.rm=TRUE)
    
    if (Medcouple>0){
      whisker_1=as.numeric(summary(Vector_input[[Attrib]])[2])-1.5*exp((-4)*mc(Vector_input[[Attrib]]))*(as.numeric(summary(Vector_input[[Attrib]])[5])-as.numeric(summary(Vector_input[[Attrib]])[2]))
      whisker_2=as.numeric(summary(Vector_input[[Attrib]])[5])+1.5*exp(3*mc(Vector_input[[Attrib]]))*(as.numeric(summary(Vector_input[[Attrib]])[5])-as.numeric(summary(Vector_input[[Attrib]])[2]))
    } else {
      whisker_1=as.numeric(summary(Vector_input[[Attrib]])[2])-1.5*exp((-3)*mc(Vector_input[[Attrib]]))*(as.numeric(summary(Vector_input[[Attrib]])[5])-as.numeric(summary(Vector_input[[Attrib]])[2]))
      whisker_2=as.numeric(summary(Vector_input[[Attrib]])[5])+1.5*exp(4*mc(Vector_input[[Attrib]]))*(as.numeric(summary(Vector_input[[Attrib]])[5])-as.numeric(summary(Vector_input[[Attrib]])[2]))
    }
    
    return(list(whisker_1,whisker_2))
  }
  
  Outliers=which(Vector_input[[Variable]]<CUTOFF(Variable)[[1]] | Vector_input[[Variable]]>CUTOFF(Variable)[[2]])
}


if (GOutliers_handling==0){
	if (length(Outliers)!=0){
	Vector_clean=Vector_input[-Outliers,]
    Output=st_as_sf(Vector_clean)
	}
} else {
Vector_clean=Vector_input
Vector_clean$Aberrants="NON"
Vector_clean$Aberrants[Outliers]="OUI"
Output=st_as_sf(Vector_clean)
}