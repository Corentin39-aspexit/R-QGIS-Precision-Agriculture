##Precision Agriculture=group
##Input_vector=vector
##Variable= Field Input_vector
##Shape=selection Triangular;Trapezoidal;Trapezoidal_inf;Trapezoidal_sup
##Rule_name=string "Class_name"
##Lower_support=number 11
##Lower_kernel=number 13
##Upper_kernel=number 14
##Upper_support=number 18
##Output=output vector

library(FisPro)

Input_vector=as(Input_vector,Class="Spatial")

# Construction de la fonction appartenance
if (Shape==0){
mf <- new(mf_triangular, Lower_support, Lower_kernel, Upper_support)
} else if (Shape==1){
mf <- new(mf_trapezoidal, Lower_support, Lower_kernel,Upper_kernel,Upper_support)
} else if (Shape==2){
mf <- new(mf_trapezoidal_inf, Upper_kernel, Upper_support)
} else if (Shape==3){
mf <- new(mf_trapezoidal_sup, Lower_support, Lower_kernel)
}

# Calcul degres appartenance
Fuzzy_values=c()
for (i in 1:nrow(Input_vector@data)){
  Fuzzy_values=c(Fuzzy_values,mf$degree(Input_vector[[Variable]][i]))
}

Input_vector@data[[paste0(Variable,"_Fuzzy_",Rule_name,sep="")]]=Fuzzy_values
Output=st_as_sf(Input_vector)














