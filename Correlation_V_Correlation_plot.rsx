##Precision Agriculture=group 
##Vector_input=Vector
##Color_factor=selection oui;non
##Variable_color=optional Field Vector_input
##showplots

library(GGally)
library(ggplot2)

Vector_input=as(Vector_input,Class="Spatial")

Name_coords=c(colnames(coordinates(Vector_input)),"optional")
Vector_input=data.frame(Vector_input)

Column_to_remove=which(colnames(Vector_input)%in%Name_coords)
Vector_input=Vector_input[,-c(Column_to_remove)]

if (Color_factor==0){
    Column_factor=which(colnames(Vector_input)%in%Variable_color)
    Vector_input[[Variable_color]]=as.factor(Vector_input[[Variable_color]])
    ggpairs(Vector_input,
            columns=c(1:length(Vector_input))[-Column_factor],
            ggplot2::aes(colour=Vector_input[[Variable_color]]),
            lower = list(continuous = wrap("points", size = 0.1))
    )
} else {
    ggpairs(Vector_input,
            lower = list(continuous = wrap("points", size = 0.1))
    )
}