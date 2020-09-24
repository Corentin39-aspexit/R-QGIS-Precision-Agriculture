##Precision Agriculture=group
##showplots   
##Vector_input=Vector
##Graph_type=selection Point;Line;Boxplot;Histogram
##X_axis=Field Vector_input
##Y_axis=Field Vector_input
##Color_type=selection aucun;couleur;remplissage
##Colorisation=optional Field Vector_input
##Use_facet_1=selection oui;non
##Facet_1=optional Field Vector_input
##Use_facet_2=selection oui;non
##Facet_2=optional Field Vector_input
##Font_size_X_axis=optional number 12
##Font_size_Y_axis=optional number 12
##Font_size_X_title=optional number 14
##Font_size_Y_title=optional number 14
##Font_size_facet_1=optional number 14
##Font_size_facet_2=optional number 14

library(ggplot2)
library(Hmisc)

Taille_axe_X=Font_size_X_axis
Taille_axe_Y=Font_size_Y_axis
Taille_titre_X=Font_size_X_title
Taille_titre_Y=Font_size_Y_title
Taille_facet_1=Font_size_facet_1
Taille_facet_2=Font_size_facet_2



Vector_input=as(Vector_input,Class="Spatial")
Vector_input=data.frame(Vector_input)


PLOT=ggplot(data=Vector_input,aes(x=Vector_input[[X_axis]],y=Vector_input[[Y_axis]]))

if (Graph_type==0){
  if (Color_type==0){
    PLOT=PLOT+geom_point()
  } else if (Color_type==1){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_point(aes(col=Vector_input[[Colorisation]]))
  } else if (Color_type==2){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_point(aes(fill=Vector_input[[Colorisation]]))
  }
} else if (Graph_type==1){
  if (Color_type==0){
    PLOT=PLOT+geom_line()
  } else if (Color_type==1){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_line(aes(col=Vector_input[[Colorisation]],group=Vector_input[[Colorisation]]))+scale_color_discrete(name = Colorisation)
  } else if (Color_type==2){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_line(aes(fill=Vector_input[[Colorisation]],group=Vector_input[[Colorisation]]))+scale_fill_discrete(name = Colorisation)
  }
} else if (Graph_type==2){
  if (Color_type==0){
    PLOT=PLOT+geom_boxplot()
  } else if (Color_type==1){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_boxplot(aes(col=Vector_input[[Colorisation]]))+scale_color_discrete(name = Colorisation)
  } else if (Color_type==2){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_boxplot(aes(fill=Vector_input[[Colorisation]]))+scale_fill_discrete(name = Colorisation)
  }
} else if (Graph_type==2){
  if (Color_type==0){
    PLOT=PLOT+geom_histogram()
  } else if (Color_type==1){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_histogram(aes(col=Vector_input[[Colorisation]]))
  } else if (Color_type==2){
    Vector_input[[Colorisation]]=as.factor(Vector_input[[Colorisation]])
    PLOT=PLOT+geom_histogram(aes(fill=Vector_input[[Colorisation]]))
  }
}


if (Use_facet_1==0 & Use_facet_2==0){
PLOT=PLOT+facet_grid(Vector_input[[Facet_1]]~Vector_input[[Facet_2]])+theme(strip.text.y = element_text(size=Font_size_facet_1),strip.text.x = element_text(size=Font_size_facet_2))
} else if (Use_facet_1==0 & Use_facet_2==1){
  PLOT=PLOT+facet_grid(Vector_input[[Facet_1]]~.)+theme(strip.text.y = element_text(size=Font_size_facet_1))
} else if (Use_facet_1==1 & Use_facet_2==0){
  PLOT=PLOT+facet_grid(~Vector_input[[Facet_2]])+theme(strip.text.x = element_text(size=Font_size_facet_2))
}

PLOT=PLOT+theme_bw()+xlab(X_axis)+ylab(Y_axis)+theme(axis.text.x = element_text(size=Font_size_X_axis),axis.text.y = element_text(size=Font_size_Y_axis),axis.title.x = element_text(size=Font_size_X_title),axis.title.y = element_text(size=Font_size_Y_title))
PLOT