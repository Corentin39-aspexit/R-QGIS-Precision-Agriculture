##Precision Agriculture=group 
##Vector_input=Vector
##Correlation_method=selection pearson;kendall;spearman
##Variable_1=Field Vector_input
##Variable_2=Field Vector_input
##Correlation=output table

Vector_input=as(Vector_input,Class="Spatial")

Coords=coordinates(Vector_input)

Correlation.df=data.frame(Variable_1=Vector_input[[Variable_1]],
                          Variable_2=Vector_input[[Variable_2]])

if (Correlation_method==0){
    Corr_simple=cor(Correlation.df$Variable_1,Correlation.df$Variable_2,method="pearson")
} else if (Correlation_method==1){
    Corr_simple=cor(Correlation.df$Variable_1,Correlation.df$Variable_2,method="kendall")
} else if (Correlation_method==2){
    Corr_simple=cor(Correlation.df$Variable_1,Correlation.df$Variable_2,method="spearman")
}

Correlation=data.frame(Correlation=Corr_simple)