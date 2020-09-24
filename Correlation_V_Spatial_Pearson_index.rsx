##Precision Agriculture=group 
##Vector_input=Vector
##Variable_1=Field Vector_input
##Variable_2=Field Vector_input
##Correlation=output table

library(SpatialPack)

Vector_input=as(Vector_input,Class="Spatial")

Coords=coordinates(Vector_input)

Correlation.df=data.frame(Variable_1=Vector_input[[Variable_1]],
                          Variable_2=Vector_input[[Variable_2]])


Corr_simple=cor(Correlation.df$Variable_1,Correlation.df$Variable_2,method="pearson")

Pearson_adjusted=modified.ttest(Correlation.df$Variable_1,Correlation.df$Variable_2,Coords)

Correlation=data.frame(Correlation=Corr_simple,
                       P_value_adjust=Pearson_adjusted$p.value,
                       F_statistic_adj=Pearson_adjusted$Fstat)