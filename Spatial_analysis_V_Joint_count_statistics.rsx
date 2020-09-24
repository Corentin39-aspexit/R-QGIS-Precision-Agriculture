##Precision Agriculture=group
##Vector_input=Vector
##Variable=Field Vector_input
##Distance_neighbourhood=number 10
##Method=selection Spatial_test;Monte_Carlo
##Output=output table

library(spdep)
library(rgeos)

Vector_input=as(Vector_input,Class="Spatial")
Vector_input$ID=c(1:nrow(Vector_input))
Vector_input[[Variable]]=as.factor(Vector_input[[Variable]])

# Construction voisinage
nlist <- dnearneigh(coordinates(Vector_input),0,Distance_neighbourhood)
W <- nb2listw(nlist, style="B", zero.policy=TRUE)

# Joint count statistic
if (Method==0){

Stats_join=joincount.test(Vector_input[[Variable]], W)
Stats_list=list()
for (i in c(1:length(Stats_join))){
  Stats_list[[i]]=c(levels(Vector_input[[Variable]])[i],Stats_join[[i]]$estimate[1],Stats_join[[i]]$statistic,Stats_join[[i]]$p.value)
}

Stack_stats=data.frame(do.call(rbind,Stats_list))
colnames(Stack_stats)=c("factor","join_count_stats","standard_deviate","p_value")

} else {
Stats_join=joincount.mc(Vector_input[[Variable]], W,nsim=1000)

Stats_list=list()
for (i in c(1:length(Stats_join))){
  Stats_list[[i]]=c(levels(Vector_input[[Variable]])[i],Stats_join[[i]]$statistic,Stats_join[[i]]$parameter,Stats_join[[i]]$p.value)
}

Stack_stats=data.frame(do.call(rbind,Stats_list))
colnames(Stack_stats)=c("factor","join_count_stats","observed_rank","p_value")
}

Output=Stack_stats