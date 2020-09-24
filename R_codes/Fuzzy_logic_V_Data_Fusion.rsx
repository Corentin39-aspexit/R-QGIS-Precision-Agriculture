##Precision Agriculture=group
##Vector_input=vector
##Aggregation=selection WAM;OWA
##Geometry=output table

library(GeoFIS)

f <- fusion_class$new(mtcars)

# The code here relies on the mtcars dataset
# We want to aggregate 4 numerical fields (mpg, cyl, disp, qsec) after assigning a membership function to each of them

# First membership function. It is a trapezoidal superior membership function applied on the mpg field with a lower support of 15 and lower kernel of 30
mpg <- Node$new("mpg")
mpg$attribute <- "mpg"
mpg$mf <- new(mf_trapezoidal_sup, 15, 30)

# Second membership function. It is a trapezoidal inferior membership function applied on the cyl field with a upper kernel of 4 and upper support of 8
cyl <- Node$new("cyl")
cyl$attribute <- "cyl"
cyl$mf <- new(mf_trapezoidal_inf, 4, 8)

# Third membership function. It is a trapezoidal inferior membership function applied on the disp field with a upper kernel of 4 and upper support of 8
disp <- Node$new("disp")
disp$attribute <- "disp"
disp$mf <- new(mf_trapezoidal_inf, 150, 350)

# Fourth membership function. It is a trapezoidal inferior membership function applied on the qsec field with a lower support of 16, a lower kernel of 17, a upper kernel of 19 and upper support of 20
qsec <- Node$new("qsec")
qsec$attribute <- "qsec"
qsec$mf <- new(mf_trapezoidal, 16, 17, 19, 20)

# Build an aggregation node
aggregate <- Node$new("aggregation")

# Aggregation of the field with a weight of 0.25 for each of them. The sum of weights must be equal to 1.
if (Aggregation==0){
aggregate$aggreg <- new_aggreg_wam(c(0.25, 0.25, 0.25, 0.25))
} else {
aggregate$aggreg <- new_aggreg_owa(c(0.25, 0.25, 0.25, 0.25))
}
aggregate$AddChildNode(mpg)
aggregate$AddChildNode(cyl)
aggregate$AddChildNode(disp)
aggregate$AddChildNode(qsec)

f$aggregate <- aggregate
f$perform()

Geometry=f$output
