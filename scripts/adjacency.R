library(raster)
library(rasterVis)

N <- 13500

V <- read.table("BrunelCaseA.con", sep = "\t", comment.char = "", quote = "", colClasses = "numeric", header = FALSE)

net <- sparseMatrix (i=V[,1], j=V[,2], x=V[,3], dims=c(N,N), index1 = FALSE)



