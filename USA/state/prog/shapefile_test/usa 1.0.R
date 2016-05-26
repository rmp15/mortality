# USA shapefile test

rm(list=ls())

# load maptools and USA map
library(maptools)
getinfo.shape("states")
USA.gen <- readShapePoly("states")
plot(USA.gen)

# extract data from shapefile
shapefile.data <- attr(USA.gen, 'data')
names(shapefile.data)[3] <- 'fips'

# create toy data
shapefile.data$color <- rnorm(nrow(shapefile.data))
attr(USA.gen, 'data') <- shapefile.data


library(rgdal)
library(spdep)

# create adjacency matrix
USA.nb <- poly2nb(USA.gen, queen=0)
plot(USA.nb,coordinates(USA.gen),add=1)



# make matrix compatible with INLA
library(INLA)

nb2INLA("USA.graph",USA.nb)
USA.adj <- "USA.graph"

# plot matrix
H <- inla.read.graph(filename=USA.adj)
image(inla.graph2matrix(H),xlab="",ylab="")
