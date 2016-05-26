# USA shapefile test

# load maptools and USA map
library(maptools)
getinfo.shape("states")
USA.gen <- readShapePoly("states")
plot(USA.gen, border="grey60")

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
