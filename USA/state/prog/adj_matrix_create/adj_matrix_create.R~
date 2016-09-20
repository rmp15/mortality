rm(list=ls())

# load maptools and USA map
library(maptools)
library(RColorBrewer)
getinfo.shape("../../data/shapefiles/states")
USA.gen <- readShapePoly("../../data/shapefiles/states")
#plot(USA.gen)

# extract data from shapefile
shapefile.data <- attr(USA.gen, 'data')
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert back into shapefile
attr(USA.gen,'data') <- shapefile.data

# create lookup for fips and DRAWSEQ
drawseq.lookup <- as.data.frame(cbind(DRAWSEQ=shapefile.data$DRAWSEQ,fips=shapefile.data$fips))

# load rgdal and spdep
#library(rgdal)
library(spdep)

# create adjacency matrix
USA.nb <- poly2nb(USA.gen, queen=1)
#plot(USA.nb,coordinates(USA.gen),add=1)

# make matrix compatible with INLA
library(INLA)

# create directory for output
file.loc <- '~/git/mortality/USA/state/output/adj_matrix_create/'
ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)

# save DRAWSEQ lookup
saveRDS(drawseq.lookup,paste0(file.loc,'drawseq.lookup.rds'))

nb2INLA(paste0(file.loc,"USA.graph"),USA.nb)
USA.adj <- paste0(file.loc,"USA.graph")


