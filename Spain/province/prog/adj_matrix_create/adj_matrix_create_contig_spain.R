rm(list=ls())

# load maptools and USA map
library(maptools)
library(RColorBrewer)
getinfo.shape("../../data/shapefiles/recintos_provinciales_inspire_peninbal_etrs89")
spain.gen <- readShapePoly("../../data/shapefiles/recintos_provinciales_inspire_peninbal_etrs89")
plot(spain.gen)

# remove island provinces (have asked Hicham)
#spain.gen  <- spain.gen [!spain.gen $STATE_FIPS %in% c("02", "15"),]

# CURRENTLY HERE!

# extract data from shapefile
shapefile.data <- attr(spain.gen, 'data')
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert back into shapefile
attr(spain.gen,'data') <- shapefile.data

# create lookup for fips and DRAWSEQ
drawseq.lookup <- as.data.frame(cbind(DRAWSEQ=shapefile.data$DRAWSEQ,fips=shapefile.data$fips))

# convert DRAWSEQ row to go from 1-49 for INLA to work
drawseq.lookup$DRAWSEQ = 1:nrow(drawseq.lookup)

# load spdep
library(spdep)

# create adjacency matrix
spain.nb <- poly2nb(spain.gen, queen=1)
#plot(spain.nb,coordinates(spain.gen),add=1)

# make matrix compatible with INLA
library(INLA)

# create directory for output
file.loc <- '~/git/mortality/spain/state/output/adj_matrix_create/'
ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)

# save DRAWSEQ lookup
saveRDS(drawseq.lookup,paste0(file.loc,'drawseq.lookup.contig.rds'))

nb2INLA(paste0(file.loc,"spain.graph.contig"),spain.nb)
spain.adj <- paste0(file.loc,"spain.graph.contig")


