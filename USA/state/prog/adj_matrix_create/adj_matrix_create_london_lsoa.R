rm(list=ls())

# load maptools and USA map
library(maptools)
library(RColorBrewer)
getinfo.shape("../../data/shapefiles/LSOA11_London_clip")
USA.gen <- readShapePoly("../../data/shapefiles/LSOA11_London_clip")
#plot(USA.gen)

# remove Alaska and Hawaii
USA.gen  <- USA.gen [!USA.gen $STATE_FIPS %in% c("02", "15"),]

# extract data from shapefile
shapefile.data <- attr(USA.gen, 'data')
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert back into shapefile
attr(USA.gen,'data') <- shapefile.data

# create lookup for fips and DRAWSEQ
drawseq.lookup <- as.data.frame(cbind(DRAWSEQ=shapefile.data$DRAWSEQ,fips=shapefile.data$fips))

# convert DRAWSEQ row to go from 1-49 for INLA to work
drawseq.lookup$DRAWSEQ = 1:nrow(drawseq.lookup)

# load spdep
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
# saveRDS(drawseq.lookup,paste0(file.loc,'drawseq.lookup.contig.rds'))

nb2INLA(paste0(file.loc,"USA.graph.contig"),USA.nb)
USA.adj <- paste0(file.loc,"USA.graph.contig")


