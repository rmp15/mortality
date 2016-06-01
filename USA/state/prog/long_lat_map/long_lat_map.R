library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, states_sp)

    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}

# read in file with global long lat
global.lon.lat <- read.csv('global_lon_lat.csv')

# adjust long coordinates by 180
global.lon.lat$lon <- global.lon.lat$lon - 180

# process global lon lat through function
lon.lat.state <- latlong2state(global.lon.lat[,c(2:3)])
lon.lat.state <- as.matrix(lon.lat.state)

# bind the lon lat data frame with the processed lon lat filter
bound <- cbind(lon.lat.state,global.lon.lat)

# omit all the irrelevant data points (i.e. not in country we are interested in)
bound.complete <- na.omit(bound)

# quick plot to check
plot(bound.complete$lon,bound.complete$lat)

# plot over ggplot map to double check
# TO DO
