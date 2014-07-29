library(rgdal)
library(maptools)
library(rgeos)

setScale(1e+15)

#Load conf
source('rrasR.conf')

shapefiles <- Sys.glob(paste0(path, '*MUE250*.shp'))
filenames <- gsub('.shp', '', basename(shapefiles))

shps <- list()
for(f in filenames){

	# Need to give new IDs to each polygon, otherwise combining shapes isn't feasible
	
	shp <- readOGR(path, f, encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)
	shp <- spChFIDs(shp, as.character(paste0(f, '_ID_', rownames(as(shp, 'data.frame')))))

	# Filter unsidered holes, islands and other "smithereens"	
	tmp <- c()
	for(p in shp@polygons) tmp <- c(tmp, list(Polygons(Filter(function(x){x@ringDir==1 && length(x@coords) > 1000}, p@Polygons), ID=p@ID)))
    for(p in shp@polygons) tmp <- c(tmp, list(Polygons(Filter(function(x){x@ringDir==1}, p@Polygons), ID=p@ID)))

	shp <- SpatialPolygons(tmp)

	shps[f] <- shp

}

