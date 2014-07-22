library(rgdal)
library(maptools)
library(rgeos)

setScale(1e+12)

path <- '/home/jarretinha/dev/cnme_data/IBGE/malhas_digitais/municipio_2010/all/'
shapefiles <- Sys.glob('/home/jarretinha/dev/cnme_data/IBGE/malhas_digitais/municipio_2010/all/*MUE250*.shp')
filenames <- gsub('.shp', '', basename(shapefiles))

shps <- list()
for(f in filenames){

	tmp <- readOGR(path, f, encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)
	for(i in 1:length(tmp@polygons)) tmp@polygons[[i]]@ID <- paste0(f, '_ID_', i)

	shps[f] <- tmp

}

