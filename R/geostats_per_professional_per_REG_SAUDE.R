library(XLConnect)
library(rgdal)
library(maptools)
library(rgeos)
library(igraph)
library(scales)


ref_data_path <- '/data/Brazil/ref_data/'
RRAS_filename <- 'Regiões_SP.xls'
wb <- loadWorkbook(paste0(ref_data_path, RRAS_filename))
rras_data <- readWorksheet(wb, 'Regiões', check.names=FALSE)

CNES_path <- '/data/Brazil/CNES/'
CNES_filename <- 'cnes_sp_RRAS_uppercase.tsv'
data <- read.delim(paste0(CNES_path, CNES_filename), sep='\t', as.is=TRUE, check.names=FALSE)

data <-  merge(data, rras_data, by.x='MUNICIPIO', by.y='MUNICÍPIO', all.y)

tmp <- tapply(data$MUNICIPIO, data$CPF, list)

# Contracts per city per physician
tmp <- lapply(tmp, table)

# Cities per physician
tmp <- lapply(tmp, function(x) names(x))

# Physicians working in two or more cities
tmp <- Filter(function(x) length(x) > 1, tmp)

p4s <- '+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs' 
shapefiles_path <- '/data/Brazil/IBGE/malhas_digitais/municipio_2010/SE/SP/'
shp <- readOGR(shapefiles_path, '35MUE250GC_SIR', encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)

reg_shp <- readOGR(ref_data_path, '', encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)

# Need a projection to calculate areas and distances
shp <- spTransform(shp, CRS(p4s))

# Slot on the go! Calculate areas (metres) convert to km^2
shp$AREA <- unlist(lapply(shp@polygons, function(x) a <- x@area)) / 1e6

# Calculate city to city distances using centroid coords
dists <- spDists(coordinates(shp)) / 1000

# Minimum geodesic distance covered by each physician
dimnames(dists) <- list(shp$NM_MUNICIP, shp$NM_MUNICIP)

for(n in 1:length(tmp)){

	lst <- tmp[[n]]
	l <- length(lst)
	m <- m <- matrix(NA, nrow=l, ncol=l, dimnames=list(lst, lst))
	
	for(i in dimnames(m)[[1]]){
	
		for(j in dimnames(m)[[2]]){
		
			m[i, j] <- dists[i, j]
			
		}
		
	}
	
	g <- minimum.spanning.tree(graph.adjacency(m, weighted=TRUE, mode='undirected'))
	tmp[[n]] <- sum(E(g)$weight)
	
}

df <- data.frame(CPF=names(tmp), DIST=unlist(tmp))
plt <- ggplot(data=df, aes(DIST)) + geom_bar(aes(y=(..count..)/sum(..count..)), binwidth=10, colour='black', fill='white') + scale_y_continuous(labels=percent)


