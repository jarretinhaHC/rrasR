library(XLConnect)
library(rgdal)
library(maptools)
library(rgeos)

setScale(1e+12)

wb <- loadWorkbook('../ref_data/Regiões_SP.xls')
data <- readWorksheet(wb, 'Regiões', check.names=FALSE)
rras_list <- unique(data$'NOME_RRAS')

df <- aggregate(x=data[c('MASCULINO', 'FEMININO', 'TOTAL')], by=list(data$NOME_RRAS), FUN='sum')
rownames(df) <- df[, 1]
df[, 1] <- c()

shp <- readOGR('/home/jarretinha/dev/cnme/maps/sp/', '35MUE250GC_SIR', encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)

for(RRAS in rras_list){

	rras <- data[data$'NOME_RRAS' == RRAS, ]
	shp$ID[shp$'NM_MUNICIP' %in% rras$'MUNICÍPIO'] <- RRAS

}

shp <- unionSpatialPolygons(shp, shp$ID)

tmp <- c()
for(p in shp@polygons){

	tmp <- c(tmp, list(Polygons(Filter(function(x){x@ringDir==1 && length(x@coords) > 1000}, p@Polygons), ID=p@ID)))

}

shp <- SpatialPolygons(tmp)

# Useless line, here just to remember how to extract data using slots
IDs <- sapply(slot(shp, 'polygons'), function(x) slot(x, 'ID'))
df$clong <- coordinates(shp)[, 1]
df$clat <- coordinates(shp)[, 2]
spdf <- SpatialPolygonsDataFrame(shp, df)

writePolyShape(spdf, 'RRAS_spdf')

