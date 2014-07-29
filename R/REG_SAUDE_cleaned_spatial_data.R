library(XLConnect)
library(rgdal)
library(maptools)
library(rgeos)

setScale(1e+15)

ref_data_path <- '/data/Brazil/ref_data/'
ref_data_path <- '/data/Brazil/ref_data/'
ref_data_filename <- 'Regiões_SP.xls'
wb <- loadWorkbook(paste0(ref_data_path, ref_data_filename))
data <- readWorksheet(wb, 'Regiões', check.names=FALSE)
reg_list <- unique(data$'NOME_REG_SAUDE')

df <- aggregate(x=data[c('MASCULINO', 'FEMININO', 'TOTAL')], by=list(data$'NOME_REG_SAUDE'), FUN='sum')
rownames(df) <- df[, 1]
df[, 1] <- c()

p4s <- '+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs' 
shapefiles_path <- '/data/Brazil/IBGE/malhas_digitais/municipio_2010/SE/SP/'
shp <- readOGR(shapefiles_path, '35MUE250GC_SIR', encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)

for(REG in reg_list){

	reg <- data[data$'NOME_REG_SAUDE' == REG, ]
	shp$ID[shp$'NM_MUNICIP' %in% reg$'MUNICÍPIO'] <- REG

}

shp <- unionSpatialPolygons(shp, shp$ID)
spdf <- SpatialPolygonsDataFrame(shp, df)
writePolyShape(spdf, paste0(ref_data_path, 'REG_SAUDE_raw_spdf'))

tmp <- c()
for(p in shp@polygons){

	tmp <- c(tmp, list(Polygons(Filter(function(x){x@ringDir==1 && length(x@coords) > 1000}, p@Polygons), ID=p@ID)))

}

shp <- SpatialPolygons(tmp)

# Useless line, here just to remember how to extract data using slots
spdf <- SpatialPolygonsDataFrame(shp, df)
writePolyShape(spdf, paste0(ref_data_path, 'REG_SAUDE_cleaned_spdf'))

