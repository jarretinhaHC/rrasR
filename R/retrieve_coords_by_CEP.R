library(ggmap)

data <- read.delim('/home/jarretinha/dev/cnme/R/programas_SP.tsv', sep='\t', check.names=FALSE, as.is=TRUE)

tmp <- unique(data$CEP)
cep <- data.frame(CEP=tmp, lon=NA, lat=NA, row.names=tmp)

for(i in length(cep$CEP)){

	tmp <- geocode(paste('CEP ', cep$CEP[i], ', Brazil', sep=''))
	cep$lon <- tmp[1, 'lon']
	cep$lat <- tmp[1, 'lat']

}

data$LON <- NA
data$LAT <- NA

for(i in 1:dim(data)[1]){

	nu_cep <- data[i, 'NU_CEP']
	data[i, 'LON'] <- cep[nu_cep, 'lon']
	data[i, 'LAT'] <- cep[nu_cep, 'lat']

}
