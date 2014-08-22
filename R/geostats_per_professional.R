library(XLConnect)
library(rgdal)
library(maptools)
library(rgeos)
library(igraph)
library(scales)
library(ggplot2)
library(naturalsort)
library(RColorBrewer)
library(reshape2)
library(plyr)

ref_data_path <- '/data/Brazil/ref_data/'
ref_filename <- 'Regiões_SP.xls'
wb <- loadWorkbook(paste0(ref_data_path, ref_filename))
ref_data <- readWorksheet(wb, 'Regiões', check.names=FALSE)

CNES_path <- '/data/Brazil/CNES/'
CNES_filename <- 'cnes_sp_RRAS_uppercase.tsv'
data <- read.delim(paste0(CNES_path, CNES_filename), sep='\t', as.is=TRUE, check.names=FALSE)

data <- merge(data, ref_data, by.x='MUNICIPIO', by.y='MUNICÍPIO', all=TRUE)

occupation <- 'MEDICO PATOLOGISTA'

data <- data[data$ESPECIALIDADE == occupation, ]

tmp <- list()
tmp$'MUNICÍPIO' <- tapply(data$MUNICIPIO, data$CPF, list)
tmp$'DRS' <- tapply(data$NOME_DRS, data$CPF, list)
tmp$'RRAS' <- tapply(data$NOME_RRAS, data$CPF, list)
tmp$'REGIÂO DE SAÚDE' <- tapply(data$NOME_REG_SAUDE, data$CPF, list)

# Contracts per city per physician
contracts_per_city <- lapply(tmp, function(lst) lapply(lst, table))

# Cities per physician
counts <- list()
counts <- lapply(contracts_per_city, function(lst) lapply(lst, function(x) length(names(x))))

df <- data.frame(COUNTS=NA, REG=NA)
for(s in names(counts))	df <- rbind(df, data.frame(COUNTS=unlist(counts[[s]]), REG=rep(s, length(counts[[s]]))))

df <- na.omit(df)
df$COUNTS <- cut(df$COUNTS, breaks=c(0:3, Inf), labels=c(1:3, '4+'))

# Percentual
plt <- ggplot(data=df, aes(COUNTS, fill=REG)) + geom_bar(aes(y= (..count..)/sum(..count..) * 4), position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais", breaks=seq(0, 1, 0.05), labels=percent) + scale_fill_brewer(name='Tipo de região', palette='Set3') + scale_x_discrete('Contagem') + ggtitle(paste(occupation, 'Regiões administrativas ocupadas\npor profissional', sep='\n'))
pdf(paste0('Ocupação - ', occupation, ' - percentual.pdf'), height=8, width=8)
print(plt)
dev.off()

# Counts
plt <- ggplot(data=df, aes(COUNTS, fill=REG)) + geom_bar(position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais") + scale_fill_brewer(name='Região administrativa', palette='Set3') + scale_x_discrete('Contagem') +  ggtitle(paste(occupation, 'Regiões administrativas ocupadas\npor profissional', sep='\n'))
pdf(paste0('Ocupação - ', occupation, ' - contagem.pdf'), height=8, width=8)
print(plt)
dev.off()

# REG

# Contracts per city per physician

df <- data.frame(COUNTS=NA, REG=NA)
for(reg in unique(data$NOME_REG_SAUDE)){

	tmp <- data[data$NOME_REG_SAUDE==reg, ]
	tmp <- tapply(tmp$MUNICIPIO, tmp$CPF, list)
	contracts_per_city <- lapply(tmp, table)
    cities_per_professional <- lapply(contracts_per_city, function(x) names(x))

	# Cities per physician
	counts <- list()
	counts <- lapply(contracts_per_city, function(x) length(names(x)))
	
	df <- rbind(df, data.frame(COUNTS=unlist(counts), REG=rep(reg, length(counts))))

}

df <- na.omit(df)
df$COUNTS <- cut(df$COUNTS, breaks=c(0:3, Inf), labels=c(1:3, '4+'))

# Percentual
df1 <- melt(ddply(df,.(REG),function(x){prop.table(table(x$COUNTS))}),id.vars = 1)

plt <- ggplot(data=df1, aes(x=variable, y=value, fill=REG)) + geom_bar(stat='identity', position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais", labels=percent) + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem') + ggtitle(paste(occupation, 'Municípios ocupados por profissional por Região de Saúde\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por Região de Saúde - ', occupation, ' - percentual.pdf'), height=18, width=18)
print(plt)
dev.off()

# Contagem

plt <- ggplot(data=df, aes(COUNTS, fill=REG)) + geom_bar(position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais") + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem', drop=FALSE) + ggtitle(paste(occupation, 'Municípios ocupados por profissional por Região de Saúde\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por Região de Saúde - ', occupation, '- contagem.pdf'), height=18, width=18)
print(plt)
dev.off()

# DRS

# Contracts per city per physician

df <- data.frame(COUNTS=NA, REG=NA)
for(drs in unique(data$NOME_DRS)){

	tmp <- data[data$NOME_DRS==drs, ]
	tmp <- tapply(tmp$MUNICIPIO, tmp$CPF, list)
	contracts_per_city <- lapply(tmp, table)
    cities_per_professional <- lapply(contracts_per_city, function(x) names(x))

	# Cities per physician
	counts <- list()
	counts <- lapply(contracts_per_city, function(x) length(names(x)))
	
	df <- rbind(df, data.frame(COUNTS=unlist(counts), REG=rep(drs, length(counts))))

}

df <- na.omit(df)
df$COUNTS <- cut(df$COUNTS, breaks=c(0:3, Inf), labels=c(1:3, '4+'))

# Percentual
df1 <- melt(ddply(df,.(REG),function(x){prop.table(table(x$COUNTS))}),id.vars = 1)

plt <- ggplot(data=df1, aes(x=variable, y=value, fill=REG)) + geom_bar(stat='identity', position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais", labels=percent) + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem') + ggtitle(paste(occupation, 'Municípios ocupados por profissional por DRS\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por DRS - ', occupation, ' - percentual.pdf'), height=12, width=12)
print(plt)
dev.off()

# Contagem

plt <- ggplot(data=df, aes(COUNTS, fill=REG)) + geom_bar(position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais") + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem', drop=FALSE) + ggtitle(paste(occupation, 'Municípios ocupados por profissional por DRS\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por DRS - ', occupation, ' - contagem.pdf'), height=18, width=18)
print(plt)
dev.off()

# RRAS

# Contracts per city per physician

df <- data.frame(COUNTS=NA, REG=NA)
for(rras in unique(data$NOME_RRAS)){

	tmp <- data[data$NOME_RRAS==rras, ]
	tmp <- tapply(tmp$MUNICIPIO, tmp$CPF, list)
	contracts_per_city <- lapply(tmp, table)
    cities_per_professional <- lapply(contracts_per_city, function(x) names(x))

	# Cities per physician
	counts <- list()
	counts <- lapply(contracts_per_city, function(x) length(names(x)))
	
	df <- rbind(df, data.frame(COUNTS=unlist(counts), REG=rep(rras, length(counts))))

}

df <- na.omit(df)
df$COUNTS <- cut(df$COUNTS, breaks=c(0:3, Inf), labels=c(1:3, '4+'))

# Percentual
df1 <- melt(ddply(df,.(REG),function(x){prop.table(table(x$COUNTS))}),id.vars = 1)

plt <- ggplot(data=df1, aes(x=variable, y=value, fill=REG)) + geom_bar(stat='identity', position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais", labels=percent) + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem') + ggtitle(paste(occupation, 'Municípios ocupados por profissional por RRAS\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por RRAS - ', occupation, ' - percentual.pdf'), height=12, width=12)
print(plt)
dev.off()

# Contagem

plt <- ggplot(data=df, aes(COUNTS, fill=REG)) + geom_bar(position='dodge', binwidth=1, colour='black') + scale_y_continuous("Profissionais") + scale_fill_manual(name='Tipo de região', values=rep(brewer.pal(7,"Set3"), times=10)) + scale_x_discrete('Contagem', drop=FALSE) + ggtitle(paste(occupation, 'Municípios ocupados por profissional por RRAS\n', sep='\n')) + facet_wrap(~REG, scales='free') + theme(legend.position='', strip.text.x = element_text(size = 8))
pdf(paste0('Ocupação por RRAS - ', occupation, ' - contagem.pdf'), height=18, width=18)
print(plt)
dev.off()

# Physicians working in two or more cities
p4s <- '+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs' 
shapefiles_path <- '/data/Brazil/IBGE/malhas_digitais/municipio_2010/SE/SP/'
shp <- readOGR(shapefiles_path, '35MUE250GC_SIR', encoding='ISO8859-1', use_iconv=TRUE, stringsAsFactors=FALSE)

# Need a projection to calculate areas and distances
shp <- spTransform(shp, CRS(p4s))

# Slot on the go! Calculate areas (metres) convert to km^2
shp$AREA <- unlist(lapply(shp@polygons, function(x) a <- x@area)) / 1e6

# Calculate city to city distances using centroid coords
dists <- spDists(coordinates(shp)) / 1000

# Minimum geodesic distance covered by each physician
dimnames(dists) <- list(shp$NM_MUNICIP, shp$NM_MUNICIP)

tmp <- tapply(data$MUNICIPIO, data$CPF, list)
contracts_per_city <- lapply(tmp, table)
cities_per_professional <- lapply(contracts_per_city, function(x) names(x))
distances_per_professional <- Filter(function(x) length(x) > 1, cities_per_professional)
for(n in 1:length(distances_per_professional)){

	lst <- distances_per_professional[[n]]
	l <- length(lst)
	m <- matrix(NA, nrow=l, ncol=l, dimnames=list(lst, lst))
	
	for(i in dimnames(m)[[1]]){
	
		for(j in dimnames(m)[[2]]){
		
			m[i, j] <- dists[i, j]
			
		}
		
	}
	
	g <- minimum.spanning.tree(graph.adjacency(m, weighted=TRUE, mode='undirected'))
	distances_per_professional[[n]] <- sum(E(g)$weight)
	
}

df <- data.frame(CPF=names(distances_per_professional), DIST=unlist(distances_per_professional))
plt <- ggplot(data=df, aes(DIST)) + geom_bar(aes(y=(..count..)/sum(..count..)), binwidth=10, colour='black', fill='red', alpha=0.5) + scale_y_continuous('Profissionais', labels=percent, breaks=seq(0, 1, 0.025)) + scale_x_continuous('Distância (km)', breaks=seq(0, 700, 50), limits=c(0, 600)) + ggtitle(paste(occupation, 'Distância percorrida por profissional\n(estimativa geodésica)\n', sep='\n'))

pdf(paste0('Distância - ', occupation, ' - percentual.pdf'), height=8, width=8)
print(plt)
dev.off()

