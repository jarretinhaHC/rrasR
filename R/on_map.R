library(XLConnect)
library(ggplot2)
library(Hmisc)
library(scales)
library(reshape2)
library(RColorBrewer)
library(rgdal)
library(ggmap)
library(maptools)
library(rgeos)

setScale(1e+15)
map_center <- c(lon=-48.433700, lat=-22.070647)

basedir <- '/data/Brazil/ref_data'

occupations_list <- (read.delim(paste(basedir, 'cbo.dat', sep='/'), sep='\t', check.names=FALSE, as.is=TRUE, header=FALSE))[,1]
specialties_list <- (read.delim(paste(basedir, 'especialidades.dat', sep='/'), sep='\t', check.names=FALSE, as.is=TRUE, header=FALSE))[,1]
program_data <- read.delim(paste(basedir, 'programas_SP_with_coords.tsv', sep='/'), sep='\t', check.names=FALSE, as.is=TRUE)
adequacy_data <- read.delim(paste(basedir, 'adequacy.dat', sep='/'), sep='\t', row.names=1, check.names=FALSE, as.is=TRUE)
files <- Sys.glob('/home/jarretinha/dev/cnme/sheets/Profissionais/CNME - Profissionais por ocupação - Especialidades - PSA*')

rras_wb <- loadWorkbook(paste(basedir, 'Regiões_SP.xls', sep='/'))
rras_data <- readWorksheet(rras_wb, 'Regiões', check.names=FALSE)
rras_list <- unique(rras_data$'NOME_RRAS')

# Read cleaned spatial data
spdf <- readShapePoly(paste(basedir, 'RRAS_spdf', sep='/'))
row.names(spdf@data) <- spdf@data$SP_ID

# Create data frames
by_specialty <- data.frame(row.names=specialties_list)
by_occupation <- data.frame(row.names=occupations_list)
by_adequacy <- data.frame(row.names=occupations_list)
without_res <- data.frame(row.names=occupations_list)
ratio <- data.frame(row.names=occupations_list)
total_by_occupation <- data.frame(row.names=occupations_list)

for(f in files){

	RRAS <- unlist(strsplit(f, '[ -.]'))[15]

	wb <- loadWorkbook(f)
	sheet <- readWorksheet(wb, sheet='Profissionais - Especialidades', rownames=1, check.names=FALSE)
	total_by_occupation[, RRAS] <- as.data.frame(rowSums(sheet))
	with_res <- sheet[, names(sheet) != 'SEM RESIDÊNCIA']
	without_res[, RRAS] <- as.data.frame(sheet[, names(sheet) == 'SEM RESIDÊNCIA'])
	by_specialty[, RRAS] <- as.data.frame(colSums(with_res))
	by_occupation[, RRAS] <- as.data.frame(rowSums(with_res))

	for(o in occupations_list){
		
		s <- adequacy_data[o, ]
		by_adequacy[o, RRAS] <- with_res[o, s]
	
	}

	ratio[, RRAS] <- total_by_occupation[, RRAS]/spdf@data[RRAS, 3]
	
}

# Set occupation and specialty
occupation <- 'MEDICO PSIQUIATRA'
specialty <- 'PSIQUIATRIA'

# ratio
tmp <- as.matrix(ratio)
scale_factor <- 10000
m <- sprintf('%2.2f/%d hab', mean(unlist(ratio[occupation, ])) *
             scale_factor, scale_factor)
s <- sprintf('%2.2f/%d hab', sd(unlist(ratio[occupation, ])) *
             scale_factor, scale_factor)
tmp[tmp==Inf] <- 0
tmp <- scale(t(tmp))
tmp[tmp=='NaN'] <- 0
ratio <- as.data.frame(tmp)
ratio[, 'SP_ID'] <- rownames(ratio)


specialty_programs <- program_data[program_data$NO_ESPECIALIDADE == specialty, ] 

labels <- paste(spdf$SP_ID, '\n', percent(t(by_adequacy[occupation, ]/total_by_occupation[occupation,])), ' (', by_adequacy[occupation, ], '/', total_by_occupation[occupation,], ')', sep='')
labels <- data.frame(value=labels, row.names=spdf$SP_ID)
labels$value <- as.character(labels$value)

df <- fortify(spdf)
ratio$id <- unique(df$id)

df <- merge(df, ratio, by.y='id', all.x=TRUE)

# Plot a beautiful map is and long, hard, tricky and quite repetitive art
# Don't be lazy and separate each group of graphical elements on its own

# Load data and set coords
plt <- ggplot(data=df) + coord_map()

# Construct map on multiple steps to avoid complicated expression lines
plt <- plt + geom_polygon(aes(x=long, y=lat, group=group, fill=cut(df[[occupation]], 11)), color='black', alpha=0.4, size=0.1)

# Map non-textual annotations
plt <- plt + geom_point(data=specialty_programs, aes(x=LON, y=LAT,
                                                     color=as.factor(DS_NATUREZA)),
                        size=5)
plt <- plt + geom_point(data=specialty_programs, aes(x=LON, y=LAT),
                        color='black',
                        shape=1,
                        size=5)

# Map textual annotations
## RRAS with arrows

# RRAS01
lx <- -44.25
ly <- -25.00
cx <- -46.49718
cy <- -23.75958
xoffset <- 4e-1
yoffset <- -1e-1
plt <- plt + annotate('text', label=labels['RRAS01', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 


# RRAS02
lx <- -43.91140
ly <- -23.75271
cx <- -46.16216
cy <- -23.50231
xoffset <- 5e-1
yoffset <- -.5e-1
plt <- plt + annotate('text', label=labels['RRAS02', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS03
lx <- -44.00
ly <- -24.50
cx <- -46.68418
cy <- -23.32776
xoffset <- 5e-1
yoffset <- -.5e-1
plt <- plt + annotate('text', label=labels['RRAS03', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS04
lx <- -47.00
ly <- -25.75
cx <- -46.95003
cy <- -23.81887
xoffset <- 0
yoffset <- -2e-1
plt <- plt + annotate('text', label=labels['RRAS04', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS05
lx <- -46.0
ly <- -25.75
cx <- -46.91287
cy <- -23.47589
xoffset <- 0
yoffset <- -2e-1
plt <- plt + annotate('text', label=labels['RRAS05', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS06
lx <- -45.0
ly <- -25.50
cx <- -46.64811
cy <- -23.65008
xoffset <- .5e-1
yoffset <- -2e-1
plt <- plt + annotate('text', label=labels['RRAS06', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS15
lx <- -45.99536
ly <- -21.49355
cx <- -46.97420
cy <- -22.21687
xoffset <- 3e-1
yoffset <- 2e-1
plt <- plt + annotate('text', label=labels['RRAS15', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

# RRAS16
lx <- -44.20803
ly <- -21.29710
cx <- -46.59743
cy <- -23.01132
xoffset <- 3e-1
yoffset <- 2e-1
plt <- plt + annotate('text', label=labels['RRAS16', 'value'], x=lx + xoffset, y=ly + yoffset, fontsize=12) + annotate('segment', x=cx, y=cy, xend=lx, yend=ly, size=0.5, arrow=arrow(length=unit(.2, "cm"))) 

## RRAS without arrows

# RRAS07
lx <- -47.68118
ly <- -24.45593
plt <- plt + annotate('text', label=labels['RRAS07', 'value'], x=lx, y=ly, fontsize=12)

# RRAS08
lx <- -48.25192
ly <- -23.81380
plt <- plt + annotate('text', label=labels['RRAS08', 'value'], x=lx, y=ly, fontsize=12)

# RRAS09
lx <- -48.94354
ly <- -22.60975
plt <- plt + annotate('text', label=labels['RRAS09', 'value'], x=lx, y=ly, fontsize=12)

# RRAS10
lx <- -50.23744
ly <- -22.32559
plt <- plt + annotate('text', label=labels['RRAS10', 'value'], x=lx, y=ly, fontsize=12)

# RRAS11
lx <- -51.65651
ly <- -22.10692
plt <- plt + annotate('text', label=labels['RRAS11', 'value'], x=lx, y=ly, fontsize=12)

# RRAS12
lx <- -50.19727
ly <- -20.79244
plt <- plt + annotate('text', label=labels['RRAS12', 'value'], x=lx, y=ly, fontsize=12)

# RRAS13
lx <- -48.08870
ly <- -21.04926
plt <- plt + annotate('text', label=labels['RRAS13', 'value'], x=lx, y=ly, fontsize=12)

# RRAS14
lx <- -47.59461
ly <- -22.48592
plt <- plt + annotate('text', label=labels['RRAS14', 'value'], x=lx, y=ly, fontsize=12)

# RRAS17
lx <- -45.30857
ly <- -23.05213
plt <- plt + annotate('text', label=labels['RRAS17', 'value'], x=lx, y=ly, fontsize=12)

# Google maps stuff
#nc <- get_map(location=map_center, zoom = 6, maptype = 'watercolor')
#plt <- plt + ggmap(nc, extent='device')

# Scales
txt_fill <- paste0(occupation,
                   '\nz-score, especialista/hab',
                   '\nMédia geral: ', m,
                   '\nDesvio geral: ', s,
                   '\n')
txt_color <- 'Instituições com programa de residência na especialidade\n'
plt <- plt + scale_color_brewer(txt_color, palette='Set3')
plt <- plt + scale_fill_brewer(txt_fill, palette='RdYlGn')
plt <- plt + scale_x_continuous(breaks = round(seq(-54, -43, by = 0.25), 2)) 
plt <- plt + scale_y_continuous(breaks = round(seq(-27, -19, by = 0.25), 2))
plt <- plt + scale_shape_discrete(guide=FALSE)

# Labels
plt <- plt + labs(x='', y='')
title <- paste0(occupation, '\nDistribuição dos profissionais e locais de formação')
plt <- plt + ggtitle(title)
# Theme/Style
plt <- plt + theme_bw()

# Themes/Panel
plt <- plt + theme(panel.grid.minor=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.border=element_blank())
# Theme/Axis
plt <- plt + theme(axis.ticks=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank())
# Theme/Plot
plt <- plt + theme(plot.margin=unit(c(0, 0, 0, 0), 'in'))
plt <- plt + theme(plot.title=element_text(face="bold", size=20))

# Print and pray!
pdf(paste('Densidade profissional - ', occupation, '.pdf', sep=''), height=24, width=24)
print(plt)
dev.off()
