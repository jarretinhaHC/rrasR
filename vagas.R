library(ggplot2)
library(Hmisc)
library(scales)
library(reshape2)
library(RColorBrewer)

data <- read.delim('programas_vagas.tsv', sep='\t', check.names=FALSE, as.is=TRUE)
data$co_resolucao <- c()

minyr <- 2002
maxyr <- 2014
mx <- 100000

groups <- read.delim('grupos.tsv', sep='\t', check.names=FALSE, as.is=TRUE)
groups$no_programa <- c()

data <- merge(data, groups, by='co_especialidade')
data <- data[data$nu_ano %in% minyr:maxyr, ]
data$co_periodo <- factor(data$co_periodo)
data_SP <- data[data$sg_estado == 'SP', ]

data_R <- data[data$sg_grupo == 'R', ]
data_R_SP <- data[data$sg_grupo == 'R' & data$sg_estado == 'SP', ]

data_P <- data[data$sg_grupo == 'P', ]
data_P_SP <- data[data$sg_grupo == 'P' & data$sg_estado == 'SP', ]

data_D <- data[data$sg_grupo == 'D', ]
data_D_SP <- data[data$sg_grupo == 'D' & data$sg_estado == 'SP', ]


plt <- ggplot(data=data, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 1), limits=c(minyr, maxyr), oob=rescale_none) + scale_y_continuous(breaks=seq(0, mx, 1000)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Accent') + ggtitle('Brasil')

pdf('Todas - Brasil.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_SP, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 1), limits=c(minyr, maxyr), oob=rescale_none) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Accent') + ggtitle('SP')

pdf('Todas - SP.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_R, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 1), limits=c(minyr, maxyr), oob=rescale_none) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Accent') + ggtitle('Pré-requisitos - Brasil')

pdf('Pré-requisitos - Brasil.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_R_SP, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 2), limits=c(minyr, maxyr)) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Set3') + ggtitle('Pré-requisitos - SP')

pdf('Pré-requisitos - SP.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_D, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 1), limits=c(minyr, maxyr), oob=rescale_none) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Accent') + ggtitle('Acesso direto - Brasil')

pdf('Direto - Brasil.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_D_SP, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 2), limits=c(minyr, maxyr)) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Set3') + ggtitle('Acesso direto - SP')

pdf('Direto - SP.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_P, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 1), limits=c(minyr, maxyr), oob=rescale_none) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Accent') + ggtitle('Acesso indireto - Brasil')

pdf('Indireto - Brasil.pdf', height=12, width=12)
print(plt)
dev.off()

plt <- ggplot(data=data_P_SP, aes(x=nu_ano, y=nu_vagas, fill=co_periodo)) + stat_summary(fun.y='sum', aes(fill=co_periodo), geom='bar', color='black', position=position_dodge(width=0.75)) + scale_x_continuous(breaks=seq(minyr, maxyr, 2), limits=c(minyr, maxyr)) + scale_y_continuous(breaks=seq(0, mx, 200)) + xlab('Ano') + ylab('Vagas') + scale_fill_brewer('Período', palette='Set3') + ggtitle('Acesso indireto - Brasil')

pdf('Indireto - SP.pdf', height=12, width=12)
print(plt)
dev.off()

