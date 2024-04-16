# CODE for nMDS between localities 

#DATA
##Data folder
Data <- "DATA"

#COVER DATA-------------
#We download from CORALNET two files, 1-species cover information and 2- metadata of photoquadrats:
# 1- metadata.csv
# 2- percent_cover.csv

library(readr)
cover <- read_csv(file.path(Data, "percent_covers.csv"))#read cover data
metadata <- read_csv(file.path(Data,"metadata.csv"))#read metadata

#Merge photoquadrat.metadata and photoquadrat.cover
AMP<- merge(metadata,cover, by.x = "Name", by.y ="Image name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(cover)
rm(metadata)

# Reemplazar "CABO DOS BAHIAS" por "PIMCPA_NORTE" en la columna "locality" de AMP
AMP$locality <- ifelse(AMP$locality == "CABO DOS BAHIAS", "PIMCPA_NORTE", AMP$locality)
# Reemplazar "PIMCPA" por "PIMCPA_SUR" en la columna "locality" de AMP
AMP$locality <- ifelse(AMP$locality == "PIMCPA", "PIMCPA_SUR", AMP$locality)

#Asign REGION for localities 
AMP$region <- ifelse(AMP$locality %in% c("MAR DEL PLATA"), "Uruguay - Buenos Aires Shelf",
                     ifelse(AMP$locality %in% c("PUERTO MADRYN","ISLOTE LOBOS", "PUNTA BS AS","PIMCPA_SUR","PIMCPA_NORTE","PUNTA TOMBO"), "North Patagonian Gulfs",
                            ifelse(AMP$locality %in% c("PUERTO BUQUE", "MAKENKE", "MONTE LEON"), "Patagonian Shelf", NA)))

AMP <- AMP %>%
  select(country, region, everything())



### nMDS-----
library(vegan)
library(ggplot2)
#nMDS calculations (no transformation + Bray)

AMP_low <- subset(AMP,strata=="LOWTIDE")
nMDS_low=metaMDS(AMP_low[,-(1:22)],k=2,trymax=3,try = 3,distance ="bray",autotransform = F)
NMDS1.low <-nMDS_low$points[,1] 
NMDS2.low <- nMDS_low$points[,2]
MDS.plot_low<-cbind(AMP_low[,-(1:22)], NMDS1.low, NMDS2.low,AMP_low$locality) 
#nMDS plot 
low <- ggplot(MDS.plot_low, aes(NMDS1.low, NMDS2.low, color=AMP_low$locality))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "top",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.low)-0.5, y=min(NMDS2.low)-0.5, label=paste('Stress =',round(nMDS_low$stress,3))) + scale_color_brewer(palette = "Set1") 

AMP_mid <- subset(AMP,strata=="MIDTIDE")
nMDS_mid=metaMDS(AMP_mid[,-(1:22)],k=2,trymax=3,try = 3,distance ="bray",autotransform = F)
NMDS1.mid <-nMDS_mid$points[,1] 
NMDS2.mid <- nMDS_mid$points[,2]
MDS.plot_mid<-cbind(AMP_mid[,-(1:22)], NMDS1.mid, NMDS2.mid,AMP_mid$region) 
#nMDS plot 
mid <- ggplot(MDS.plot_mid, aes(NMDS1.mid, NMDS2.mid, color=AMP_mid$region))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "top",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.mid)-0.5, y=min(NMDS2.mid)-0.5, label=paste('Stress =',round(nMDS_mid$stress,3)))+ scale_color_brewer(palette = "Set1") 


AMP_high <- subset(AMP,strata=="HIGHTIDE")
nMDS_high=metaMDS(AMP_high[,-(1:22)],k=2,trymax=3,try = 3,distance ="bray",autotransform = F)
NMDS1.high <-nMDS_high$points[,1] 
NMDS2.high <- nMDS_high$points[,2]
MDS.plot_high<-cbind(AMP_high[,-(1:22)], NMDS1.high, NMDS2.high,AMP_high$region) 
#nMDS plot 
high <- ggplot(MDS.plot_high, aes(NMDS1.high, NMDS2.high, color=AMP_high$region))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "top",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.high)-0.5, y=min(NMDS2.high)-0.5, label=paste('Stress =',round(nMDS_high$stress,3)))+ scale_color_brewer(palette = "Set1")


library(patchwork)
(low / mid / high)

