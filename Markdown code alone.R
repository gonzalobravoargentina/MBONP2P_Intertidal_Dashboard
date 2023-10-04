library(flexdashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(spocc)
library(readr)
library(reshape2)
library(xts)
library(dygraphs)
library(plotly)
library(lubridate)
library(dplyr)
library(htmltools)
library(DT)
library(shiny)
library(xts)
library(htmlwidgets)
library(RColorBrewer)
palette(brewer.pal(8, "Set2"))


#READ both files metadata and percent cover 

# Suppress summaries info
options(dplyr.summarise.inform = FALSE)

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
AMP$region <- ifelse(AMP$locality %in% c("MAR DEL PLATA", "ISLOTE LOBOS", "PUNTA BS AS"), "NORTH",
                     ifelse(AMP$locality %in% c("PUERTO MADRYN"), "CENTER",
                            ifelse(AMP$locality %in% c("PUNTA TOMBO", "PIMCPA_SUR", "PUERTO BUQUE", "MAKENKE", "MONTE LEON", "PIMCPA_NORTE"), "SOUTH", NA)))

AMP <- AMP %>%
  select(country, region, everything())



#all seaweed
AMP$algae <- as.numeric(paste(AMP$MAA +AMP$MAEC + AMP$MAEF+ AMP$MAEN+ AMP$MAF+ AMP$MAG+ AMP$MALA+ AMP$MALCB+ AMP$MAS))







# Puedes imprimir el DataFrame para verificar los resultados
print(AMP)







#Create long type dataframe 
library(reshape)
AMP_long = melt(AMP, id.vars = 1:22, measure.vars = 23:ncol(AMP), variable_name = "CATAMI", value_name ="cover", na.rm = T)
#rename columns because the ontop command is not working 
colnames(AMP_long)[23] <- "CATAMI"
colnames(AMP_long)[24] <- "cover"

#Calculate mean, SD, SE for cover data by factors 
library(doBy)
Coverdata <- summaryBy(cover ~ CATAMI + strata,data=AMP_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})

library(doBy)
Coverdata_AMPs <- summaryBy(cover ~ CATAMI + strata +locality ,data=AMP_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})

library(dplyr)
library(lubridate)
AMP <- AMP %>%
  mutate(year = lubridate::year(Date))  %>%
  select(Name,Date, year, everything())

photo_bydate = as.data.frame(table(AMP$year,AMP$site,AMP$locality,AMP$strata))
colnames(photo_bydate)=c("Fecha","Sitio","Localidad","Estrato","n fotocuadrantes")  

#SST-----
#getSST.r was used to get data 
## get sampling event dates
samplingDates = unique(AMP$Date)

## read SST values
library(readr)
setwd(paste0(getwd(),"/DATA"))#set new WD to folder DATA
SST = read_csv("ISLAPINGUINO_SST.csv")
SST.clim = read_csv("ISLAPINGUINO_Climatology.csv")
setwd("..")# original WD



### Mapa Fotoquadrantes-------
library(leaflet)

# Crear el mapa
map <- leaflet(AMP)%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldImagery",
    # give the layer a name
    group = "Satelite"
  ) %>%
  addProviderTiles(
    "OpenStreetMap",
    group = "Mapa para zoom"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "Satelite", "Mapa para zoom"),
    # position it on the topleft
    position = "topleft"
  ) %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = AMP, 
    ~Longitude, 
    ~Latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T,
    popup = ~Comments,
    clusterOptions = markerClusterOptions())




map  # Mostrar el mapa


### Cobertura de organismos vivos por estrato--------
# Use this when using percent_covers.csv and full names from CoralNet
# taxacover = AMP_long %>% filter(CATAMI != "Substrate..Consolidated..hard.") %>% 
#   group_by(site, strata, Image.ID) %>% 
#   summarise(sumcover = sum(cover, na.rm=T))

# Use this when using percent_covers_AMP2023.cvs and short names from CoralNet
taxacover = AMP_long %>% filter(CATAMI != "SC", CATAMI !="algae") %>% 
  group_by(locality, strata, Name) %>% 
  summarise(sumcover = sum(cover, na.rm=T))

pp = ggplot(taxacover, aes(x=factor(strata,level=c('LOWTIDE', 'MIDTIDE', 'HIGHTIDE')), sumcover, fill=strata))
pp = pp + geom_boxplot() + ylab("% Cobertura de todas las especies por cuadrante") + xlab("")+
  facet_grid(~locality) + 
  theme_bw(base_size = 10) + theme(legend.position = "none")
ggplotly(pp)


### Coberturas de Molluscos-------
library(echarts4r)

# Use this when using percent_covers.csv and full names from CoralNet
# value <- Coverdata[Coverdata$CATAMI == "Molluscs..Bivalves", ]

# Use this when using percent_covers_AMP2023.cvs and short names from CoralNet
value <- Coverdata[Coverdata$CATAMI == "MOB", ]

Molluscs <- e_charts() %>% 
  e_gauge(round(value$cover.mean[1], 0), 
          "ALTO", 
          center = c("20%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(1, "green"),
                c(1, "green"),
                c(1, "green")
              )
            ))) %>% 
  e_gauge(round(value$cover.mean[3], 0), 
          "MEDIO", 
          center = c("50%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(0.20, "red"),
                c(0.4, "yellow"),
                c(1, "green")
              )
            ))) %>% 
  e_gauge(round(value$cover.mean[2], 0), 
          "BAJO", 
          center = c("80%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(1, "green"),
                c(1, "green"),
                c(1, "green")
              )
            ))) %>% 
  e_title("Moluscos (%)")

Molluscs



### Coberturas de Macro-Algas------
# this creates a gauge with % cover values color coded by threshold levels
library(echarts4r)

# Use  short names from CoralNet
value <- Coverdata[Coverdata$CATAMI == "algae", ]


MAS <- e_charts() %>% 
  e_gauge(round(value$cover.mean[1], 0), 
          "ALTO", 
          center = c("20%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(1, "green"),
                c(1, "green"),
                c(1, "green")
              )
            ))) %>% 
  e_gauge(round(value$cover.mean[3], 0), 
          "MEDIO", 
          center = c("50%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(1, "green"),
                c(1, "green"),
                c(1, "green")
              )
            ))) %>% 
  e_gauge(round(value$cover.mean[2], 0), 
          "BAJO", 
          center = c("80%", "20%"), 
          radius = "35%",
          color = "black",
          min=0, 
          max=100,
          splitNumber = 5,
          axisLine = list(
            lineStyle = list(
              color=list(
                c(0.10, "red"),
                c(.20, "yellow"),
                c(1, "green")
              )
            ))) %>% 
  e_title("Macro-algas (%)")

MAS


### Frecuencia de especies-----

taxafreq = AMP_long %>% filter(cover>0)%>%
  group_by(locality, strata, CATAMI) %>%  
  summarise(sppfreq = n()) %>% arrange(sppfreq) %>% mutate(sppacum = cumsum(sppfreq))

pp = ggplot(taxafreq, aes(CATAMI, sppfreq, fill=strata))
pp = pp + geom_bar(stat="identity") + coord_flip() + facet_grid(~locality) + 
  theme_bw(base_size = 10) + xlab("") + ylab("Número de foto-cuadrantes")

ggplotly(pp)



### Categorias más abundantes por estrato------

library(plotly)
library(dplyr)
library(cols4all)
# Obtener las categorías únicas en la columna 'CATAMI'
categorias_unicas <- unique(Coverdata_AMPs$CATAMI)
num_categorias <- length(categorias_unicas)

# Obtener una paleta de colores de cols4all con 18 colores
paleta_cols4all <- c4a("poly.alphabet", 18)

# Crear un vector de colores asignados a las categorías
colores_categoria <- paleta_cols4all[match(Coverdata_AMPs$CATAMI, categorias_unicas)]

# Asignar los colores al dataframe en una nueva columna llamada 'Color'
Coverdata_AMPs$Color <- colores_categoria

# Obtener las localidades únicas en la columna 'locality'
localidades_unicas <- unique(Coverdata_AMPs$locality)

# Iniciar un loop para cada localidad
for (localidad in localidades_unicas) {
  # Filtrar los datos para la localidad actual
  Coverdata_AMPs_site <- filter(Coverdata_AMPs, locality == localidad)
  
  # Crear un gráfico de torta para cada estrato
  sel_data.L <- filter(Coverdata_AMPs_site, strata == "LOWTIDE", cover.mean > 0, CATAMI != "algae")
  sel_data.M <- filter(Coverdata_AMPs_site, strata == "MIDTIDE", cover.mean > 0, CATAMI != "algae")
  sel_data.H <- filter(Coverdata_AMPs_site, strata == "HIGHTIDE", cover.mean > 0, CATAMI != "algae")
  
  p <- plot_ly(labels = ~CATAMI, values = ~cover.mean, legendgroup = ~CATAMI, textinfo = 'label+percent',
               marker = list(colors = ~Color)) %>%  # Aquí asignamos los colores desde la columna 'Color'
    add_pie(data = sel_data.L, name = "Bajo", title = 'Estrato Bajo', domain = list(row = 0, column = 0)) %>%
    add_pie(data = sel_data.M, name = "Medio", title = 'Estrato Medio', domain = list(row = 0, column = 1)) %>%
    add_pie(data = sel_data.H, name = "Alto", title = 'Estrato Alto', domain = list(row = 0, column = 2)) %>%
    layout(title = localidad, showlegend = T,
           grid = list(rows = 1, columns = 3),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # Mostrar el gráfico para esta localidad
  print(p)
}



### nMDS-----
library(vegan)
#nMDS calculations (no transformation + Bray)
AMP <- AMP[,-(42)]# take out algae

unique(AMP$locality)
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
MDS.plot_mid<-cbind(AMP_mid[,-(1:22)], NMDS1.mid, NMDS2.mid,AMP_mid$locality) 
#nMDS plot 
mid <- ggplot(MDS.plot_mid, aes(NMDS1.mid, NMDS2.mid, color=AMP_mid$locality))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "top",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.mid)-0.5, y=min(NMDS2.mid)-0.5, label=paste('Stress =',round(nMDS_mid$stress,3)))+ scale_color_brewer(palette = "Set1") 


AMP_high <- subset(AMP,strata=="HIGHTIDE")
nMDS_high=metaMDS(AMP_high[,-(1:22)],k=2,trymax=3,try = 3,distance ="bray",autotransform = F)
NMDS1.high <-nMDS_high$points[,1] 
NMDS2.high <- nMDS_high$points[,2]
MDS.plot_high<-cbind(AMP_high[,-(1:22)], NMDS1.high, NMDS2.high,AMP_high$locality) 
#nMDS plot 
high <- ggplot(MDS.plot_high, aes(NMDS1.high, NMDS2.high, color=AMP_high$locality))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "top",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.high)-0.5, y=min(NMDS2.high)-0.5, label=paste('Stress =',round(nMDS_high$stress,3)))+ scale_color_brewer(palette = "Set1")


library(patchwork)
(low / mid / high)


### Fotoquadrantes por fecha y sitio
knitr::kable(as.data.frame(table(AMP$year,AMP$locality)),col.names = c("Año","Localidad","n"))

