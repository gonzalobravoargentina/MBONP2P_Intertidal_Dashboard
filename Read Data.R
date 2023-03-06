#CORALNET files:
# annotations.csv (info about each pooint)
# metadata.csv
# percent_cover.csv

#Source: https://coralnet.ucsd.edu/source/2897/

#READ both files metadata and percent covers 
setwd(paste0(getwd(),"/DATA"))#set new WD to folder DATA
PIMCPA.cover <- read.csv("percent_covers.csv")
PIMCPA.metadata <- read.csv("metadata.csv")
setwd("..")# original WD

colnames(PIMCPA.cover)[2] <-"Name" 
#Merge PIMCPA.metadata and PIMCPA.cover
PIMCPA<- merge(PIMCPA.metadata,PIMCPA.cover, by = "Name", all.x = TRUE) 


#Create long type dataframe 
library(reshape)
PIMCPA_long = melt(PIMCPA, id.vars = 1:21, measure.vars = 22:ncol(PIMCPA), variable_name = "CATAMI", value_name ="cover", na.rm = T)
#rename columns because the ontop command is not working 
colnames(PIMCPA_long)[23] <- "cover"

names(PIMCPA_long)
#Calculate mean, SD, SE for cover data by factors 
library(doBy)
Coverdata <- summaryBy(cover ~ CATAMI + strata,data=PIMCPA_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})


#add year from date info
PIMCPA$year <- lubridate::year(PIMCPA$Date)

photo_bydate = as.data.frame(table(PIMCPA$year,PIMCPA$site,PIMCPA$strata))
colnames(photo_bydate)=c("Fecha","Sitio","Estrato","n fotocuadrantes") 
