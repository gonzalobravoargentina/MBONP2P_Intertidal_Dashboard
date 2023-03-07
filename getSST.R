### Get SST timeseries from satellite products using erddap
### the source of data is `jplMURSST41`. See https://coastwatch.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html
### data is extracted with `rerddap::griddap` for a particular coordinate and stored as csv file.
### E Klein. eklein@usb.ve
### 2019-04-10

library(readr)
library(rerddap)
library(lubridate)
library(dplyr)


## functions

## remove all spaces from string
NoSpaces = function(x){
  return(gsub(" ", "", x))
}

## set site coordinates and time for SST extraction
SSTSiteName = "PIMCPA"   ## for the resulting file name
SSTcoords.lon = -65.777863
SSTcoords.lat = -45.077625

SSTstartDate = "2002-06-01"

## set climatological date start-end
SSTclimStartDate = "2002-06-01"
SSTclimEndDate = "2012-12-31"

## set dataset source
SSTsource = info("jplMURSST41")

##
## Get sst 
SST = griddap(SSTsource, 
              time=c(SSTstartDate, "last"),
              longitude = c(SSTcoords.lon, SSTcoords.lon),
              latitude = c(SSTcoords.lat, SSTcoords.lat),
              fields = "analysed_sst",
              fmt = "csv")

SST = SST[,c(1,4)]
names(SST) = c("time", "SST")

## convert time to a Data object
SST$time = as.Date(ymd_hms(SST$time))

##
## Calculate climatology
SST.clim = SST %>% filter(time>=ymd(SSTclimStartDate), time<=SSTclimEndDate) %>% 
  group_by(yDay = yday(time)) %>% 
  summarise(SST.mean = mean(SST),
            SST.median = median(SST),
            SST.sd = sd(SST),
            SST.q5 = quantile(SST, 0.05),
            SST.q10 = quantile(SST, 0.10),
            SST.q25 = quantile(SST, 0.25),
            SST.q75 = quantile(SST, 0.75),
            SST.q90 = quantile(SST, 0.90),
            SST.q95 = quantile(SST, 0.95),
            SST.min = min(SST),
            SST.max = max(SST))


## save SST
write_csv(SST, path = paste0(NoSpaces(SSTSiteName), "_SST.csv"))
write_csv(SST.clim, path = paste0(NoSpaces(SSTSiteName), "_Climatology.csv"))

