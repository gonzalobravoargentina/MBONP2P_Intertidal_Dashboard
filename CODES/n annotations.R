#DATA
##Data folder
Data <- "DATA"
#Proportion of the labels in the set

library(readr)
df <- read_csv(file.path(Data, "annotations.csv"))

library(dplyr)
dfSummary <- df %>% group_by(Label) %>% 
  summarise(nPoints = n(), site = paste0(unique(locality), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(df),1)) %>% 
  arrange(-nPoints) %>% 
  relocate(Label, nPoints, Percent, site)

library(formattable)
formattable(dfSummary, list(Percent=color_bar("steelblue")))


dfusers <- df %>% group_by(Annotator) %>% 
  summarise(nPoints = n(), site = paste0(unique(locality), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(df),1)) %>% 
  arrange(-nPoints) %>% 
  relocate(Annotator, nPoints, Percent, site)

dfusers$Photos <- dfusers$nPoints/100

dfusers <- dfusers%>% relocate(Annotator,Photos,nPoints, site)
