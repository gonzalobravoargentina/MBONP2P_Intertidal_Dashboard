#shiny function

library(shiny)
library(plotly)
library(dplyr)

#READ both files metadata and percent cover 
library(RColorBrewer)
palette(brewer.pal(8, "Set2"))
library(readr)
cover <- read_csv("/Users/gonzalobravo/Documents/GitHub/MBON_CoralNet_Dashboard/DATA/percent_covers.csv") # read cover data
metadata <- read_csv("/Users/gonzalobravo/Documents/GitHub/MBON_CoralNet_Dashboard/DATA/metadata.csv") # read metadata

# Merge photoquadrat.metadata and photoquadrat.cover
AMP <- merge(metadata, cover, by.x = "Name", by.y ="Image name", all.x = TRUE) 

# Remove original data frames from the environment
rm(cover, metadata)

# Reemplazar "CABO DOS BAHIAS" por "PIMCPA_NORTE" en la columna "locality" de AMP
AMP$locality <- ifelse(AMP$locality == "CABO DOS BAHIAS", "PIMCPA_NORTE", AMP$locality)
# Reemplazar "PIMCPA" por "PIMCPA_SUR" en la columna "locality" de AMP
AMP$locality <- ifelse(AMP$locality == "PIMCPA", "PIMCPA_SUR", AMP$locality)

# all seaweed
AMP$algae <- as.numeric(paste(AMP$MAA + AMP$MAEC + AMP$MAEF + AMP$MAEN + AMP$MAF + AMP$MAG + AMP$MALA + AMP$MALCB + AMP$MAS))

# Create long type dataframe 
library(reshape)
AMP_long <- melt(AMP, id.vars = 1:22, measure.vars = 23:ncol(AMP), variable_name = "CATAMI", value_name = "cover", na.rm = TRUE)
# Rename columns because the ontop command is not working 
colnames(AMP_long)[23:24] <- c("CATAMI", "cover")

# Calculate mean, SD, SE for cover data by factors 
library(doBy)
Coverdata <- summaryBy(cover ~ CATAMI + strata, data = AMP_long, FUN = function(x) { c(mean = mean(x), SD = sd(x), SE = sqrt(var(x)/length(x)))})

library(doBy)
Coverdata_AMPs <- summaryBy(cover ~ CATAMI + strata + locality, data = AMP_long, FUN = function(x) { c(mean = mean(x), SD = sd(x), SE = sqrt(var(x)/length(x)))})

library(dplyr)
library(lubridate)
AMP <- AMP %>%
  mutate(year = lubridate::year(Date),
         month = lubridate::month(Date)) %>%
  select(Name, Date, year, month, everything())

photo_bydate <- as.data.frame(table(AMP$year, AMP$site, AMP$locality, AMP$strata))
colnames(photo_bydate) <- c("Fecha", "Sitio", "Localidad", "Estrato", "n fotocuadrantes")  



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

# UI function
ui <- fluidPage(
  selectInput("localidad", "Localidad:", choices = unique(Coverdata_AMPs$locality)),
  plotlyOutput("pieChart", height = "600px")
)

# Server function
server <- function(input, output, session) {
  output$pieChart <- renderPlotly({
    localidad <- input$localidad
    
    # Filtrar los datos para la localidad actual
    Coverdata_AMPs_site <- filter(Coverdata_AMPs, locality == localidad)
    
    # Crear un gráfico de torta para cada estrato
    sel_data.L <- filter(Coverdata_AMPs_site, strata == "LOWTIDE", cover.mean > 0, CATAMI != "algae")
    sel_data.M <- filter(Coverdata_AMPs_site, strata == "MIDTIDE", cover.mean > 0, CATAMI != "algae")
    sel_data.H <- filter(Coverdata_AMPs_site, strata == "HIGHTIDE", cover.mean > 0, CATAMI != "algae")
    
    p <- plot_ly(labels = ~CATAMI, values = ~cover.mean, legendgroup = ~CATAMI, textinfo = 'label+percent',
                 marker = list(colors = ~Color)) %>%
      add_pie(data = sel_data.L, name = "Bajo", title = 'Estrato Bajo', domain = list(row = 0, column = 0)) %>%
      add_pie(data = sel_data.M, name = "Medio", title = 'Estrato Medio', domain = list(row = 0, column = 1)) %>%
      add_pie(data = sel_data.H, name = "Alto", title = 'Estrato Alto', domain = list(row = 0, column = 2)) %>%
      layout(title = localidad, showlegend = T,
             grid = list(rows = 1, columns = 3),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}

shinyApp(ui, server)
