#BOXPLOTS for regions

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


library(ggplot2)
# Definir el orden deseado en la leyenda
orden_fill <- c("Uruguay - Buenos Aires Shelf", "North Patagonian Gulfs", "Patagonian Shelf")

# Crea una paleta de colores personalizada
mi_paleta <- c("brown", "gray", "orange", "purple", "yellow", "red", "pink", "gray", "black","blue")

# Define el orden en el eje x
orden_x <- c("LOWTIDE", "MIDTIDE", "HIGHTIDE")

# Crea una función para simplificar la creación de gráficos
crear_grafico <- function(data, variable, titulo) {
  ggplot(data, aes(x = factor(strata, levels = orden_x), y = .data[[variable]])) +
    geom_boxplot(aes(fill = factor(region, levels = orden_fill))) +
    labs(title = titulo, y = "Cover %", x = "Strata", fill = "Region") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
    guides(fill = guide_legend(title = "Region"))
}

# Crear los cuatro gráficos
grafico_SC <- crear_grafico(AMP, "SC", "SC")
grafico_MOB <- crear_grafico(AMP, "MOB", "MOB")
grafico_MAF <- crear_grafico(AMP, "MAF", "MAF")
grafico_MAS <- crear_grafico(AMP, "MAS", "MAS")
grafico_MAA <- crear_grafico(AMP, "MAA", "MAA")
grafico_MAEN <- crear_grafico(AMP, "MAEN", "MAEN")


legend <- ggplot(AMP, aes(x = strata, y = `SC`)) +
  geom_boxplot(aes(fill = factor(region, levels = orden_fill))) +
  labs(title = "", y = "Cover %", x = "Strata", fill = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
  guides(fill = guide_legend(title = "Region"))

legend2 <- ggplot(AMP, aes(x = strata, y = `SC`)) +
  geom_boxplot(aes(fill = factor(region, levels = orden_fill))) +
  labs(title = "", y = "Cover %", x = "Strata", fill = "Region") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
  guides(fill = guide_legend(title = "Region"))

library(cowplot)
legend_plot <- get_legend(legend)#take legend
legend_plot2 <- get_legend(legend2)
plot_grid(grafico_SC, grafico_MOB, grafico_MAF, grafico_MAS,grafico_MAA,grafico_MAEN,legend_plot2 ,legend_plot, ncol = 3,nrows = 3, rel_heights = c(1,1,0.1))





#PLOT for locality 
#

library(ggplot2)
# Definir el orden deseado en la leyenda
orden_fill <- c("MAR DEL PLATA", "ISLOTE LOBOS","PUNTA BS AS","PUERTO MADRYN","PUNTA TOMBO","PIMCPA_NORTE", "PIMCPA_SUR", "PUERTO BUQUE", "MAKENKE", "MONTE LEON")

# Crea una paleta de colores personalizada
mi_paleta <- c("brown", "gray", "orange", "purple", "yellow", "red", "pink", "gray", "black","blue")

# Define el orden en el eje x
orden_x <- c("LOWTIDE", "MIDTIDE", "HIGHTIDE")

# Crea una función para simplificar la creación de gráficos
crear_grafico <- function(data, variable, titulo) {
  ggplot(data, aes(x = factor(strata, levels = orden_x), y = .data[[variable]])) +
    geom_boxplot(aes(fill = factor(locality, levels = orden_fill))) +
    labs(title = titulo, y = "Cover %", x = "Strata", fill = "Locality") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
    guides(fill = guide_legend(title = "Region"))
}

# Crear los cuatro gráficos
grafico_SC <- crear_grafico(AMP, "SC", "SC")
grafico_MOB <- crear_grafico(AMP, "MOB", "MOB")
grafico_MAF <- crear_grafico(AMP, "MAF", "MAF")
grafico_MAS <- crear_grafico(AMP, "MAS", "MAS")
grafico_MAA <- crear_grafico(AMP, "MAA", "MAA")
grafico_MAEN <- crear_grafico(AMP, "MAEN", "MAEN")


legend <- ggplot(AMP, aes(x = strata, y = `SC`)) +
  geom_boxplot(aes(fill = factor(locality, levels = orden_fill))) +
  labs(title = "", y = "Cover %", x = "Strata", fill = "Locality") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
  guides(fill = guide_legend(title = "Region"))

legend2 <- ggplot(AMP, aes(x = strata, y = `SC`)) +
  geom_boxplot(aes(fill = factor(region, levels = orden_fill))) +
  labs(title = "", y = "Cover %", x = "Strata", fill = "Region") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = mi_paleta, breaks = orden_fill) +
  guides(fill = guide_legend(title = "Region"))

library(cowplot)
legend_plot <- get_legend(legend)#take legend
legend_plot2 <- get_legend(legend2)
plot_grid(grafico_SC, grafico_MOB, grafico_MAF, grafico_MAS,grafico_MAA,grafico_MAEN,legend_plot2 ,legend_plot, ncol = 3,nrows = 3, rel_heights = c(1,1,0.1))

