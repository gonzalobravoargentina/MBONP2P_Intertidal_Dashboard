# DATA
## Data folder
Data <- "DATA"

# COVER DATA-------------
# We download from CORALNET two files, 1-species cover information and 2- metadata of photoquadrats:
# 1- metadata.csv
# 2- percent_cover.csv

library(readr)
cover <- read_csv(file.path(Data, "percent_covers.csv")) # Read cover data
metadata <- read_csv(file.path(Data, "metadata.csv")) # Read metadata

# Merge photoquadrat.metadata and photoquadrat.cover
AMP <- merge(metadata, cover, by.x = "Name", by.y = "Image name", all.x = TRUE)

# Remove original data frames from environment
rm(cover)
rm(metadata)

# Replace "CABO DOS BAHIAS" with "PIMCPA_NORTE" in the "locality" column of AMP
AMP$locality <- ifelse(AMP$locality == "CABO DOS BAHIAS", "PIMCPA_NORTE", AMP$locality)
# Replace "PIMCPA" with "PIMCPA_SUR" in the "locality" column of AMP
AMP$locality <- ifelse(AMP$locality == "PIMCPA", "PIMCPA_SUR", AMP$locality)

# Assign REGION for localities
unique(AMP$locality)
AMP$region <- ifelse(AMP$locality %in% c("MAR DEL PLATA"), "Uruguay - Buenos Aires Shelf",
                     ifelse(AMP$locality %in% c("PUNTA LOMA","ISLOTE LOBOS", "PUNTA BS AS","PIMCPA_SUR","PIMCPA_NORTE","PUNTA TOMBO"), "North Patagonian Gulfs",
                            ifelse(AMP$locality %in% c("PUNTA BUQUE", "MAKENKE", "MONTE LEON"), "Patagonian Shelf", NA)))

AMP <- AMP %>%
  select(country, region, everything())

library(lubridate)
AMP <- AMP %>%
  mutate(year = lubridate::year(Date)) %>%
  mutate(month = lubridate::month(Date)) %>%
  mutate(estacion = ifelse(month %in% c(12, 1, 2, 3, 4, 5), "Cálida", "Fría")) %>%
  select(Name, Date, year, month, estacion, everything())

### nMDS-----

# Load necessary libraries
library(vegan)
library(ggplot2)
library(parallel)
library(patchwork)
library(dplyr)

# Function to run metaMDS in parallel
run_metaMDS <- function(data, k = 2, trymax = 3, try = 3, distance = "bray", autotransform = TRUE) {
  metaMDS(data, k = k, trymax = trymax, try = try, distance = distance, autotransform = autotransform)
}

# Prepare data
AMP_low <- subset(AMP, strata == "LOWTIDE")  # Subset data for LOWTIDE
AMP_mid <- subset(AMP, strata == "MIDTIDE")  # Subset data for MIDTIDE
AMP_high <- subset(AMP, strata == "HIGHTIDE")  # Subset data for HIGHTIDE

# Select only numeric columns
numeric_columns <- function(df) {
  df[sapply(df, is.numeric)]
}

# Function to calculate the average coordinates for each locality
average_nmds <- function(nmds, data, locality_col) {
  nmds_data <- data.frame(nmds$points, locality = locality_col)
  nmds_avg <- nmds_data %>%
    group_by(locality) %>%
    summarize(across(starts_with("MDS"), mean, .names = "avg_{col}"))
  return(nmds_avg)
}

# Define a specific color palette
locality_levels <- unique(c(as.character(AMP_low$locality), as.character(AMP_mid$locality), as.character(AMP_high$locality)))
palette <- setNames(RColorBrewer::brewer.pal(min(length(locality_levels), 12), "Set3"), locality_levels)

# Create plots
create_avg_nMDS_plot <- function(avg_data, nmds, title) {
  ggplot(avg_data, aes(x = avg_MDS1, y = avg_MDS2, color = locality)) +
    geom_point(size = 3) +  # Plot points
    geom_text(aes(label = locality), hjust = 0.4, vjust = 1.8) +  # Add locality labels to points
    stat_ellipse(type = 't', size = 1) +  # Add ellipses
    theme_bw() +
    theme(
      legend.position = "none",  # Remove legend
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    ) +
    annotate("text", x = max(avg_data$avg_MDS1) - 0.5, y = min(avg_data$avg_MDS2) - 0.5, label = paste('Stress =', round(nmds$stress, 3))) +  # Add stress value
    scale_color_manual(values = palette) +  # Use the specified color palette
    ggtitle(title)  # Add title
}

# Function to run nMDS for each year and station
run_nmds_for_group <- function(group_data, group_col) {
  results <- list()
  
  for (group in unique(group_data[[group_col]])) {
    subset_data <- subset(group_data, group_data[[group_col]] == group)
    AMP_low <- subset(subset_data, strata == "LOWTIDE")
    AMP_mid <- subset(subset_data, strata == "MIDTIDE")
    AMP_high <- subset(subset_data, strata == "HIGHTIDE")
    
    data_list <- list(
      list(data = numeric_columns(AMP_low[,-(1:25)]), autotransform = TRUE),
      list(data = numeric_columns(AMP_mid[,-(1:25)]), autotransform = T),
      list(data = numeric_columns(AMP_high[,-(1:25)]), autotransform = T)
    )
    
    cl <- makeCluster(detectCores() - 1)
    clusterExport(cl, c("run_metaMDS", "metaMDS", "numeric_columns"))
    clusterEvalQ(cl, library(vegan))
    
    result_list <- parLapply(cl, data_list, function(params) {
      run_metaMDS(params$data, autotransform = params$autotransform)
    })
    stopCluster(cl)
    
    nMDS_low <- result_list[[1]]
    nMDS_mid <- result_list[[2]]
    nMDS_high <- result_list[[3]]
    
    avg_low <- average_nmds(nMDS_low, AMP_low, AMP_low$locality)
    avg_mid <- average_nmds(nMDS_mid, AMP_mid, AMP_mid$locality)
    avg_high <- average_nmds(nMDS_high, AMP_high, AMP_high$locality)
    
    low_avg_plot <- create_avg_nMDS_plot(avg_low, nMDS_low, paste("Low Tide -", group))
    mid_avg_plot <- create_avg_nMDS_plot(avg_mid, nMDS_mid, paste("Mid Tide -", group))
    high_avg_plot <- create_avg_nMDS_plot(avg_high, nMDS_high, paste("High Tide -", group))
    
    low_avg_plot <- low_avg_plot + theme(legend.position = "top")
    combined_plot <- (low_avg_plot / mid_avg_plot / high_avg_plot) 
    
    results[[group]] <- combined_plot
  }
  
  return(results)
}

# Run nMDS for each year
nmds_by_year <- run_nmds_for_group(AMP, "year")

# Run nMDS for each season
nmds_by_season <- run_nmds_for_group(AMP, "estacion")

# Display the results for a specific year and season
nmds_by_year[[2024]]
nmds_by_season[["Cálida"]]
nmds_by_season[["Fría"]]
