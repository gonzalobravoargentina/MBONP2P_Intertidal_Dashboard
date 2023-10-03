library(patchwork)
# Crear una lista para almacenar los gráficos generados en el bucle
plot_list <- list()

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
  
  # Agregar el gráfico a la lista
  plot_list[[length(plot_list) + 1]] <- p
}


# Crear archivos PNG para cada gráfico en plot_list
for (i in seq_along(plot_list)) {
  export(plot_list[[i]], file = paste0("grafico", i, ".png"))
}
