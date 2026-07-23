# plotting functions
# can be sourced in other files to make plotting easier

source('config.R')

# Color palette function
palfun = function(splist, palname){
  pal <- brewer.pal(n = length(splist), name = "Spectral")[c(1,9,6,8,2,5,3,7,4,10)]

  # name the palette so ggplot can match species -> color
  palnamed <- setNames(pal, splist)
  palnamed
  
}


# Plot a world map
world <- ne_countries(scale="medium", returnclass="sf") # world map data
# here's some ggplot code
world_map <- map_data("world")

world_plot = function(){ 
  ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),
               fill = "light grey", color = "dark grey") + 
  coord_fixed(ratio = 1.3) +
    theme_void()
}

# Plot a North America map
north_america <- world_map %>%
  filter(region %in% c("USA", "Canada", "Mexico"))
na_plot = function(xmin = -170, xmax = -50, ymin = 10, ymax = 85){
  ggplot() +
  geom_polygon(data=north_america, aes(x=long, y=lat, group=group),
               fill = "light grey", color = "dark grey") + 
  coord_fixed(xlim = c(xmin, xmax), ylim=c(ymin, ymax), ratio = 1.3) +
    theme_void()
}

# Plot the points on top of a map
point_plot <- function(dataset,
                       x = decimalLongitude,
                       y = decimalLatitude,
                       col = scientificName,
                       pal = NULL,
                       name = "Species") {
  
  # assign default palette inside the function
  if (is.null(pal)) {
    pal <- pal_named
  }
  
  bg  <- subset(dataset, scientificName != "Aedes sierrensis")
  occ <- subset(dataset, scientificName == "Aedes sierrensis")
  
  list(
    geom_point(
      data = occ,
      aes(x = {{ x }}, y = {{ y }},
          color = {{ col }}),
      size = 2, alpha = 0.7,
      inherit.aes = FALSE
    ),
    geom_point(
      data = bg,
      aes(x = {{ x }}, y = {{ y }},
          color = {{ col }}),
      size = 2, alpha = 0.7,
      inherit.aes = FALSE
    ),
    scale_color_manual(values = pal, name = name),
    theme(legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  )
}
  

# raster multipanel plotting function
plot_raster_stack <- function(raster_list,
                              ensemble_raster = NULL,
                              xlim = NULL,
                              ylim = NULL,
                              ncol = 3,
                              zlim = NULL,
                              device_file = NULL,
                              width = 3200,
                              height = 3200,
                              res = 300,
                              main_suffix = "") {
  
  rast_stack <- rast(raster_list)
  
  if (!is.null(ensemble_raster)) {
    plot_stack <- c(rast_stack, ensemble_raster)
  } else {
    plot_stack <- rast_stack
  }
  
  if (is.null(zlim)) {
    zlim <- global(plot_stack, range, na.rm = TRUE)
  }
  
  if (!is.null(device_file)) {
    png(device_file, width = width, height = height, res = res)
  }
  
  par(
    mfrow = c(ceiling(nlyr(plot_stack) / ncol), ncol),
    
    # near-zero margins
    mar = c(0, 0, 0.8, 0),
    
    # removes extra padding around plotting region
    xaxs = "i",
    yaxs = "i",
    
    # forces plots to fill region more tightly
    tcl = -0.2,
    cex.main = 1.9
  )
  
  n_models <- nlyr(rast_stack)
  
  for (i in 1:nlyr(plot_stack)) {
    
    is_ensemble <- (!is.null(ensemble_raster) && i == (n_models + 1))
    
    plot(
      plot_stack[[i]],
      xlim = xlim,
      ylim = ylim,
      zlim = zlim,
      legend = FALSE,
      axes = FALSE,
      box = FALSE,   # removes frame whitespace illusion
      main = if (is_ensemble) {
        "Ensemble"
      } else {
        names(raster_list)[i]
      }
    )
  }
  
  dev.off()
  
  invisible(plot_stack)
}
