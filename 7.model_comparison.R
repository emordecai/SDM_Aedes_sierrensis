# Comparing fitted models

source('config.R')
source('plotting_functions.R')

# Goals
# 1. Plot prediction rasters for models together
# 2. Average rasters and calculate deviations across models, plot against occurrence and background points
# 3. Plot environmental layers in a multi-panel plot
# 4. Plot PDPs of key variables together
# 5. Plot variable importance plots grouped by variable type across models

### Set up the directory, if needed, and load models
folder_path <- "comparison_plots"

if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}


# List the models to use
model_names = paste0("model", 1:8)
bundles <- lapply(model_names, function(m){
  readRDS(file.path(
    m,
    "model_fits",
    paste0("xgb_models_bundle_", m, ".rds")
    ))
})
names(bundles) <- model_names

# check model structure
str(bundles$model1, max.level = 2)

# check the features included in each model
lapply(bundles, function(b) b$feature_cols)

# load saved prediction rasters
pred_rasters <- lapply(model_names, function(m){
  rast(paste0(m, "/model_fits/predicted_presence_probability_", m, ".tif"))
})
names(pred_rasters) <- model_names

### Calculate an ensemble mean raster and deviation rasters for each model
pred_stack <- rast(pred_rasters)
mean_raster <- app(pred_stack, mean, na.rm=TRUE)
dev_rasters <- setNames(as.list(pred_stack - mean_raster), names(pred_rasters))
sd_raster   <- app(pred_stack, sd, na.rm = TRUE)


### Plot individual model prediction rasters and ensemble mean together
# Plotting function is saved in plotting_functions.R

# plot each individual model prediction and the mean
plot_raster_stack(
  pred_rasters,
  ensemble_raster = mean_raster,
  xlim = c(-147.51487, -63.03359),
  ylim = c(8.980222, 65.16871),
  device_file = "comparison_plots/pred_raw.png"
)

# plot deviations of each model from the mean
plot_raster_stack(
  dev_rasters, 
  ensemble_raster = mean_raster,
  xlim = c(-147.51487, -63.03359),
  ylim = c(8.980222, 65.16871),
  device_file = "comparison_plots/pred_dev.png"
)

# plot ensemble mean and sd rasters together
png("comparison_plots/ensemble_summary.png", width = 2400, height = 1200, res = 300)
par(
  mfrow=c(1,2),
  mar = c(0, 0, 1.2, 0),
  xaxs = "i",
  yaxs = "i",
  tcl = -0.2,
  cex.main = 1.6
)
plot(mean_raster, 
     xlim = c(-147.51487, -63.03359),
     ylim = c(8.980222, 65.16871),
     main = "Mean"
)
plot(sd_raster, 
     xlim = c(-147.51487, -63.03359),
     ylim = c(8.980222, 65.16871),
     main = "Standard deviation"
)
par(mfrow=c(1,1))
dev.off()

### Plot feature layers
# load feature raster stack
r_stack0 <- rast("rasters/SDM_predictors_NA_1km.tif") 
names(r_stack0)

# subset to the layers used any any of the models
# list of unique layers used in models
# feature_names = sort(unique(unlist(lapply(bundles, function(b) b$feature_cols))))
# r_stack <- r_stack0[[feature_names]] #subsets to the used layers
# 
# 
# setdiff(names(r_stack0), names(r_stack)) #check which layers were dropped
# r_list <- as.list(r_stack)
# names(r_list) <- names(r_stack)
# length(r_list)

# plot all environmental layers
r_list <- as.list(r_stack0)
names(r_list) <- names(r_stack0)
length(r_list)

png("comparison_plots/environmental_vars.png", width = 3200, height = 2400, res = 300)
par(mfrow = c(7,4),
    mar = c(0, 0, 0.8, 0),
    xaxs = "i",
    yaxs = "i",
    tcl = -0.2,
    cex.main = 0.8) #1.6

for (i in 1:length(r_list)){
  plot(r_list[[i]],
       xlim = c(-147.51487, -63.03359),
       ylim = c(8.980222, 65.16871),
       main = names(r_list)[[i]],
       legend = FALSE,
       axes = FALSE,
       box = FALSE
       )
}

dev.off()
par(mfrow = c(1,1))

# Plot bioclim and land cover rasters separately
png("comparison_plots/bioclim_vars.png", width = 3000, height = 2400, res = 300)

par(
  mfrow = c(4, 5),
  mar = c(0.1, 0.1, 0.8, 0.1),
  oma = c(0, 0, 0, 0),
  xaxs = "i",
  yaxs = "i",
  tcl = -0.15,
  mgp = c(0, 0, 0),
  cex.main = 0.7
)

for (i in 1:19) {
  plot(
    r_list[[i]],
    xlim = c(-147.51487, -63.03359),
    ylim = c(8.980222, 65.16871),
    main = names(r_list)[i],
    legend = FALSE,
    axes = FALSE,
    box = FALSE
  )
}

dev.off()


png("comparison_plots/landcover_vars.png", width = 3000, height = 1200, res = 300)
par(
  mfrow = c(2, 5),
  mar = c(0.1, 0.1, 0.8, 0.1),
  oma = c(0, 0, 0, 0),
  xaxs = "i",
  yaxs = "i",
  tcl = -0.15,
  mgp = c(0, 0, 0),
  cex.main = 0.7
)

for (i in 20:26){
  plot(r_list[[i]],
       xlim = c(-147.51487, -63.03359),
       ylim = c(8.980222, 65.16871),
       main = names(r_list)[[i]],
       legend = FALSE,
       axes = FALSE,
       box = FALSE
  )
}
dev.off()

