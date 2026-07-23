# visualize variable importance, PDPs, and prediction surface

source('config.R')
source('plotting_functions.R')

# specify which model to work with
mname = "model8"

outputs <- readRDS(paste0(mname, "/model_fits/model_outputs_BOinloop_all_spp_", mname, ".rds"))

# ---------------------------
# Aggregate feature importance across folds
# ---------------------------
all_imp <- do.call(rbind, feature_importance)

# Aggregate feature importance acorss folds (by Gain)
gain_imp <- bind_rows(feature_importance) %>%
  group_by(Feature) %>%
  summarise(Gain = mean(Gain))
gain_imp
gt = data.table(gain_imp[rev(order(gain_imp$Gain)),])

# Order variables by mean importance
gt[, Feature := factor(Feature, levels = Feature[order(Gain)])]


# Plot feature importance by Gain as a barplot
ggplot(gt, aes(x = Feature, y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Feature",
    y = "Gain",
  ) +
  theme_minimal(base_size = 13)
ggsave(paste0(mname, "/plots/var_importance_gain_all_spp_", mname, ".png"))

# Plot SHAP partial dependence plots for the top 6 variables

shap_importance_by_fold <- lapply(seq_along(shap_values), function(j) {
  
  dt <- as.data.table(shap_values[[j]])
  
  # stopifnot(
  #   "variable"   %in% names(dt),
  #   "shap_value" %in% names(dt)
  # )
  
  out <- dt[
    ,
    .(mean_abs_shap = mean(abs(rfvalue), na.rm = TRUE)),
    by = variable
  ]
  
  out[, fold := j]
  setorder(out, -mean_abs_shap)
  
  return(out)
})

shap_importance_by_fold[[1]]
shap_importance_by_fold[[2]]
shap_importance_by_fold[[3]]

# all the same order

# aggregate feature importance by fold
# Combine all folds
shap_importance_all <- rbindlist(shap_importance_by_fold)

# Compute mean and sd across folds
shap_importance_summary <- shap_importance_all[
  , .(
    mean_importance = mean(mean_abs_shap),
    sd_importance   = sd(mean_abs_shap)
  ),
  by = variable
][order(-mean_importance)]

shap_importance_summary

# plot it as a barplot
# Ensure data.table
dt <- as.data.table(shap_importance_summary)

# Order variables by mean importance
dt[, variable := factor(variable, levels = variable[order(mean_importance)])]

# Bar plot with error bars
ggplot(dt, aes(x = variable, y = mean_importance)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(
      ymin = mean_importance - sd_importance,
      ymax = mean_importance + sd_importance
    ),
    width = 0.25
  ) +
  coord_flip() +
  labs(
    x = "Feature",
    y = "Mean |SHAP value| (across folds)",
    title = "Feature importance based on SHAP values"
  ) +
  theme_minimal(base_size = 13)
ggsave(paste0(mname, "/plots/var_importance_shap_all_spp_", mname, ".png"))


# --------------------------
# Plot SHAP PDPs by fold
# --------------------------
# Choose fold (e.g., fold 1)
j <- 1
top_vars <- shap_importance_summary$variable[1:6]  # top features by rfvalue

# ---------------------------
# Get training data for this fold
# ---------------------------
X_train_df <- analysis_data_v3[analysis_data_v3$fold != j, top_vars, drop = FALSE] |> as.data.frame()

# ---------------------------
# Get SHAP long table for this fold
# ---------------------------
shap_long <- shap_values[[j]]

# Make sure plotting function uses rfvalue
shap_long$shap_value <- shap_long$rfvalue

# ---------------------------
# Plot SHAP PDPs for top variables
# ---------------------------
for (var in top_vars) {
  stopifnot(var %in% shap_long$variable)  # sanity check
  
  # For binary features, smooth = FALSE
  binary_features <- c("forest", "shrub", "grass", "wetland", "crop", "urban") 
  
  r <- shap.plot.dependence(
    shap_long,
    x = var,
    smooth = !(var %in% binary_features)
  )
  
  print(r)
}

### Now let's aggregate over the folds and look at overall PDPs

# ---------------------------
# Stack all folds (keep observation-level rows)
# ---------------------------
shap_long_all <- rbindlist(lapply(shap_values, function(dt) {
  dt <- as.data.table(dt)
  dt$shap_value <- dt$rfvalue   # ensure correct column
  return(dt)
}))

# ---------------------------
# Top features
# ---------------------------
top_vars <- shap_importance_summary$variable[1:6]

# ---------------------------
# Binary features
# ---------------------------
binary_features <- c("forest", "shrub", "grass", "wetland", "crop", "urban") 

# ---------------------------
# Plot fold-robust PDPs
# ---------------------------
for (var in top_vars) {
  stopifnot(var %in% shap_long_all$variable)
  
  p <- shap.plot.dependence(
    shap_long_all,
    x = var,
    smooth = !(var %in% binary_features)
  )
  print(p)
  
  # for saving plots
  filename <- paste0(mname, "/plots/shap_pdp_", var, ".png")
  ggsave(filename = filename, plot = p, width = 6, height = 4, units = "in")
}

# plot the binary ones
for (var in binary_features){
  p <- shap.plot.dependence(
    shap_long_all,
    x = var,
    smooth = FALSE
  )
  print(p)
  # for saving plots
  filename <- paste0(mname, "/plots/shap_pdp_", var, ".png")
  ggsave(filename = filename, plot = p, width = 6, height = 4, units = "in")
}

# --------------------------
# Interaction PDPs
# --------------------------
# look at the top two variable interaction plots

var1 = "bio04"
var2 = "bio16"

shap.plot.dependence(
  shap_long_all,
  x = var1,
  color_feature = var2,
  # interaction = TRUE,
  smooth = TRUE
)


fvar = var1
all_other_vars = shap_importance_summary$variable[shap_importance_summary$variable!=fvar]
for (var in all_other_vars) {
  stopifnot(var %in% shap_long_all$variable)
  
  p <- shap.plot.dependence(
    shap_long_all,
    x = fvar,
    color_feature = var,
    smooth = TRUE
  )
  print(p)
  # for saving plots
  filename <- paste0(mname, "/plots/shap_pdp_",fvar,"intx_with_", var, ".png")
  ggsave(filename = filename, plot = p, width = 6, height = 4, units = "in")
  
}

# check correlations between a focal variable and other variables 
round(cor(
  analysis_data_v3[, feature_cols],
  use = "pairwise.complete.obs"
), 2)[fvar, ]


#-------------------------------
# Plot a prediction map
#-------------------------------
# load the saved model
obj <- readRDS(paste0(mname, "/model_fits/xgb_models_bundle_", mname, ".rds"))


model <- obj$final_model
feature_cols <- obj$feature_cols

# load the rasters
r_stack0 <- rast("rasters/SDM_predictors_NA_1km.tif") 
# Need to create this raster with both worldclim and landcover rasters in the same order as the model features
# double-check
names(r_stack0)
feature_cols # these should match exactly

# # If the names don't match exactly, change them to match, and drop any unused layers
# which(names(r_stack0)=="grassland")
# names(r_stack0)[which(names(r_stack0)=="grassland")] <- "grass"
# names(r_stack0)[which(names(r_stack0)=="cropland")] <- "crop"

setdiff(names(r_stack0), feature_cols) # which features are in the raster but not the model
setdiff(feature_cols, names(r_stack0)) # which are in the model but not the raster (should be none)

r_stack <- r_stack0[[feature_cols]]

# check that it worked
names(r_stack)
setdiff(names(r_stack), feature_cols) # check for no differences


# function to create predictions
xgb_predict_fun <- function(model, data) {
  data <- as.matrix(data)
  # data <- data[, obj$feature_cols, drop = FALSE]
  predict(model, data)
}

# Create prediction raster using saved model
stopifnot(all(obj$feature_cols %in% names(r_stack)))

pred_map <- terra::predict(
  r_stack,
  obj$final_model,
  fun = xgb_predict_fun,
  na.rm = TRUE
)

# # to reload the previously saved map, use this
# pred_map <- rast(paste0(mname, "/model_fits/predicted_presence_probability_", mname, ".tif"))


# Plot it
# png(paste0(mname, "/plots/prediction_map_", mname, ".png"), width = 800, height = 600)
par(mfrow=c(1,1))
plot(pred_map, xlim = c(-147.51487, -63.03359), ylim = c(8.980222, 65.16871))
# dev.off()

# zoom in on PNW
# png(paste0(mname, "/plots/prediction_map_", mname, "_PNW.png"), width = 600, height = 800)
plot(pred_map, xlim = c(-135,-110), ylim = c(33,55))
# dev.off()

# zoom in on MX
# png(paste0(mname, "/plots/prediction_map_", mname, "_MX.png"), width = 600, height = 800)
plot(pred_map, xlim = c(-120,-85), ylim = c(15,35))
# dev.off()

writeRaster(
  pred_map,
  paste0(mname, "/model_fits/predicted_presence_probability_", mname, ".tif"),
  overwrite = TRUE
)


# Add the points used to fit the model to the raster

# Convert raster to dataframe
pred_df <- as.data.frame(pred_map, xy = TRUE, na.rm = TRUE)

# Rename prediction column if needed
colnames(pred_df)[3] <- "pred"

final_pass = read.csv("data/aedes_sierrensis_all_spp_thinned_since1981.csv", header=TRUE)
final_pass$species_group <- ifelse(
  final_pass$scientificName == "Aedes sierrensis",
  "Aedes sierrensis",
  "Other"
)

# Plot the prediction surface, with country outlines, with occurrence and background points
ggplot() +
  geom_raster(data = pred_df, aes(x = x, y = y, fill = pred)) +
  scale_fill_viridis_c(name = "Prediction") +
  
  # North America polygon
  geom_polygon(
    data = north_america,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "dark grey",
    linewidth = 0.3
  ) + 
  
  # Black outline layer (bigger)
  geom_point(
    data = final_pass,
    aes(x = lon, y = lat),
    color = "black",
    size = 3
  ) +
  
  # Inner points (smaller, colored)
  geom_point(
    data = final_pass,
    aes(x = lon, y = lat, color = species_group),
    size = 2, alpha = 0.5
  ) +
  
  scale_color_manual(
    values = c("Aedes sierrensis" = "lightblue", "Other" = "pink"),
    name = "Species"
  ) +
  
  coord_fixed(
    xlim = c(range(pred_df$x)),
    ylim = c(range(pred_df$y)),
    ratio = 1.3
  ) +
  theme_minimal()
ggsave(paste0(mname, "/plots/pred_map_points_", mname, ".png"))

