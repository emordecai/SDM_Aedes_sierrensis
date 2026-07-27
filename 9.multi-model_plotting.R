#### Plotting model outputs from a set of models

source('config.R')
source('plotting_functions.R')

# Read in model list
model_specs = readRDS("data/model_specs.RDS")

# Read in model outputs
models <- map_chr(model_specs, "mname")

# variable descriptions
var_desc = c(
  bio01 = "mean annual temp",
  bio02 = "mean DTR",
  bio03 = "isothermality",
  bio04 = "temp seasonality",
  bio05 = "max temp hottest month",
  bio06 = "min temp coldest month", 
  bio07 = "annual temp range",
  bio08 = "temp wettest quarter",
  bio09 = "temp driest quarter",
  bio10 = "temp warmest quarter",
  bio11 = "temp coldest quarter",
  bio12 = "annual precip",
  bio13 = "precip wettest month",
  bio14 = "precip driest month",
  bio15 = "precip seasonality",
  bio16 = "precip wettest quarter",
  bio17 = "precip driest quarter",
  bio18 = "precip warmest quarter",
  bio19 = "precip coldest quarter"
)

# ---------------------------
# Binary features
# ---------------------------
binary_features <- c(
  "forest", "shrub", "grass",
  "wetland", "crop", "urban"
)

# --------------------------
# Model nicknames
# --------------------------
model_nicknames = c(
  model1 = "1. baseline",
  model2 = "2. seasonal avgs",
  model3 = "3. extremes",
  model4 = "4. hypotheses",
  model5 = "5. kitchen sink",
  model6 = "6. seasonal avgs reduc. LC",
  model7 = "7. hypotheses reduc. LC",
  model8 = "8. hypotheses no LC"
)

#-----------------------------------
# Read model fit tables
#-----------------------------------

auc_df <- models %>%
  map_dfr(function(m){
    
    read_csv(
      file.path(
        m,
        "model_fits",
        paste0("model_auc_table_", m, ".csv")
      ),
      show_col_types = FALSE
    ) %>%
      mutate(model = m)
    
  })

head(auc_df)

# convert to long format for plotting
auc_long <- auc_df %>%
  pivot_longer(
    cols = c(AUC_In, AUC_Out),
    names_to = "metric",
    values_to = "AUC"
  )

# Compare model AUCs

ggplot(auc_long, aes(x = Fold, y = AUC, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ model, labeller = labeller(model = model_nicknames)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey60") +
  scale_fill_manual(values = c("AUC_In" = "#4C78A8", "AUC_Out" = "#5DA5A4")) +
  coord_cartesian(ylim = c(0.7, 1)) +
  theme_bw() +
  labs(x = "Fold", y = "AUC", fill = "Metric")
ggsave("comparison_plots/AUCs.png")

# Read in BO loop outputs
outputs <- models %>%
  set_names() %>%
  map(~ readRDS(
    file.path(
      .x,
      "model_fits",
      paste0("model_outputs_BOinloop_all_spp_", .x, ".rds")
    )
  ))

# read in XGBoost model bundles
xgb_bundles <- models %>%
  set_names() %>%
  map(~ readRDS(
    file.path(
      .x,
      "model_fits",
      paste0("xgb_models_bundle_", .x, ".rds")
    )
  ))

# Plot feature importance by model, using Gain and SHAP
gain_all <- map(models, ~{
  outputs <- readRDS(file.path(.x, "model_fits",
                               paste0("model_outputs_BOinloop_all_spp_", .x, ".rds")))
  bind_rows(outputs[[2]]) %>% mutate(model = .x)
})

shap_all <- map(models, ~{
  outputs <- readRDS(file.path(.x, "model_fits",
                               paste0("model_outputs_BOinloop_all_spp_", .x, ".rds")))
  bind_rows(outputs[[3]]) %>% mutate(model = .x)
})

# combine and summarize Gain across folds, then plot
gain_df <- bind_rows(gain_all)

gain_sum <- gain_df %>%
  group_by(model, Feature) %>%
  summarise(Gain = mean(Gain, na.rm = TRUE),
            .groups = "drop")


pretty_feature <- function(x) {
  base_var <- stringr::str_remove(x, "___.*$")
  ifelse(
    base_var %in% names(var_desc),
    unname(var_desc[base_var]),
    base_var
  )
}

p_gain <- gain_sum %>%
  mutate(
    Group = case_when(
      str_detect(Feature, "^bio(0[1-9]|1[0-1])$") ~ "Temperature",
      str_detect(Feature, "^bio(1[2-9])$") ~ "Precipitation",
      TRUE ~ "Land cover"
    )
  ) %>%
  ggplot(aes(
    x = reorder_within(Feature, Gain, model),
    y = Gain,
    fill = Group
  )) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(
    ~ model,
    scales = "free_y",
    labeller = labeller(model = model_nicknames)
  ) +
  scale_x_reordered(labels = pretty_feature) +
  scale_fill_manual(
    values = c(
      "Temperature" = "#A74A4A",
      "Precipitation" = "#5A7FAE",
      "Land cover" = "#5B8A4A"
    )
  ) +
  labs(
    x = NULL,
    y = "Gain"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )

p_gain

ggsave(
  "comparison_plots/gain.png",
  p_gain,
  width = 12,
  height = 8,
  dpi = 300
)

# combine and summarize SHAP across folds, then plot
shap_df <- bind_rows(shap_all)

shap_sum <- shap_df %>%
  mutate(abs_shap = abs(value)) %>%
  group_by(model, variable) %>%
  summarise(SHAP = mean(abs_shap, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Feature = variable)



p_shap <- shap_sum %>%
  mutate(
    Group = case_when(
      str_detect(Feature, "^bio(0[1-9]|1[0-1])$") ~ "Temperature",
      str_detect(Feature, "^bio(1[2-9])$") ~ "Precipitation",
      TRUE ~ "Land cover"
    )
  ) %>%
  ggplot(aes(
    x = reorder_within(Feature, SHAP, model),
    y = SHAP,
    fill = Group
  )) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(
    ~ model,
    scales = "free_y",
    labeller = labeller(model = model_nicknames)
  ) +
  scale_x_reordered(labels = pretty_feature) +
  scale_fill_manual(
    values = c(
      "Temperature" = "#A74A4A",
      "Precipitation" = "#5A7FAE",
      "Land cover" = "#5B8A4A"
    )
  ) +
  labs(
    x = NULL,
    y = "Mean |SHAP|"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )
p_shap

ggsave("comparison_plots/shap.png",
       p_shap,
       width = 12,
       height = 8,
       dpi = 300)



# Now loop through the models and plot SHAP PDPs
# This loop saves them individually in each model's plot folder


# ---------------------------
# Loop through models already loaded
# ---------------------------
for (mname in names(outputs)) {
  
  cat("Processing:", mname, "\n")
  
  # ---------------------------
  # Extract SHAP list
  # outputs[[3]] = shap_values
  # ---------------------------
  shap_values <- outputs[[mname]][[3]]
  
  # ---------------------------
  # Stack folds
  # ---------------------------
  shap_long_all <- rbindlist(
    lapply(shap_values, function(dt) {
      
      dt <- as.data.table(dt)
      
      # ensure SHAP column exists
      if ("rfvalue" %in% names(dt)) {
        dt[, shap_value := rfvalue]
      }
      
      dt
    }),
    fill = TRUE
  )
  
  # ---------------------------
  # SHAP importance ranking
  # ---------------------------
  shap_importance_summary <- shap_long_all %>%
    group_by(variable) %>%
    summarise(
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_abs_shap))
  
  # ---------------------------
  # Top 6 variables
  # ---------------------------
  top_n <- min(6, nrow(shap_importance_summary))
  top_vars <- shap_importance_summary$variable[seq_len(top_n)]
  
  # ---------------------------
  # Plot PDPs
  # ---------------------------
  for (var in top_vars) {
    
    cat("  Plotting:", var, "\n")
    
    p <- shap.plot.dependence(
      shap_long_all,
      x = var,
      smooth = !(var %in% binary_features)
    ) +
      theme_classic(base_size = 13) +
      theme(
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
      ) +
      labs(
        title = paste(mname, "-", var),
        x = var,
        y = "SHAP value"
      )
    
    print(p)
    
    # ---------------------------
    # Save plot
    # ---------------------------
    ggsave(
      filename = file.path(
        mname,
        "plots",
        paste0("shap_pdp_", var, ".png")
      ),
      plot = p,
      width = 6,
      height = 4,
      dpi = 300
    )
  }
}

# This loop combines the PDPs for the top 6 variables for each model into a single figure
# and saves them in comparison_plots


get_var_label <- function(v) {
  if (v %in% names(var_desc)) {
    return(var_desc[[v]])
  } else {
    return(v)  # binary features or anything not in dictionary
  }
}

# ---------------------------
# Loop through models
# ---------------------------
for (mname in names(outputs)) {
  
  cat("Processing:", mname, "\n")
  
  # ---------------------------
  # Extract SHAP values
  # ---------------------------
  shap_values <- outputs[[mname]][[3]]
  
  # ---------------------------
  # Stack folds
  # ---------------------------
  shap_long_all <- rbindlist(
    lapply(shap_values, function(dt) {
      
      dt <- as.data.table(dt)
      
      if ("rfvalue" %in% names(dt)) {
        dt[, shap_value := rfvalue]
      }
      
      dt
    }),
    fill = TRUE
  )
  
  # ---------------------------
  # Rank variables
  # ---------------------------
  shap_importance_summary <- shap_long_all %>%
    group_by(variable) %>%
    summarise(
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_abs_shap))
  
  # ---------------------------
  # Top 6 variables
  # ---------------------------
  top_n <- min(6, nrow(shap_importance_summary))
  top_vars <- shap_importance_summary$variable[seq_len(top_n)]
  
  # ---------------------------
  # Build plot list
  # ---------------------------
  plot_list <- list()
  
  for (var in top_vars) {
    
    cat("  Plotting:", var, "\n")
    
    p <- shap.plot.dependence(
      shap_long_all,
      x = var,
      smooth = !(var %in% binary_features)
    ) +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(color = "black", size = 16),
        axis.title = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 20)
      ) +
      labs(
        title = get_var_label(var),
        x = var,
        y = "SHAP value"
      )
    
    p$layers[[1]]$aes_params$size <- 1.3   # points (default is ~1)
    p$layers[[1]]$aes_params$colour <- "gray"
    if (length(p$layers) > 1) {
      p$layers[[2]]$aes_params$linewidth <- 1.5
      p$layers[[2]]$aes_params$colour <- "black"
    }
    
    
    plot_list[[var]] <- p
  }
  
  # ---------------------------
  # Combine into 6-panel figure
  # ---------------------------
  combined_plot <- wrap_plots(plot_list, ncol = 2) +
    plot_annotation(
      title = paste(mname, "- SHAP Partial Dependence"),
      theme = theme(
        plot.title = element_text(
          face = "bold",
          size = 24,
          hjust = 0.5
        )
      )
    )
  
  # ---------------------------
  # Save combined figure
  # ---------------------------
  ggsave(
    filename = file.path(
      "comparison_plots",
      paste0(mname, "_SHAP_PDPs.png")
    ),
    plot = combined_plot,
    width = 10,
    height = 12,
    dpi = 300
  )
}


#############################
# Now create a composite figure displaying SHAP PDPs from multiple models together
#############################

# ---------------------------
# Choose model-variable pairs for the composite plot
# ---------------------------
selected_pdps <- tibble::tribble(
  ~model,   ~variable,
  "model4", "bio08",
  "model4", "bio04",
  "model3", "bio05",
  "model3", "bio06",
  "model4", "bio16",
  "model4", "bio18",
  "model2", "bio17"
)

# ---------------------------
# Helper to extract SHAP data for one model
# ---------------------------
get_shap_long <- function(mname) {
  shap_values <- outputs[[mname]][[3]]
  
  rbindlist(
    lapply(shap_values, function(dt) {
      dt <- as.data.table(dt)
      
      if ("rfvalue" %in% names(dt)) {
        dt[, shap_value := rfvalue]
      }
      
      dt
    }),
    fill = TRUE
  )
}

# ---------------------------
# Precompute SHAP data once per model used in the figure
# ---------------------------
models_needed <- unique(selected_pdps$model)

shap_cache <- setNames(vector("list", length(models_needed)), models_needed)

for (mname in models_needed) {
  cat("Loading SHAP data for:", mname, "\n")
  shap_cache[[mname]] <- get_shap_long(mname)
}

# ---------------------------
# Build the custom panel list in the exact order you want
# ---------------------------
plot_list <- list()
panel_cols <- c(
  rep("#A32626", 4),  # Stanford cardinal
  rep("#3B6EA5", 3)   # slate blue
)


for (i in seq_len(nrow(selected_pdps))) {
  mname <- selected_pdps$model[i]
  var   <- selected_pdps$variable[i]
  
  cat("Plotting:", var, "from", mname, "\n")
  
  shap_long_all <- shap_cache[[mname]]
  
  if (!var %in% shap_long_all$variable) {
    warning("Skipping ", var, " in ", mname, " because it is not present in the data.")
    next
  }
  
  p <- shap.plot.dependence(
    shap_long_all,
    x = var,
    smooth = !(var %in% binary_features)
  ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text = element_text(color = "black", size = 16),
      axis.title = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", size = 20)
    ) +
    labs(
      title = paste0(
        "Model ", unname(model_nicknames[mname])
      ),
      x = get_var_label(var),
      y = "SHAP value"
    )
  
  # adjust aesthetics
  # Increase point size and smooth line width
  p$layers[[1]]$aes_params$size <- 1.3   # points (default is ~1)
  p$layers[[1]]$aes_params$colour <- "gray"
  if (length(p$layers) > 1) {
    p$layers[[2]]$aes_params$linewidth <- 1.5
    p$layers[[2]]$aes_params$colour <- panel_cols[i]
  }
  
  plot_list[[paste(mname, var, sep = "_")]] <- p
}

# ---------------------------
# Combine into one composite figure
# ---------------------------
combined_plot <- wrap_plots(plot_list, ncol = 4) 
combined_plot

# ---------------------------
# Save
# ---------------------------
ggsave(
  filename = file.path("comparison_plots", "selected_SHAP_PDPs.png"),
  plot = combined_plot,
  width = 16,
  height = 8,
  dpi = 300
)



# Plot prediction maps for each model

# -------------------------------
# Load predictor rasters
# -------------------------------
r_stack0 <- rast("rasters/SDM_predictors_NA_1km.tif")

# -------------------------------
# Define the plotting areas
# -------------------------------
countries <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

study_area <- countries[
  countries$name %in% c(
    "Canada",
    "United States of America",
    "Mexico"
  ),
]

study_vect <- terra::vect(study_area)

template <- r_stack0[[1]]

study_raster <- terra::rasterize(
  study_vect,
  template,
  field = 1,
  background = NA
)

# -------------------------------
# prediction function
# -------------------------------
xgb_predict_fun <- function(model, data) {
  data <- as.matrix(data)
  predict(model, data)
}


# -------------------------------
# Loop through models
# -------------------------------
for (mname in names(xgb_bundles)) {
  # for (mname in last(names(xgb_bundles))){    # to check the problem model only
  cat("Processing:", mname, "\n")
  
  # -------------------------------
  # Load model bundle
  # -------------------------------
  obj <- xgb_bundles[[mname]]
  
  model <- obj$final_model
  feature_cols <- obj$feature_cols
  
  
  # -------------------------------
  # Align raster to model features
  # -------------------------------
  r_stack <- r_stack0[[feature_cols]]
  
  r_stack <- terra::mask(r_stack, study_raster)
  
  pred_map <- terra::predict(
    r_stack,
    model,
    fun = xgb_predict_fun,
    na.rm = TRUE
  )
  
  pred_map <- terra::mask(pred_map, study_raster)
  
  # safety checks
  stopifnot(all(feature_cols %in% names(r_stack)))
  
  # -------------------------------
  # save raster 
  # -------------------------------
  writeRaster(
    pred_map,
    filename = file.path(
      "rasters",
      paste0("prediction_", mname, ".tif")
    ),
    overwrite = TRUE
  )
  
  # -------------------------------
  # plot
  # -------------------------------
  png(
    filename = file.path(
      "comparison_plots",
      paste0(mname, "_prediction_map.png")
    ),
    width = 1000,
    height = 700
  )

  plot(
    pred_map,
    main = paste("Predicted suitability:", mname),
    xlim = c(-147.51487, -63.03359),
    ylim = c(8.980222, 65.16871)
  )
  
  dev.off()
}

# Plot the model prediction rasters and ensemble model together
# load previously computed rasters
model_names <- names(xgb_bundles)

pred_rasters <- lapply(model_names, function(m){
  rast(file.path("rasters", paste0("prediction_", m, ".tif")))
})

names(pred_rasters) <- model_nicknames[model_names]

saveRDS(pred_rasters, "rasters/pred_rasters.rds")

# calculate ensemble (simple average)
pred_stack <- rast(pred_rasters)

ensemble_mean <- app(pred_stack, mean, na.rm = TRUE)
ensemble_sd <- app(pred_stack, sd, na.rm = TRUE)

# -------------------------------
# save raster 
# -------------------------------
writeRaster(
  ensemble_mean,
  filename = file.path(
    "rasters",
    paste0("ensemble_mean", ".tif")
  ),
  overwrite = TRUE
)

writeRaster(
  ensemble_mean,
  filename = file.path(
    "rasters",
    paste0("ensemble_sd", ".tif")
  ),
  overwrite = TRUE
)



# add ensemble to plotting list
pred_with_ensemble <- c(pred_rasters, list(Ensemble = ensemble_mean))

# plot models together with ensemble
plot_raster_stack(
  pred_with_ensemble,
  xlim = c(-147.51487, -63.03359),
  ylim = c(8.980222, 65.16871),
  device_file = "comparison_plots/pred_model_ensemble.png"
)

# plot ensemble mean and standard deviation
png("comparison_plots/ensemble_summary.png", width = 2400, height = 1200, res = 300)
par(
  mfrow=c(1,2),
  mar = c(0, 0, 1.2, 0),
  xaxs = "i",
  yaxs = "i",
  tcl = -0.2,
  cex.main = 1.6
)
plot(ensemble_mean, 
     xlim = c(-147.51487, -63.03359),
     ylim = c(8.980222, 65.16871),
     main = "Mean"
)
plot(ensemble_sd, 
     xlim = c(-147.51487, -63.03359),
     ylim = c(8.980222, 65.16871),
     main = "Standard deviation",
     col = colorRampPalette(
       c("#deebf7", "#f4a582", "#b2182b")
     )(100)
)
par(mfrow=c(1,1))
dev.off()
