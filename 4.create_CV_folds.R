# 4. Create cross-validation folds

source('config.R')
source('plotting_functions.R')

data2 = read.csv("data/aedes_all_spp_allbioclim_since1981.csv", header=TRUE)

# Create cross-validation folds
set.seed(10101)
n_folds <- 3

# Convert to sf
data2_sf <- st_as_sf(data2, coords = c("lon", "lat"), crs = 4326)
sf::sf_use_s2(TRUE)

# ---------------------------------------------------------
# STEP 1: spatial cross-validation with blockCV
# ---------------------------------------------------------

cv <- blockCV::cv_spatial(
  x = data2_sf,
  column = "presence",        # assumes 0/1 column exists
  k = n_folds,
  size = 100000,               # 200000 <-- IMPORTANT: tune this (meters)
  selection = "random",
  iteration = 5000,            # increase for better balance
  biomod2 = FALSE,
  progress = FALSE
)

# ---------------------------------------------------------
# STEP 2: extract fold assignments (robust across versions)
# ---------------------------------------------------------

if (!is.null(cv$folds_ids)) {
  
  data2$fold <- cv$folds_ids
  
} else {
  
  # fallback for older blockCV outputs
  data2$fold <- NA_integer_
  
  for (f in seq_len(n_folds)) {
    idx <- cv$folds_list[[f]][[2]]
    data2$fold[idx] <- f
  }
}

# ---------------------------------------------------------
# STEP 3: diagnostics
# ---------------------------------------------------------

table(data2$fold, data2$presence)

# check for pathological folds
if (any(table(data2$fold, data2$presence)[, "1"] == 0)) {
  warning("At least one fold has zero presences — adjust block size or increase iterations.")
}

# plot histograms of each feature to check balance
# Select feature columns (robust version)
feature_data <- data2 %>%
  dplyr::select(
    -any_of(c(
      "scientificName", "year", "lon", "lat",
      "row_code", "system.index", "presence", "geo"
    ))
  ) %>%
  mutate(fold = factor(fold))

# Long format
feature_long <- feature_data %>%
  tidyr::pivot_longer(
    cols = -fold,
    names_to = "feature",
    values_to = "value"
  )

# Plot histograms
ggplot(feature_long, aes(x = value, fill = fold)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
  facet_wrap(~ feature, scales = "free") +
  theme_bw() +
  labs(x = NULL, y = "Count", fill = "Fold")

fold_scaled <- feature_long %>%
  group_by(feature) %>%
  mutate(z = scale(value)) %>%
  ungroup()

# explore covariate space among folds
fold_cov_summary <- feature_long %>%
  group_by(fold, feature) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    .groups = "drop"
  )

fold_z_summary <- fold_scaled %>%
  group_by(fold, feature) %>%
  summarise(
    mean_z = mean(z, na.rm = TRUE),
    .groups = "drop"
  )

# barplot of feature variable deviations
ggplot(fold_z_summary, aes(x = feature, y = mean_z, fill = fold)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw()


# ---------------------------------------------------------
# STEP 4: spatial visualization (blockCV native)
# ---------------------------------------------------------

plot(cv)

# ---------------------------------------------------------
# STEP 5: plot it in prettier format
# ---------------------------------------------------------

foldcols <- c("red", "orange", "dodgerblue")
  # palfun(unique(data2$fold))

na_plot() +
  point_plot(
    data2,
    lon, lat,
    factor(fold),
    pal = foldcols,
    name = "fold"
  )
ggsave("plots/aedes_sierrensis_all_spp_3folds.png")

# rearrange the columns so the predictor variables are together and the geometry is at the end
analysis_data <- data2
colnames(analysis_data)
analysis_data_final = analysis_data[,c(1:26, 29:35, 28, 36, 27)]
colnames(analysis_data_final)

# save the final data
write.csv(analysis_data_final, "data/aedes_all_spp_3folds_2026-06-24.csv", row.names=FALSE)

# analysis_data_final = read.csv("data/aedes_all_spp_3folds_2026-06-24.csv", header=TRUE)