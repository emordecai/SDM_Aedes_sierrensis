# raster_cleanup.R

# Had to directly download landcover rasters by country
# combine them here, aggregate to 1 km, calculate binary land cover variables

source('config.R')

# Load and clean the bioclim rasters
bioclim1 <- rast("rasters/worldclim_bioclim_na-file1.tif")

names(bioclim1)

# plot a couple to check
par(mfrow=c(1,1))
plot(bioclim1$bio01)
plot(bioclim1[['bio02']])


# Scale the bioclim variables correctly
# scale by 0.1: "tenths" created above
# scale by 0.01: "hundredths" created above

tenths = which(names(bioclim1) %in% c('bio01', 'bio02', 'bio05', 'bio06', 'bio07', 'bio08', 'bio09', 'bio10', 'bio11'))
hundredths = which(names(bioclim1)=='bio04')
# rescale these raster layers
for (i in 1:length(tenths)){
  bioclim1[[tenths[i]]] <- bioclim1[[tenths[i]]]*0.1
}
bioclim1[[hundredths]] <- bioclim1[[hundredths]]*0.01

# # Obtain regression coefficients previously used to create bio06anom (used in an older version of the model)
# lmcoef = readRDS("bio06anom_coef.rds")
# bio06_hat = lmcoef[1] + lmcoef[2] * bio01

# # Compute anomaly raster
# bio06anom <- bio06 - bio06_hat
# names(bio06anom) <- "bio06anom"

# Visualize the bioclim rasters 
# png("plots/bioclim_rasters_all.png", width = 800, height = 600)
par(mfrow=c(4,5))
for(i in 1:19){
  plot(bioclim1[[i]], main = names(bioclim1[[i]]))
}
# dev.copy(png, "plots/bioclim_rasters_all.png")
par(mfrow=c(1,1))

# dev.off()


# # add bio06anom to raster stack 
# bioclim_clean <- c(
#   bio01,
#   bio05,
#   bio08,
#   bio12,
#   bio15,
#   bio06anom
#   )
# names(bioclim_clean)

bioclim_clean = bioclim1
writeRaster(bioclim_clean, "rasters/bioclim_clean_alllayers.tif", overwrite=TRUE)



# # ------------------------------
# # Load country-specific land cover rasters
# # ------------------------------
# us_lc  <- rast("rasters/usa_land_cover_2020v2_30m_tif/USA_NALCMS_landcover_2020v2_30m/data/USA_NALCMS_landcover_2020v2_30m.tif")
# ca_lc  <- rast("rasters/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif")
# mx_lc  <- rast("rasters/mex_land_cover_2020v2_30m_tif/MEX_NALCMS_landcover_2020v2_30m/data/MEX_NALCMS_landcover_2020v2_30m.tif")
# 
# # # ------------------------------
# # # 2. Aggregate each to 1 km (~33 pixels)
# # # ------------------------------
# aggregate_to_1km <- function(raster) {
#   aggregate(raster, fact=33, fun=modal, na.rm=TRUE)
# }
# 
# us_lc_1km <- aggregate_to_1km(us_lc)
# ca_lc_1km <- aggregate_to_1km(ca_lc)
# mx_lc_1km <- aggregate_to_1km(mx_lc)

# # Save these rasters because they took a long time to run
# writeRaster(us_lc_1km, "rasters/us_lc_1km.tif", overwrite=TRUE)
# writeRaster(ca_lc_1km, "rasters/ca_lc_1km.tif", overwrite=TRUE)
# writeRaster(mx_lc_1km, "rasters/mx_lc_1km.tif", overwrite=TRUE)

us_lc_1km = rast("rasters/us_lc_1km.tif")
ca_lc_1km = rast("rasters/ca_lc_1km.tif")
mx_lc_1km = rast("rasters/mx_lc_1km.tif")

na_extent <- union(ext(us_lc_1km), union(ext(ca_lc_1km), ext(mx_lc_1km)))
template <- rast(ext = na_extent,
                 resolution = res(us_lc_1km),  # keep 1 km
                 crs = crs(us_lc_1km))         # keep CRS consistent

# ------------------------------
# 3. Resample each raster to the template
# ------------------------------
us_r <- resample(us_lc_1km, template, method = "near")
ca_r <- resample(ca_lc_1km, template, method = "near")
mx_r <- resample(mx_lc_1km, template, method = "near")

# Treat 0's as NAs, not true classes
us_r[us_r == 0] <- NA
ca_r[ca_r == 0] <- NA
mx_r[mx_r == 0] <- NA


# ------------------------------
# 4. Mosaic using cover() to fill NAs
# ------------------------------
lc_na <- cover(ca_r, cover(us_r, mx_r))

# ------------------------------
# Optional: inspect
# ------------------------------
lc_na
plot(lc_na)

# ------------------------------
# 1. Load your bioclim raster
# ------------------------------

bio <- rast("rasters/bioclim_clean_alllayers.tif")  # your cleaned bioclim stack created above

# ------------------------------
# 2. Define land cover classes for binary layers
# ------------------------------
forest_classes  <- 1:6
shrub_classes   <- 7:8
grass_classes   <- 9:10
wetland_classes <- 14
crop_classes    <- 15
urban_classes   <- 17
water_classes   <- 18

# ------------------------------
# 3. Convert aggregated/mosaicked landcover raster into 7 binary layers
# ------------------------------
forest <- ifel(is.na(lc_na), NA, ifel(lc_na %in% forest_classes, 1, 0))
shrub   <- ifel(is.na(lc_na), NA, ifel(lc_na %in% shrub_classes, 1, 0))
grass   <- ifel(is.na(lc_na), NA, ifel(lc_na %in% grass_classes, 1, 0))
wetland <- ifel(is.na(lc_na), NA, ifel(lc_na %in% wetland_classes, 1, 0))
crop    <- ifel(is.na(lc_na), NA, ifel(lc_na %in% crop_classes, 1, 0))
urban   <- ifel(is.na(lc_na), NA, ifel(lc_na %in% urban_classes, 1, 0))
water   <- ifel(is.na(lc_na), NA, ifel(lc_na %in% water_classes, 1, 0))

lc_stack <- c(forest, shrub, grass, wetland, crop, urban, water)
names(lc_stack) <- c("forest","shrub","grass","wetland","crop","urban","water")


# Project lc_stack to match the CRS and grid of bio
# method = "near" is critical for categorical/binary data
lc_stack_resampled <- project(lc_stack, bio, method = "near")

# Plot the reprojected landcover rasters
par(mfrow = c(2,4))
for (i in 1:length(names(lc_stack_resampled))){
  plot(lc_stack_resampled[[i]], legend = FALSE, main = names(lc_stack_resampled[[i]]))
}
par(mfrow = c(1,1))

# Now they can be combined into a single stack
combined_stack <- c(bio, lc_stack_resampled)


# ------------------------------
# 6. Save the stacked raster with both bioclim and landcover variables
# ------------------------------
writeRaster(combined_stack, "rasters/SDM_predictors_NA_1km.tif", overwrite=TRUE)
writeRaster(lc_stack_resampled, "rasters/landcover_cleaned.tif", overwrite=TRUE)
