# 3. Join thinned occurrence and background points with environmental data

source('config.R')

occ_bg = read.csv("data/aedes_sierrensis_all_spp_thinned_since1981.csv", header=TRUE)

#### Load, check, and clean environmental data from GEE


# Climate data from Bioclim
# Land cover data from NLCD dataset NALCMS
bioclim = read.csv("data/aedes_bioclim_mean_2026-06-24.csv")
landcover = read.csv("data/aedes_landcover_mode_2026-06-24.csv")

str(bioclim)
# bioclim contains all the bands
# for now, we'll keep all of them, then in the next script we can subset accordingly

# Do some visualization to check values
par(mfrow = c(2,5))
for (i in 2:20){
  hist(bioclim[,i], main = colnames(bioclim[i]))
}
par(mfrow=c(1,1))

# Note that bioclim gives scaled values for computational efficiency
# Rescale them for interpretability

tenths = which(colnames(bioclim) %in% c('bio01', 'bio02', 'bio05', 'bio06', 'bio07', 'bio08', 'bio09', 'bio10', 'bio11'))
hundredths = which(colnames(bioclim)=='bio04')

bioclim[,tenths] <- bioclim[,tenths]*0.1
bioclim[,hundredths] <- bioclim[,hundredths]*0.01


# repeat visualization to see the rescaled values
par(mfrow = c(2,5))
for (i in 2:20){
  hist(bioclim[,i], main = colnames(bioclim[i]))
}
par(mfrow=c(1,1))

# now visualize land cover data
counts = table(landcover$mode)
print(counts)
df = as.data.frame(counts)
colnames(df) = c("coverClass", "Frequency")
par(mfrow=c(1,1))
ggplot(df, aes(x = "", y = Frequency, fill = coverClass)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Land cover classes") +
  theme_void() # Use a minimal theme

# data fall into 18 categories
# most common classes:
# 1. temperate or sub-polar needleleaf forest
# 17. urban and built-up
# 15. cropland
# 14. wetland
# 10. temperate or sub-polar grassland
# 8. temperate or sub-polar shrubland
# 5. temperate or sub-polar broadleaf forest

# Some points are falling over primarily water (class 18)
# let's plot and check those points
waterpts = landcover[which(landcover$mode=="18"), 'row_code']
# plotting functions in the plotting_functions.R script
# uses the occurrence dataset occ_bg loaded above
na_plot() + point_plot(occ_bg[waterpts,], lon, lat)

# check which points they are
occ_bg[waterpts,]

# they are a mix of coastal and lake-adjacent places, some bg and some occ

### Next step is to join the environmental and occurrence data
# the datasets are called bioclim, landcover, and occ_bg
covariates <- inner_join(bioclim, landcover, by = "row_code")
# double-check that everything matched up properly
which(covariates$system.index.y != covariates$system.index.x)
which(covariates$scientificName.y != covariates$scientificName.x)
which(covariates$year.y != covariates$year.x)
which(covariates$geo.y != covariates$geo.x)

data0 <- inner_join(occ_bg, covariates, by = "row_code")
str(data0)

# We now have a lot of redundant columns, which we can drop
colnames(data0)
redun = which(colnames(data0) %in% c("presence.x", "scientificName.x", "year.x", "system.index.y", "presence.y", "scientificName.y", "year.y", ".geo.y"))
data = data0[,-redun]
colnames(data)
renamed = which(colnames(data) %in% c('system.index.x', '.geo.x', 'mode'))
colnames(data)[renamed] <- c("system.index", "geo", "landcover")
colnames(data)


#### Now we can check for correlations
pred_df <- data %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-year, -row_code, -lon, -lat, -presence, -landcover)

corr <- cor(pred_df, use = "pairwise.complete.obs")
corr

# this version has variables in order
# png("plots/corr_envt_ordered.png", width = 800, height = 800)
corrplot(corr,
         method = "ellipse",       # Use ellipses for a modern look
         type = "upper",          # Display upper triangle to reduce clutter
         # order = "hclust",        # Cluster variables by similarity
         hclust.method = "ward.D2",# Use Ward's clustering method
         tl.col = "black",        # Text label color
         tl.srt = 45,             # Text label rotation
         tl.cex = 0.7,            # Text label size
         diag = FALSE,            # Hide diagonal correlations
         addCoef.col = NULL,      # Do not show coefficients (too crowded)
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)
)
# dev.off()

data2 = data

### One-hot code the land cover variable
# The classes of interest are: 
# forest: 1-6
# shrubland: 7-8
# grassland: 9-10
# wetland: 14
# cropland: 15
# urban: 17
# water: 18

# create vectors for each class of interest
forest = rep(0, nrow(data2))
shrub  = rep(0, nrow(data2))
grass  = rep(0, nrow(data2))
wetland  = rep(0, nrow(data2))
crop  = rep(0, nrow(data2))
urban  = rep(0, nrow(data2))
water  = rep(0, nrow(data2))

# loop through and convert land cover classes to binary variables
for (i in 1:nrow(data2)){
  if (data2$landcover[i] %in% c("1", "2", "3", "4", "5", "6")) forest[i] <- 1
  if (data2$landcover[i] %in% c("7", "8")) shrub[i] <- 1
  if (data2$landcover[i] %in% c("9", "10")) grass[i] <- 1
  if (data2$landcover[i]  == "14") wetland[i] <- 1
  if (data2$landcover[i] == "15") crop[i] <- 1
  if (data2$landcover[i] == "17") urban[i] <- 1
  if (data2$landcover[i] == "18") water[i] <- 1
}

data2 = data.frame(data2, forest, shrub, grass, wetland, crop, urban, water)
colnames(data2) # sanity check

write.csv(data2, "data/aedes_all_spp_allbioclim_since1981.csv", row.names=FALSE)

# data2 = read.csv("data/aedes_all_spp_allbioclim_since1981.csv", header=TRUE)


# plot correlations between select bioclim variables and land cover
pred_df2 <- data2 %>%
  dplyr::select(bio01, bio04, bio08, bio09, bio10, bio15, bio16, bio17, bio18, forest, urban, wetland)

corr2 <- cor(pred_df2, use = "pairwise.complete.obs")
corr2

# this version has variables in order
# png("plots/corr_envt_ordered_landcover.png", width = 800, height = 800)
corrplot(corr2,
         method = "ellipse",       # Use ellipses for a modern look
         type = "upper",          # Display upper triangle to reduce clutter
         # order = "hclust",        # Cluster variables by similarity
         hclust.method = "ward.D2",# Use Ward's clustering method
         tl.col = "black",        # Text label color
         tl.srt = 45,             # Text label rotation
         tl.cex = 0.7,            # Text label size
         diag = FALSE,            # Hide diagonal correlations
         addCoef.col = NULL,      # Do not show coefficients (too crowded)
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)
)
# dev.off()
