# GBIF data cleaning and visualization

source('config.R')

### Load the saved raw GBIF downloads
g = read.csv("data/gbif_complete_cases_since1981.csv", header=TRUE)

# source previously written plotting function script
source('plotting_functions.R')

# Create a color palette for plotting
# get all species (in a fixed order)
species_levels <- sort(unique(g$scientificName))
pal_named = palfun(species_levels)

# Plot the points against the world map to check for any strange locations
world_plot() + point_plot(g)

# plot the points against a North America map
na_plot() + point_plot(g)
ggsave("plots/aedes_sierrensis_all_spp_bg.png")

# Zoom in on western North America to check that occurrence and background points overlap
na_plot(-140, -110, 30, 55) + point_plot(g)
ggsave("plots/aedes_sierrensis_all_spp_bg_westcoast.png")


# the semi-transparent plotting shows that there are several spots with many overlapping points
# we will need to sub-sample these at the next step
# inspect just the Aedes sierrensis points

na_plot() + point_plot(subset(g, scientificName=="Aedes sierrensis"))
ggsave("plots/aedes_sierrensis_occ.png")

# There is one suspicious point that is in central-east Mexico. (Actually it is two overlapping).
suspect = which(g$scientificName=="Aedes sierrensis" & g$decimalLongitude > -101)
g[suspect,]
# They were sampled in slightly different places in different years, so maybe they are real
# keep them for now, but may want to revisit this later

# In summary, for now we are keeping all the points in dataset 'g', 
# which is the full download of complete records from GBIF except Hawaii

### Now, to reduce redundancy, we will create a probability mask and subsample the points

#-------------------------------------------------------------#
#double check how many unique points in a 1000m raster         #
#-------------------------------------------------------------#

#make raster with 1000m sq grid cells
range(g$decimalLongitude)
range(g$decimalLatitude)

r <- raster(xmn = -159.6601, xmx = -53.910, ymn = 15.09, ymx = 69.1623, res = 0.0083)

#-------------------------------------------------------------#
# Double check how many unique points per 1000 m raster cell   #
#-------------------------------------------------------------#

# One point per grid cell
s  <- dismo::gridSample(
  g[g$scientificName == "Aedes sierrensis", c("decimalLongitude", "decimalLatitude")],
  r, n = 1
) # 273 obs for focal species

s0 <- dismo::gridSample(
  g[g$scientificName != "Aedes sierrensis", c("decimalLongitude", "decimalLatitude")],
  r, n = 1
) # 3450 obs for background species

#-------------------------------------------------------------#
# Create background mask using probability sampling            #
#-------------------------------------------------------------#

set.seed(4930) # to make this result reproducible
background <- g[g$scientificName != "Aedes sierrensis", ] # exclude focal species

#-----------------------------------------------------#
# Extract number per grid cell                         #
#-----------------------------------------------------#

bg_points <- background %>%
  dplyr::select(decimalLongitude, decimalLatitude) %>%
  as.matrix()

bg_longlat <- cellFromXY(r, bg_points) %>%
  as.data.frame() %>%
  cbind(background$year, background$scientificName) %>%
  mutate(count = 1) %>%
  setNames(c("cell", "year", "scientificName", "count")) %>%
  group_by(cell) %>%
  dplyr::summarize(
    count = sum(count),
    # scientificName = unique(scientificName),
    scientificName = first(scientificName),
    max_year = max(year),
    avg_year = mean(year)
  ) %>%
  arrange(desc(count)) %>%
  mutate(
    lon = xFromCell(r, cell),
    lat = yFromCell(r, cell)
  ) %>%
  dplyr::select(-cell) %>%
  filter(!is.na(lon) & !is.na(lat))

bg_mask_sf <- st_as_sf(
  bg_longlat,
  coords = c("lon", "lat"),
  agr = "constant",
  remove = FALSE,
  crs = 4326
)


# Randomly sample background points without replacement, weighted by sampling intensity
multiplier <- 2

bg_mask_weights <- bg_mask_sf %>%
  mutate(weight = count / sum(count))

bg_mask_df <- bg_mask_sf[
  sample(
    nrow(bg_mask_weights),
    size = multiplier * nrow(s),
    replace = FALSE,
    prob = bg_mask_weights$weight
  ),
]

# Format background dataset
bg_mask_df <- st_drop_geometry(bg_mask_df)
names(bg_mask_df)[c(4)] <- c("year")
bg_mask_df <- bg_mask_df[, c("scientificName", "year", "lon", "lat")]
bg_mask_df$presence <- 0
head(bg_mask_df)

# Format presence points
occ_points <- g[as.numeric(row.names(s)), c("scientificName", "year", "decimalLongitude", "decimalLatitude")]
occ_points$presence <- 1

# rename the lon and lat columns more conveniently
colnames(occ_points)[c(3:4)] <- c("lon", "lat")
occ_points = as_tibble(occ_points) 
# not sure if this is necessary but gets it in the same format as the background points
head(occ_points)

# Check the thinned background and occurrence and datasets
nrow(bg_mask_df) # 546 
nrow(occ_points) # 273 subsampled to one point per grid cell

bglist = unique(bg_mask_df$scientificName)
for (i in 1:length(bglist)){
  print(paste(bglist[i],
        nrow(subset(bg_mask_df, scientificName==bglist[i])),
        sep = " "))
}

# Final presence/background dataset
final_pass <- rbind(occ_points, bg_mask_df)
# Add row identifier for GEE
final_pass$row_code <- seq(1, nrow(final_pass), by = 1)

write.csv(final_pass, "data/aedes_sierrensis_all_spp_thinned_since1981.csv", row.names=FALSE)

final_pass = read.csv("data/aedes_sierrensis_all_spp_thinned_since1981.csv", header=TRUE)

# Plot the thinned data
na_plot() + point_plot(final_pass, lon, lat, pal = pal_named)
ggsave("plots/aedes_sierrensis_all_spp_thinned.png")


