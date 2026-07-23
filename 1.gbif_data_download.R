# 1. download occurrence records and background points from GBIF


source('config.R')

### 1-2. Download occurrence records and background points
unique_names = c("Aedes sierrensis", "Aedes communis", "Aedes sticticus", "Aedes fitchii", "Aedes excrucians")

latlong <- data.frame()

for (i in 1:length(unique_names)){
  message("Querying: ", unique_names[i])
  
  tryCatch({
    b <- data.frame(
      occ_search(
        scientificName = unique_names[i],
        continent = "north_america",
        occurrenceStatus = "PRESENT",
        hasCoordinate = TRUE,
        limit = 10500
      )$data
    )
    
    if("decimalLatitude" %in% colnames(b)){
      c <- subset(b, select = c("scientificName", "decimalLatitude", "decimalLongitude", "year", "basisOfRecord"))
      
      # enforce species name consistency
      c[1:nrow(c), 1] <- unique_names[i]
      
      # filter to records after 1980 and non-missing year
      c <- subset(c, !is.na(year) & year > 1980)
      
      latlong <- rbind(latlong, c)
    }
    
  }, error=function(e){
    message("Error for ", unique_names[i], ": ", e$message)
  })
}

# check what came out

nrow(latlong)
for (i in 1:length(unique_names)){
  print(paste(unique_names[i], 
              length(which(latlong$scientificName == unique_names[i])),
              sep = " "))
}

# check the basisOfRecord and remove inaccurate ones (MACHINE_OBSERVATION and FOSSIL_SPECIMEN)
unique(latlong$basisOfRecord)
nrow(subset(latlong, basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")))
latlong[which(latlong$basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")),]

latlong = latlong[-which(latlong$basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")),]

### For the species with many records, extract just the first 3000 from each country in CA, US, MX
most_common = c("Culex tarsalis", "Culex pipiens", 
                "Culex quinquefasciatus", "Aedes triseriatus")

country_list = c("CA", "US", "MX")

latlong2 = data.frame()

for (i in seq_along(most_common)) {
  for (j in seq_along(country_list)){
    
    message("Querying: ", most_common[i], " ", country_list[j])
    
    tryCatch({
      
      b <- occ_search(
        scientificName = most_common[i],
        country = country_list[j],  
        occurrenceStatus = "PRESENT",
        hasCoordinate = TRUE,
        year = "1981,2025",   # server-side temporal filter
        limit = 3000
      )$data
      
      if (!is.null(b) && "decimalLatitude" %in% colnames(b)) {
        
        c <- subset(b, select = c("scientificName",
                                  "decimalLatitude",
                                  "decimalLongitude",
                                  "year",
                                  "basisOfRecord"))
        
        # enforce consistent species name
        c$scientificName <- most_common[i]
        
        # safety filter (GBIF year can be messy)
        c <- subset(c, !is.na(year) & year > 1980)
        
        latlong2 <- rbind(latlong2, c)
      }
      
    }, error = function(e) {
      message("Error for ", most_common[i], " ", country_list[j], ": ", e$message)
    })
  }
}

# check what came out

nrow(latlong2)
for (i in 1:length(most_common)){
  print(paste(most_common[i], 
              length(which(latlong2$scientificName == most_common[i])),
              sep = " "))
}

# check the basisOfRecord and remove inaccurate ones (MACHINE_OBSERVATION and FOSSIL_SPECIMEN)
unique(latlong2$basisOfRecord)
nrow(subset(latlong2, basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")))
latlong2[which(latlong2$basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")),]

latlong2 = latlong2[-which(latlong2$basisOfRecord %in% c("MACHINE_OBSERVATION", "FOSSIL_SPECIMEN", "MATERIAL_CITATION")),]


# add a targeted search for Aedes vexans in WA, UT, ID to fill gaps
states_target <- c("Idaho", "Washington", "Utah")

latlong_vexans_states <- data.frame()

for (st in states_target){
  
  message("Querying: Aedes vexans ", st)
  
  tryCatch({
    
    b <- occ_search(
      scientificName = "Aedes vexans",
      stateProvince = st,
      country = "US",
      occurrenceStatus = "PRESENT",
      hasCoordinate = TRUE,
      year = "1981,2025",
      limit = 3000
    )$data
    
    if (!is.null(b) && "decimalLatitude" %in% colnames(b)) {
      
      c <- subset(b, select = c("scientificName",
                                "decimalLatitude",
                                "decimalLongitude",
                                "year", 
                                "basisOfRecord"))
      
      c$scientificName <- "Aedes vexans"
      
      c <- subset(c, !is.na(year) & year > 1980)
      
      latlong_vexans_states <- rbind(latlong_vexans_states, c)
    }
    
  }, error = function(e) {
    message("Error for Aedes vexans ", st, ": ", e$message)
  })
}

# check what came out
nrow(latlong_vexans_states)
unique(latlong_vexans_states$basisOfRecord) # none to remove here

# bind the three data frames together and save

all_latlong = rbind(latlong, latlong2, latlong_vexans_states)
nrow(all_latlong)
all_latlong = all_latlong[,!names(all_latlong)=="basisOfRecord"]


write.csv(all_latlong, file = "data/raw_gbif_downloads_since1981.csv", row.names=FALSE)

### Load the saved raw GBIF downloads
g1 = read.csv("data/raw_gbif_downloads_since1981.csv", header=TRUE)

# remove points from Hawaii
hawaii = which(g1$decimalLongitude < -150 & g1$decimalLatitude < 30)
length(hawaii)
g1 = g1[-which(g1$decimalLongitude < -150 & g1$decimalLatitude < 30),]
nrow(g1)


# check how many records were downloaded for each species
all_spp = unique(g1$scientificName)
for (i in seq_along(all_spp)){
  print(paste(all_spp[i], 
              length(which(g1$scientificName == all_spp[i])),
              sep = " "))
}

# Subset to points that have lat-long coordinates and work with this dataset
nrow(g1)
g2 = g1[complete.cases(g1),]
nrow(g2)
nrow(g1) - nrow(g2) # 0 removed

g <- g2

write.csv(g, file = "data/gbif_complete_cases_since1981.csv", row.names = FALSE)

