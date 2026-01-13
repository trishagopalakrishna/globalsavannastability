# This script compares the Olson et al 2001 and Dinnerstein et al 2017 global ecoregion maps. 

# Packages
library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(tmap)
library(ggplot2)
library(here)

##################################################### 1. Data input
global_olson <- st_read(here("Data", "StudyArea_Delineation", "OlsonBiomes", "official", "wwf_terr_ecos.shp"))
global_dinnerstein <- st_read(here("Data", "StudyArea_Delineation", "Ecoregions2017", "Ecoregions2017.shp"))

pennington2018_classification <- read_csv(here("Data", "StudyArea_Delineation", "FromPenningtonetal2018", "Classifications_RTP_CL_link_new.csv"))
sf::sf_use_s2(FALSE) # Turn off spherical geometry

##################################################### 2. Exploratory Statistics
# 2.1 Summary of ecoystem names 
n_olson <- global_olson %>% group_by(ECO_NAME) %>% 
  summarise(n= n()) %>% 
  st_drop_geometry() %>% rename(olson_name = ECO_NAME, olson_n = n)
n_dinnerstein <- global_dinnerstein %>% group_by(ECO_NAME) %>% 
  summarise(n= n()) %>% 
  st_drop_geometry() %>% rename(dinnerstein_name = ECO_NAME, dinnerstein_n = n)

# 2.2 Difference in ecoregions (non -spatial)
not_in_dinnerstein <- setdiff(n_olson$olson_name, n_dinnerstein$dinnerstein_name)
not_in_olson <- setdiff(n_dinnerstein$dinnerstein_name, n_olson$olson_name)

# 2.3 Extracting realms 
# Neotropic (NT), IndoMalayan (IM), Afrotropic (AT), Australasia (AA)
realms_olson <- global_olson %>% 
  dplyr::filter(REALM == "NT" | REALM == "IM" | REALM == "AT" | REALM == "AA" )
realms_dinnerstein <- global_dinnerstein %>% 
  dplyr::filter(REALM == "Neotropic" | REALM == "Indomalayan" | REALM == "Afrotropic" | REALM == "Australasia" )

##################################################### 3. Delineation based on multiple criteria
# Criteria -1 Considering classification from Pennington et al., 2018 in Olson map

join_realms_olson <- left_join(realms_olson, pennington2018_classification, by= join_by("ECO_ID"=="ID"))
savanna_pennington_olson <- join_realms_olson %>% filter(Biome == "S")

dinnerstein_savanna_intersect <- st_intersection(realms_dinnerstein, savanna_pennington_olson)
st_write(dinnerstein_savanna_intersect$geometry, here("Scratch", "pennington_dinerstein_savanna.shp")) #temp files

# Criteria -2 Biome_Name = Tropical & Subtropical Grasslands, Savannas & Shrublands in Dinnerstein
dinnerstein_savanna_biome <- realms_dinnerstein %>% 
  filter(BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" |
           BIOME_NAME == "Flooded Grasslands & Savannas")
st_write(dinnerstein_savanna_biome$geometry, here("Scratch", "dinerstein_savanna.shp")) #temp files
# In QGIS, for above shp, I fixed geometry using "Fix geometries" in the Search field of
# the Processing Toolbox, using default settings

# In QGIS, I then dissolved the fixed dinnerstein shp and the penington_dinnersteon_savanna.shp (unfixed)
# using Vector > Geo processing tools > Dissolve
# Lastly, I unionized the above two dissolved shps using Vector > Geo processing tools > Union

remove(n_olson, n_dinnerstein, not_in_dinnerstein, not_in_olson)
gc()
remove(global_dinnerstein, global_olson, join_realms_olson, realms_dinnerstein, realms_olson)

##################################################### 4. Simplification of features
global_tropical_savanna_extent <- st_read(here("Outputs", "studyarea_delineation", "ecoregion_wise_delineation", "tropical_savanna_extent.shp"))

unlist(lapply(1:nrow(global_tropical_savanna_extent), \(i) nrow(st_coordinates(global_tropical_savanna_extent[i,]))))
# above shp has >1000000 vertices and hence cannot be ingested as a GEE asset

x<-st_simplify(global_tropical_savanna_extent)
unlist(lapply(1:nrow(x), \(i) nrow(st_coordinates(x[i,])))) 
# still too  many vertices
st_write(x, here("Scratch", "x.shp")) #temp file

# The temp file created above was input into QGIS in where I further simplified
# features using Vector Geometry > Simplify. I retained default settings i.e.
# simplification method was Distance (Douglas- Peucker) and Tolerance set was 1.
# The output from this step was ingested into GEE


##################################################### 5. Maps to share
country_shp<- st_read(here("Data", "studyarea_delineation","ne_10m_admin_0_countries", "ne_10m_admin_0_countries.shp"))
country_shp <- country_shp %>% filter(NAME != "Antarctica")

mapping_function <- function (bounding_box){
  x_map<- tm_shape(country_shp, bbox = bounding_box)+ tm_borders() +
    tm_shape (global_tropical_savanna_extent)  + tm_fill(fill ="#339966", fill_alpha =0.6)+
    tm_layout(frame = FALSE)
  x_map
}

# bounding box defined 
global_tropics <-  st_bbox(c(xmin = 176.1, xmax = -178.9, 
                             ymax = 26.7, ymin = -64.9 ), 
                           crs = st_crs("EPSG:4326"))

ssea <- st_bbox(c( xmin = 61.3, xmax = 160.5, 
                   ymax = -17.6, ymin = 33.9), 
                crs = st_crs("EPSG:4326"))


map_global_tropical_savannas <- mapping_function(global_tropics)
tmap_save(map_global_tropical_savannas, here("Outputs", "studyarea_delineation", "ecoregion_wise_delineation", "global_tropical_savannas.png"),
          width = 1920, height = 1080)

map_sseas <- mapping_function(ssea)
tmap_save(map_sseas, here("Outputs", "studyarea_delineation", "ecoregion_wise_delineation", "ssea_tropical_savannas.png"),
          width = 800, height = 800)