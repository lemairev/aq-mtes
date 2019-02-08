library(sf)

# produce just the contour of france
indir <- "/home/developer/data/map/france/reg/"
oudir <- "/home/developer/data/map/france/fr/"

shp_fr  <- st_read(indir, "regions-20180101") %>%
  dplyr::filter(!code_insee %in% c(paste0("0", 1:4), "06")) # rm domtom

shp_frc <- st_union(shp_fr)

shp_frcs <- rmapshaper::ms_simplify(shp_frc, keep = 0.01)

st_write(shp_frc, dsn = oudir, layer = "fr_contour_lonlat",
         driver = "ESRI Shapefile")

oudir <- "/home/developer/data/map/france/fr_simplified/"
if (dir.exists(oudir)) dir.create(oudir)
st_write(shp_frcs, dsn = oudir, layer = "fr_contour_lonlat_simplified",
         driver = "ESRI Shapefile")

# produce a simplified version of the dept map (faster to load & draw for shiny)
# and remove domtom
indir <- "/home/developer/data/map/france/dep/"
oudir <- "/home/developer/data/map/france/dep_simple/"

if (dir.exists(oudir)) dir.create(oudir)

shp_fr  <- st_read(indir, "departements-20180101") %>% 
  dplyr::filter(!code_insee %in% 971:976) # rm domtom

shp_frc <- rmapshaper::ms_simplify(shp_fr, keep = 0.01)

st_write(shp_frc, dsn = oudir, layer = "departements-20180101_simplified", 
         driver = "ESRI Shapefile", delete_dsn = T)
