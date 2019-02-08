# load lib
library(tidyverse)
library(sf)

# path 
indir <- "data/processed/"

# read & clean communes shp -----------------------------------------------
# read corresp reg to clean the names (i.e replace old with new regions)
reg_corresp <- read_rds("data/processed/corresp_region.Rds") %>% 
  select(c(reg_code = "code_new", reg_name = "name_new", "code_old"))

# shp commune ==> to data frame for aggregation
newlab <- c(reg_code_old = "INSEE_Régi", reg_name_old = "Nom_Région",
            dep_code = "INSEE_Dépa", dep_name = "Nom_Départ", 
            com_code = "INSEE_Comm", com_name = "Nom_Commun", 
            com_x    = "Abscisse_C", com_y    = "Ordonnée_C",
            area     = "Superficie", pop      = "Population")

# read shp & extract crs
fr_df     <- st_read("data/map/", layer = "communes") 
fr_df_crs <- st_crs(fr_df) 

# to data frame (sel & rename col plus tr types)
fr_df <- as_tibble(fr_df) %>% # as df
  select(!!newlab) %>% # remove some col & rename the other
  mutate_if(.predicate = is.factor, .funs = as.character) %>% # tr col types 
  left_join(reg_corresp, by = c("reg_code_old" = "code_old")) # add new regions
 
# tr lcc coordinate to lonlat (only the center com_xy)
fr_df_lonlat <- select(fr_df, com_x, com_y) %>% 
  st_as_sf(coords = c("com_x","com_y"), crs = fr_df_crs) %>%  # tr to sf pts
  st_transform(crs = 4326) %>% # reproj to lonlat (epsg 4326)
  st_coordinates() %>% # extract coord as X, Y
  as_tibble() %>% 
  set_names(c("com_x_lonlat","com_y_lonlat")) # cosmetic name

# add new coordinates to fr_df & save
fr_df <- bind_cols(fr_df, fr_df_lonlat) 
write_rds(fr_df, path = paste0(indir, "commune.Rds")) 

# read emissions to aggregate & add required geo info 
emi_df <- read_csv(paste0(indir, "emission_all_addfrac.csv")) %>% # emissions df
  left_join(fr_df, by = c("geo" = "com_code")) # add geo info to emissions df

# aggregate data ----------------------------------------------------------
# by depto
agg_dep <- aggregate(
  emis ~ dep_code + dep_name + reg_code + reg_name + pol + snap + time, 
  data = emi_df, FUN = sum
)
# by region (from depto agg)
agg_reg <- aggregate(
  emis ~ reg_code + reg_name + pol + snap + time, data = agg_dep, FUN = sum
)
# country agg
agg_fr  <- aggregate(emis ~ pol + snap + time, data = agg_reg, FUN = sum)

# save
walk2(
  .x = list(agg_dep, agg_reg, agg_fr), 
  .y = c("agg_dep", "agg_reg", "agg_fr"),
  .f = ~ write_rds(.x, path = paste0(indir, "emission_all_", .y, "_addfrac.Rds"))
)
