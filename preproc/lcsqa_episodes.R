# load lib
library(tidyverse)
library(readxl)

# patth
indir <- "data/lcsqa/"

# read & clean data -------------------------------------------------------
# corresp regions 
reg_corresp <- read_rds("data/processed/corresp_region.Rds") %>% 
  select(c("code_new", "name_new", "emi_histo_name", "emi_actu_name"))

# histo: drop col not in actu file, fix date format & duplicated lines
ifile <- "episodes_15042015_14092017.xlsx"
histo <- read_xlsx(paste0(indir, ifile), col_names = TRUE, skip = 2) %>% 
  select(-c(7:9)) %>% 
  mutate_if(function(x) inherits(x, "POSIXct"), .funs = as.Date) %>% 
  distinct %>% drop_na %>%  # unique & drop_na (no need for actu, no na)
  left_join(reg_corresp, by = c("Région" = "emi_histo_name")) %>% # add new names
  select(-c("Région", "emi_actu_name"))
  
# actu: read, rm dupl lines & rename for binding 
ifile <- paste0(indir, "episodes_23092017_07122018.csv")
actu  <- read_delim(ifile, col_names = TRUE, delim = ";", 
                    locale = locale("fr", encoding = "Latin3")) %>% 
  # !! dbl the nb artifically because of emi_histo_name ==> distinct
  left_join(reg_corresp, by = c("Région" = "emi_actu_name")) %>%  
  select(-c("Région", "emi_histo_name")) %>% 
  distinct

names(actu) <- names(histo) # ensure same name

# bind & rename 
newlab <- c(date = "Date de l'épisode", orig = "Origine", pol = "Polluant",
            reg_code = "code_new", reg_name = "name_new", dept = "Département",
            seuil = "Seuil dépassé")

episode <- bind_rows(histo, actu) %>% rename(!!newlab) 

# tidy dept & seuil (probably more elegant solution exists)----------------
# tidy dept
episode <- episode %>% 
  # remove white space between same name
  mutate(
    dept = str_replace(dept, "TERRITOIRE DE BELFORT", "TERRITOIRE-DE-BELFORT"),
    dept = str_replace(dept, "LA REUNION", "LA-REUNION")
  ) %>% 
  # split dept (one by line)
  separate_rows(dept, sep = "[:blank:]{1,20}", convert = TRUE) 

# clean seuil & date  
clean_seuil <- select(episode, seuil) %>%
  separate(seuil, into = paste0("seuil",1:8), sep = "h\\ ", remove = F, 
           fill = "right") %>% # sep 
  # create clean seuil column (add h at the end if missing, 
  # take 1st split as ref cause full)
  mutate(seuil = ifelse(str_detect(seuil1, "[1-9]$"), 
                        yes = str_c(seuil1, "h"), no = seuil1)) %>% 
  select("seuil")
  
# append both 
episode <- episode %>% 
  dplyr::select(-seuil) %>%     # drop old seuil
  bind_cols(clean_seuil) %>% 
  distinct

# save 
write_rds(episode, "data/processed/episode_list.Rds")

# explications, verifications  --------------------------------------------
# extract nb of split == nb of dept in the col dept 
# assuming dept are separated by space (seems good cause dept composed
# are separated with "-" such as SEINE-ET-MARNE)
# except for TERRITOIRE DE BELFORT & LA REUNION ==> rename cause we sep after 

# separate by blank (space or tab) but could be more than one  
# i.e set 1 to 20 (judge enough large to cover the whole range)
# nb_dept <- map_dbl(episode$dept, ~ length(str_split(.x, "[:blank:]{1,20}", simplify = T)))

# seems that the same seuil is repeated for each dept in seuil (i.e if 5 dept 
# 5 same seuil) ==> separate by row the dept to tidy and take the first match
# for the column seuil (cause the other element are the same)
# nb_seuil <- map_dbl(episode$seuil, ~ str_count(.x, "[1-9]h"))

# episode %>%  
#   select(seuil) %>% 
#   map_df(~ str_count(.x, "[1-9]h")) %>% 
#   add_column(episode$seuil) %>% 
#   add_column(nb_dept) %>% View()

# first sel seuil & trimws then check if end by a number, 
# if yes append the "h" that was removed when we split by "h\\ " 
# then identify the factor to use to recode the var (distinct)
# select(episode, seuil) %>%  
#   separate_rows(seuil, sep = "h\\ ", convert = T) %>% # sep 
#   map_df(~ str_squish(.x)) %>% # in case of white space: rm them
#   map_df(~ ifelse(str_detect(.x, "[1-9]$"), yes = str_c(.x, "h"), no = .x)) %>% # add h at the end
#   distinct

# highlights the fact that it is the same
# episode[,"seuil", drop = FALSE] %>%
#   separate(col = seuil, into = paste0("seuil",1:8), sep = "h\\ ", remove = F) %>%
#   map_df(~ str_squish(.x)) %>% 
#   map_df(~ ifelse(str_detect(.x, "[1-9]$"), yes = str_c(.x, "h"), no = .x)) %>% 
#   View()

