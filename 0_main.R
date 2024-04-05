rm(list = ls())
gc()

# Packages
load.lib <- c("tidyverse",                         # to manipulate files
              "openxlsx", "data.table", "readr",   # to read files
              "sf", "tmap",                        # to work with geodata
              "furrr",                             # to do parallel computing
              "parallel", "foreach", "doParallel",
              "arrow")                              
install.lib <- load.lib[!load.lib %in% installed.packages()]
if ("arrow" %in% install.lib) {
  install.packages('arrow', repos = c('https://apache.r-universe.dev')) 
  
  install.lib <- install.lib[str_detect(install.lib, "arrow", negate = TRUE)]
}
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib)

# Definitions
sf_use_s2()
select <- dplyr::select
read.xlsx <- openxlsx::read.xlsx
st_as_sf <- sf::st_as_sf

tmap_mode("view")

plan(multisession, workers = 6)

# define path for raw files
path_files <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/UAB/tese/ch_overpass/data/input/radares_bruto/"





source("helper_ids_speedcameras.R")
# plot
# tm_shape(helper_geo) +
#   tm_dots("lote")+
#   tm_basemap("OpenStreetMap")

# histogram to see oppening dates
# hist(helper_ids$year)

source("helper_raw_files.R")
# table(helper_zip$year)

