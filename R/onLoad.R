# package global variables
radarsp_env <- new.env(parent = emptyenv()) # nocov start

.onLoad <- function(libname, pkgname){
  
  # data release
  radarsp_env$data_release <- 'v.002'
  
  # time coverage
  radarsp_env$year_first <- c(2019)
  radarsp_env$year_last <- c(2019)
  
  # local cache dir
  cache_d <- paste0('radarsp_env/data_release_', radarsp_env$data_release)
  radarsp_env$cache_dir <- tools::R_user_dir(cache_d, which = 'cache')
  
  # get ids from speed cameras
  local_file <- paste0(
    radarsp_env$cache_dir,
    "/dic_dados.parquet")
  
  file_url <- paste0(
    "https://github.com/ciclocidade/radares_sp/releases/download/",
    radarsp_env$data_release,
    "/dic_dados.parquet")
  
  local_file <- 
    download_file(
      file_url = file_url,
      showProgress = FALSE,
      cache = TRUE)
  
  radarsp_env$id_radares <- dplyr::collect(arrow_open_dataset(local_file))$id
  
  ## delete any files from old data releases
  dir_above <- dirname(radarsp_env$cache_dir)
  all_cache <- list.files(dir_above, pattern = 'data_release',full.names = TRUE)
  old_cache <- c(all_cache[!grepl(radarsp_env$data_release, all_cache)], "dic_dados.parquet")
  if(length(old_cache)>0){ unlink(old_cache, recursive = TRUE) }

} # nocov end
