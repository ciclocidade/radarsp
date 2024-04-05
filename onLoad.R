# package global variables
radares_sp_env <- new.env(parent = emptyenv()) # nocov start

.onLoad <- function(libname, pkgname){
  
  # data release
  radares_sp_env$data_release <- 'v.001'
  
  # local cache dir
  cache_d <- paste0('radares_sp_env/data_release_', radares_sp_env$data_release)
  radares_sp_env$cache_dir <- tools::R_user_dir(cache_d, which = 'cache')
  
  # get ids from speed cameras
  local_file <- paste0(
    radares_sp_env$cache_dir,
    "/dic_dados.parquet")
  
  file_url <- paste0(
    "https://github.com/ciclocidade/radares_sp/releases/download/data_",
    radares_sp_env$data_release,
    "/dic_dados.parquet")
  
  # local_file <- download_test(file_url = file_url)
  
  local_file <- 
    download_file(
      file_url = file_url,
      showProgress = TRUE,
      cache = TRUE)
  
  radares_sp_env$id_radares <- arrow_open_dataset(local_file) %>% 
    collect() %>% 
    pull(id)
  
  ## delete any files from old data releases
  dir_above <- dirname(radares_sp_env$cache_dir)
  all_cache <- list.files(dir_above, pattern = 'data_release',full.names = TRUE)
  old_cache <- c(all_cache[!grepl(radares_sp_env$data_release, all_cache)], "dic_dados.parquet")
  if(length(old_cache)>0){ unlink(old_cache, recursive = TRUE) }

} # nocov end
