set_radarsp_cache_dir <- function(path = NULL) {
  
  if (!is.null(path)) {
    if (!dir.exists(path)) {stop('Caminho informado não existe.')}
    radarsp_env$cache_dir <- path
  }
  
  
  if (is.null(path)) {
    cache_d <- paste0('radarsp/data_release_', radarsp_env$data_release)
    radarsp_env$cache_dir <- tools::R_user_dir(cache_d, which = 'cache')
  }
  
  message(paste("Arquivos do pacote 'radarsp' serão salvos (cached) em ", radarsp_env$cache_dir))
  
}
