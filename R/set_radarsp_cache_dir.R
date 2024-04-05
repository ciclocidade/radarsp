#' Set custom cache directory for radarsp files
#'
#' Set custom directory for caching files from the radarsp package. If users
#' want to set a custom cache directory, the function needs to be run again in
#' each new R session.
#'
#' @param path String. The path to an existing directory. It defaults to `path = NULL`,
#'        to use the default directory
#'
#' @return A message indicating the directory where radarsp files are cached.
#'
#' @export
#' @family Cache data
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' # Set custom cache directory
#' tempd <- tempdir()
#' set_radarsp_cache_dir(path = tempd)
#'
#' # back to default path
#' set_radarsp_cache_dir(path = NULL)

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
