#' Set custom cache directory for censobr files
#'
#' Set custom directory for caching files from the censobr package. If users
#' want to set a custom cache directory, the function needs to be run again in
#' each new R session.
#'
#' @param path String. The path to an existing directory. It defaults to `path = NULL`,
#'        to use the default directory
#'
#' @return A message indicating the directory where censobr files are cached.
#'
#' @export
#' @family Cache data
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' # Set custom cache directory
#' tempd <- tempdir()
#' set_censobr_cache_dir(path = tempd)
#'
#' # back to default path
#' set_radares_sp_cache_dir(path = NULL)

set_radares_sp_cache_dir <- function(path = NULL) {
  
  if (!is.null(path)) {
    if (!dir.exists(path)) {stop('Caminho informado não existe.')}
    radares_sp_env$cache_dir <- path
  }
  
  
  if (is.null(path)) {
    cache_d <- paste0('radares_sp/data_release_', radares_sp_env$data_release)
    radares_sp_env$cache_dir <- tools::R_user_dir(cache_d, which = 'cache')
  }
  
  message(paste("Arquivos do pacote radares_sp serão salvos (cached) em ", radares_sp_env$cache_dir))
  
}
