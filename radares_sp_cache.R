#' Manage cached files from the censobr package
#'
#' @param list_files Logical. Whether to print a message with the address of all
#'        censobr data sets cached locally. Defaults to `TRUE`.
#' @param delete_file String. The file name (basename) of a censobr data set
#'        cached locally that should be deleted. Defaults to `NULL`, so that no
#'        file is deleted. If `delete_file = "all"`, then all cached censobr
#'        files are deleted.
#'
#' @return A message indicating which file exist and/or which ones have been
#'         deleted from local cache directory.
#' @export
#' @family Cache data
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' # list all files cached
#' radares_sp_cache(list_files = TRUE)
#'
#' # delete particular file
#' radares_sp_cache(delete_file = '20160101_24')
#'
radares_sp_cache <- function(list_files = TRUE,
                             delete_file = NULL){
  
  # check inputs
  checkmate::assert_logical(list_files)
  checkmate::assert_character(delete_file, null.ok = TRUE)
  
  # find / create local dir
  if (!dir.exists(radares_sp_env$cache_dir)) { dir.create(radares_sp_env$cache_dir, recursive=TRUE) }
  
  # list cached files
  files <- list.files(radares_sp_env$cache_dir, full.names = TRUE)
  
  # if wants to dele file
  # delete_file = "20160101_24.parquet"
  if (!is.null(delete_file)) {
    
    # IF file does not exist, print message
    if (!any(grepl(delete_file, files)) & delete_file != "all") {
      message(paste0("O arquivo '", delete_file, "' não está armazenado localmente (cached)."))
    }
    
    # IF file exists, delete file
    if (any(grepl(delete_file, files))) {
      f <- files[grepl(delete_file, files)]
      unlink(f, recursive = TRUE)
      message(paste0("O arquivo '", delete_file, "' foi deletado."))
    }
    
    # Delete ALL file
    if (delete_file=='all') {
      
      # delete any files from censobr, current and old data releases
      dir_above <- dirname(radares_sp_env$cache_dir)
      unlink(dir_above, recursive = TRUE)
      message(paste0("Todos os arquivos foram deletados."))
      
    }
  }
  
  # list cached files
  files <- list.files(radares_sp_env$cache_dir, full.names = TRUE)
  
  # print file names
  if(isTRUE(list_files)){
    message('No momento os arquivos armazenados localmente são:')
    message(paste0(files, collapse = '\n'))
  }
}
