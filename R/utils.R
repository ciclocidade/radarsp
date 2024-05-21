#' Download file from url
#'
#' @param file_url String. A url.
#' @param showProgress Logical.
#' @param cache Logical.

#' @return A string to the address of the file in a tempdir
#'
#' @keywords internal
download_file <- function(file_url = file_url,
                          showProgress = show_progress,
                          cache = cache){ # nocov start
  
  # check input
  checkmate::assert_logical(showProgress)
  checkmate::assert_logical(cache)
  
  # name of local file
  file_name <- basename(file_url)
  
  # create local dir
  if (isTRUE(cache) & !dir.exists(radarsp_env$cache_dir)) { dir.create(radarsp_env$cache_dir, recursive=TRUE) }
  
  # location of local file
  local_file <- paste0(radarsp_env$cache_dir,"/",file_name)
  
  # cache message
  cache_message(local_file, cache)
  
  # If not caching, remove local file to download it again
  if (cache==FALSE & file.exists(local_file)) {
    unlink(local_file, recursive = T)
  }
  
  # has the file been downloaded already? If not, download it
  if (cache==FALSE |
      !file.exists(local_file) |
      file.info(local_file)$size == 0) {
    
    # download data
    try(silent = TRUE,
        httr::GET(url=file_url,
                  if(showProgress==TRUE){ httr::progress()},
                  httr::write_disk(local_file, overwrite = TRUE),
                  config = httr::config(ssl_verifypeer = FALSE))
    )
  }
  
  # Halt function if download failed (file must exist and be larger than 200 kb) !!!!!!!!!!!!!! VERIFICAR TAMANHO MÍNIMO PARA O MEU CASO
  if (!file.exists(local_file)) {
    message('Parece que sua conexção com a internet não está funcionando.')
    return(invisible(NULL))
    
  } else {
    return(local_file)
  }
} # nocov end



arrow_open_dataset <- function(filename){
  
  tryCatch(
    arrow::open_dataset(filename),
    error = function(e){
      msg <- paste(
        "Dados salvos localmente parecem estar corrompidos. Por favor tente baixar os dados novamente usando 'cache = FALSE'.",
        "Se você usa um processador M2 ou M3, tente reinstalar o pacote 'arrow' usando install.packages('arrow', repos = c('https://apache.r-universe.dev'))",
        sprintf("Você pode remover os dados corrompidos em 'radarsp::radarsp_cache(delete_file = \"%s\")'", filename),
        sep = "\n"
      )
      stop(msg)
    }
  )
}



cache_message <- function(local_file = parent.frame()$local_file,
                          cache = parent.frame()$cache){ # nocov start
  
  # name of local file
  file_name <- basename(local_file[1])
  dir_name <- dirname(local_file[1])
  
  ## if file already exists
  # YES cache
  if (file.exists(local_file) & isTRUE(cache)) {
    message('Lendo dados (', file_name, ') salvos localmente.')
  }
  
  # NO cache
  if (file.exists(local_file) & isFALSE(cache)) {
    message('Sobrepondo dados (', file_name, ') salvos localmente.')
  }
  
  ## if file does not exist yet
  # YES cache
  if (!file.exists(local_file) & isTRUE(cache)) {
    message(paste("Baixando os dados (", file_name, ") e salvando para usos futuros."))
  }
  
  # NO cache
  if (!file.exists(local_file) & isFALSE(cache)) {
    message(paste("Baixando dados(", file_name, "). Escolher a opção 'cache = TRUE' é fortemente recomendado para acelerar usos futuros do pacote. Se 'cache = TRUE' os dados serão salvos localmente em: ", dir_name))
  }
} # nocov end
