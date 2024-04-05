dicionario_radarsp <- function(as_data_frame = TRUE,
                                  show_progress = TRUE,
                                  cache = TRUE) {
  
  ### check inputs
  checkmate::assert_logical(as_data_frame)
  checkmate::assert_logical(show_progress)
  checkmate::assert_logical(cache)
  
  if (isFALSE(as_data_frame)) {
    print(
      "Função retornará dados em formato 'parquet'."
    )
  }
  
  ### Get url
  file_url <- paste0("https://github.com/ciclocidade/radares_sp/releases/download/v.002/",
                      "dic_dados", 
                      ".parquet")
  
  ### Download
  local_file <- download_file(file_url = file_url,
                              showProgress = show_progress,
                              cache = cache,
                              as_data_frame = as_data_frame)
  
  # check if download worked
  if(is.null(local_file)) { return(NULL) }
  
  ### read data
  df <- arrow_open_dataset(local_file)
  
  ### output format
  if (isTRUE(as_data_frame)) { return( dplyr::collect(df) )
  } else {
    return(df)
  }
  
}
