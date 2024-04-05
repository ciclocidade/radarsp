#' Download São Paulo speed camera data
#'
#' @description
#' Download 15 min frequency speed camera records grouped by equipment. 
#'
#' @template start
#' @template end
#' @template id
#' @template as_data_frame
#' @template show_progress
#' @template cache
#'
#' @return An arrow `Dataset` or a `"data.frame"` object.
#' @export
#' @family Microdata
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' # return data as arrow Dataset
#' df <- read_15min(start = "2019/01/10",
#'                  as_data_frame = TRUE)

read_15min <- function(start = "2016/01/01", # string, YYYY/MM/DD
                       end = NULL,   # string, YYYY/MM/DD
                       id_to_filter = NULL,    # string, XXXX
                       as_data_frame = FALSE,
                       show_progress = TRUE,
                       cache = TRUE){         # define folder to save?
  
  ### check inputs
  checkmate::assert_string(start)
  checkmate::assert_string(end, null.ok = TRUE)
  checkmate::assert_vector(id_to_filter, null.ok = TRUE)     # if string, must have four characters
  checkmate::assert_logical(as_data_frame)
  checkmate::assert_logical(cache)
  
  # time interval definition
  year_first <- c(2016)
  year_last <- c(2020)
  
  if (isFALSE(as_data_frame)) {
    print(
      "Função retornará dados em formato 'parquet'. Se o período for maior do que 1 dia, os dados estarão em uma lista."
    )
  }
  
  if (is.null(end)) {
    end <- start
    
    print(paste0(
      "Não foi informada uma data de fim de período. Os dados serão baixados apenas para o dia definido como inicial: ",
      start))
  }
  
  if (as.Date(start, "%Y/%m/%d") > as.Date(end, "%Y/%m/%d")) {
    stop("Se a data final foi definida ('end'), ela deve ser algum dia após a data inicial ('start') no período entre ",
         year_first, "/01/01 and ",
         year_last, "/12/31. ", 
         "Datas devem ser informadas no formato: 'YYYY/MM/DD'"
         )
  }
  
  if (as.Date(start) < as.Date(paste0(year_first, "/01/01"), "%Y/%m/%d") | 
      as.Date(end) > as.Date(paste0(year_last, "/12/31"), "%Y/%m/%d")) {
    stop(
      paste0(
        "Defina datas válidas para o período entre ",
        year_first, "/01/01 and ",
        year_last, "/12/31. ",
        "Datas devem ser informadas no formato: 'YYYY/MM/DD'"
      )
    )
  }
  
  time_interval <- stringr::str_replace_all(
    as.character(
      seq.Date(
        as.Date(start, "%Y/%m/%d"), 
        as.Date(end, "%Y/%m/%d"), 
        by = 1)
      ), 
    "-", ""
    )
  
  if (length(time_interval) > 366) {
    stop(
      "Para que o download não fique muito lento, você pode baixar até 366 dias de dados de uma vez"
    )
  }
  
  if (isFALSE(is.null(id_to_filter))) {
    # check if IDs exist
    ids_not_found <- id_to_filter[id_to_filter %notin% radares_sp_env$id_radares]
    
    if (length(ids_not_found) > 0) {
      stop(
        paste0(
          "Defina identificadores (id_to_filter) válidos para os locais. IDs ",
          paste(ids_not_found, collapse = " ,"),
          " não foram encontrados. Use função 'radares_sp::data_dictionary()' para abrir tabela de referência com os ids existentes."
        )
      )
    }
  }
  
  ### Get url
  files_url <- paste0("https://github.com/ciclocidade/radares_sp/releases/download/data_v.001/",
                     time_interval, "_15.parquet")
  
  list_df <- list()
  for (i in seq_along(files_url)) {
    local_file <- download_file(file_url = files_url[i],
                                showProgress = show_progress,
                                cache = cache)
    
    # check if download worked
    if(is.null(local_file)) { 
      list_df[[i]] <- NULL } else {
        ### read data
        list_df[[i]] <- arrow_open_dataset(local_file)
      }
  }
  
  # Remove NULL objects from list
  Filter(Negate(is.null), list_df)
  
  ### Filter for selected IDs or for IDs for which we have information
  if (isFALSE(is.null(id_to_filter))) {
    for (i in 1:length(list_df)) {
      list_df[[i]] <- list_df[[i]] %>% 
        filter(id %in% id_to_filter)
    }
  } else {
    for (i in 1:length(list_df)) {
      list_df[[i]] <- list_df[[i]] %>% 
        filter(id %in% radares_sp_env$id_radares)
    }
  }
  
  ### output format
  if (isTRUE(as_data_frame)) { 
    for (i in 1:length(list_df)) {
      list_df[[i]] <- list_df[[i]] %>% 
        dplyr::collect()
    }
    return( dplyr::bind_rows(list_df) )
  } else {
    return( list_df )
  }
  
}
