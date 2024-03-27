#' Download São Paulo speed camera data
#'
#' @description
#' Download 15 min frequency speed camera records grouped by equipment. 
#'
#' @template year
#' @template month
#' @template day
#' @template start
#' @template end
#' @template id
#' @template as_data_frame
#' @template showProgress
#' @template cache
#'
#' @return An arrow `Dataset` or a `"data.frame"` object.
#' @export
#' @family Microdata
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' # return data as arrow Dataset
#' df <- read_population(year = 2010,
#'                       showProgress = FALSE)
#'
#'
read_15min <- function(year = NULL,  # numeric, YYYY
                       month = NULL, # string, MM
                       day = NULL,   # string, DD
                       start = NULL, # string, YYYY/MM/DD
                       end = NULL,   # string, YYYY/MM/DD
                       id = NULL,    # string, XXXX
                       as_data_frame = FALSE,
                       showProgress = TRUE,
                       cache = TRUE){
  
  ### check inputs
  checkmate::assert_numeric(year)                  # if string/numeric, must have four characteres
  checkmate::assert_string(month, null.ok = TRUE)) # if string, must have two characteres
  checkmate::assert_string(day, null.ok = TRUE))   # if string, must have two characteres
  checkmate::assert_vector(id, null.ok = TRUE)     # if string, must have four characteres
  checkmate::assert_logical(as_data_frame)
  checkmate::assert_string(add_labels, pattern = 'pt', null.ok = TRUE)
  
  # time interval definition
  years <- c(2016:2020)
  time_interval <- FALSE
  
  ymd <- paste0(year, month, day)
  
  if (length(ymd) > 0) {
    print(
      paste0("Using pre-defined period:",
             ifelse(!is.null(year), paste0(" year ", year, " ")),
             ifelse(!is.null(year), paste0(" month ", month, " ")),
             ifelse(!is.null(year), paste0(" day ", day, " "))
      )
  } else if (length(ymd) == 0 & (is.null(start) | is.null(end))) {
    stop(
      paste0("Error: define a year, month, day or start and end data for the time interval.\n
             Data currently only available for the years ",
             paste(years, collapse = " "))
    )
  } else if (length(ymd) == 0 & !is.null(start) & !is.null(end) & as.Date(end) < as.Date(start)) {
    stop(
      "End date must be equal or after start date. Date are in the format: YYYY/MM/DD"
      )
  } else if (length(ymd) == 0 & !is.null(start) & !is.null(end) & as.Date(end) >= as.Date(start)) {
    print("Using time interval")
    
    time_interval <- TRUE
  }
  
  if (time_interval) {
    time_interval <- 
      as.character(
        seq.Date(
          as.Date(start, "%Y%m%d"), 
          as.Date(end, "%Y%m%d"), 
          by = 1)
        )
  } else if (isFALSE(year %in% years)) { 
    stop(paste0("Error: Data currently only available for the years ",
                paste(years, collapse = " ")))
  } else if (!is.null(year) & !is.null(month) & !is.null(day)) {
    time_interval <- paste0(year, month, day)
  } else if (!is.null(year) & !is.null(month) & is.null(day)) {
    time_interval <- stringr::str_replace_all(
      as.character(
        seq.Date(
          this_month(x = paste0(year, "-", month, "-01"))$start, 
          this_month(x = paste0(year, "-", month, "-01"))$end, 
          by = 1)
      ))
  } else if (!is.null(year) & is.null(month) & is.null(day)) {
    time_interval <- stringr::str_replace_all(
      as.character(
        seq.Date(
          this_month(x = paste0(year, "-01-01"))$start, 
          this_month(x = paste0(year, "-12-01"))$end, 
          by = 1)
      ))
  }
  
  if (length(time_interval) > 366) {
    stop(
      "You can donwload up to 366 days of data at once"
    )
  }
  
  ### Download url helper
  helper <- c()
  
  
  # check if IDs exist
  
  helper_url <- helper %>% 
    filter(stringr::str_detect(txt, paste(intervalo, collapse = "|")))

  
  ### Get url
  file_url <- paste0("https://github.com/ipeaGIT/censobr/releases/download/",
                     censobr_env$data_release, "/", year, "_population_",
                     censobr_env$data_release, ".parquet")
  
  
  ### Download with MAP
  local_file <- download_file(file_url = file_url,
                              showProgress = showProgress,
                              cache = cache)
  
  if (!is.null(id)) {
    local_file <- local_file %>% 
      filter(local %in% id)
  }
  
  # check if download worked
  if(is.null(local_file)) { return(NULL) }
  
  
  ### read data
  df <- arrow_open_dataset(local_file)
  
  ### Select
  if (!is.null(columns)) { # columns <- c('V0002','V0011')
    df <- dplyr::select(df, dplyr::all_of(columns))
  }
  
  ### Add labels
  if (!is.null(add_labels)) { # add_labels = 'pt'
    df <- add_labels_population(arrw = df,
                                year = year,
                                lang = add_labels)
  }
  
  ### output format
  if (isTRUE(as_data_frame)) { return( dplyr::collect(df) )
  } else {
    return(df)
  }
  
}
