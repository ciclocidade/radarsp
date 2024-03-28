#' Download São Paulo speed camera data
#'
#' @description
#' Download speed camera records grouped by equipment and day. 
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
#' df <- read_population(year = 2010,
#'                       show_progress = FALSE)
#'
#'
read_15min <- function(start = "2016/01/01", # string, YYYY/MM/DD
                       end = NULL,   # string, YYYY/MM/DD
                       id = NULL,    # string, XXXX
                       as_data_frame = FALSE,
                       show_progress = TRUE,
                       cache = TRUE){
  
  ### check inputs
  checkmate::assert_string(start)
  checkmate::assert_string(end, null.ok = TRUE)
  checkmate::assert_vector(id, null.ok = TRUE)     # if string, must have four characters
  checkmate::assert_logical(as_data_frame)
  
  # time interval definition
  year_first <- c(2016)
  year_last <- c(2020)
  
  if (is.null(end)) {
    end <- start
  }
  
  if (as.Date(start) > as.Date(end)) {
    stop("If 'end' was defined, it must be after 'start'. 
         If 'end' is not defined, it will be taken as the same as 'start'. 
         Date are in the format: YYYY/MM/DD"
    )
  }
  
  if (as.Date(start) < as.Date(paste0(year_first, "-01-01")) | as.Date(end) > as.Date(paste0(year_last, "-12-31"))) {
    stop(
      paste0(
        "Define valid dates between ",
        year_first, "-01-01 and ",
        year_last, "-12-31"
      )
    )
  }
  
  
  time_interval <- stringr::str_replace_all(
    as.character(
      seq.Date(
        as.Date(start), 
        as.Date(end), 
        by = 1)
    ), 
    "-", ""
  )
  
  if (length(time_interval) > 366) {
    stop(
      "You can download up to 366 days of data at once"
    )
  }
  
  ### Download url helper
  helper_ids <- download_file(
    paste0("https://github.com/...../download/",
           "helpers/",
           "ids.csv")
  )
  
  # check if IDs exist
  ids_not_found <- id[id %notin% helper_ids$local]
  
  if (length(ids_not_found) > 0) {
    stop(
      paste0(
        "Define valid IDs. IDs ",
        paste(ids_not_found, collapse = " ,"),
        " not found"
      )
    )
  }
  
  
  ### Get url
  files_url <- paste0("https://github.com/...../download/",
                      "byday/",
                      time_interval, ".parquet")
  
  ### Download with MAP
  local_file <- map(
    files_url,
    download_file,
    dir = tempdir(),
    show_progress = show_progress,
    cache = cache
  )
  
  # check if download worked
  if(is.null(local_file)) { return(NULL) }
  
  ### read data
  df <- arrow_open_dataset(local_file)
  
  ### Filter
  if (!is.null(id)) {
    df <- df %>% 
      filter(local %in% id)
  }
  
  ### output format
  if (isTRUE(as_data_frame)) { return( dplyr::collect(df) )
  } else {
    return(df)
  }
  
}