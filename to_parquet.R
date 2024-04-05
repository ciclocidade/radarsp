# ############################################################################ #
####                           DEFINE FUNCTION                              ####
# ############################################################################ #
read_save_pqt <- function(file = NULL, 
                          path_file = NULL, 
                          path_save = NULL,
                          frequency = c("24", "60", "15")) {
  
  df <- read_delim(
    paste0(path_file, file),
    delim = "other", escape_double = FALSE, 
    col_names = FALSE, trim_ws = TRUE
  )
  
  file_name <- str_replace(file, ".csv", "")
  
  if (frequency == "24") {
    df <- df %>% 
      mutate(id = as.character(str_sub(X1, 1, 4)),
             data = as.numeric(str_sub(X1, 5, 12)),
             volume = as.numeric(str_sub(X1, 13, 18))) %>% 
      select(-X1)
  } else if (frequency == "60") {
    df <- df %>% 
      mutate(id = as.character(str_sub(X1, 1, 4)),
             data = as.numeric(str_sub(X1, 5, 12)),
             hora = as.character(str_sub(X1, 13, 14)), 
             volume = as.numeric(str_sub(X1, 15, 18)),
             vel_p = as.numeric(str_sub(X1, 19, 23)),
             vel_p_sd = as.numeric(str_sub(X1, 24, 28)))%>% 
      select(-X1)
  } else if (frequency == "15") {
    df <- df %>% 
      mutate(id = as.character(str_sub(X1, 1, 4)),
             data = as.numeric(str_sub(X1, 5, 12)),
             hora = as.character(str_sub(X1, 13, 14)), 
             min = as.numeric(str_sub(X1, 15, 15)), 
             volume = as.numeric(str_sub(X1, 16, 19)),
             vel_p = as.numeric(str_sub(X1, 20, 24)),
             vel_p_sd = as.numeric(str_sub(X1, 25, 29)))%>% 
      select(-X1)
  }
  
  write_parquet(
    df,
    sink = paste0(path_save, file_name, "_", frequency, ".parquet"),
    version = "latest"
  )
}

# ############################################################################ #
####                        SAVE FILES IN PARQUET                             ####
# ############################################################################ #
vec_frequency <- c("byday", "byhour", "by15min")
vec_frequency2 <- c("24", "60", "15")

for (i in seq_along(vec_frequency)) {
  vec_files <- list.files(
    paste0("DATA/", vec_frequency[i], "/"),
    pattern = ".csv",
    full.names = FALSE
  )
  
  map(
    vec_files,
    read_save_pqt,
    path_file = paste0("DATA/", vec_frequency[i], "/"), 
    path_save = paste0("DATA/parquet/"),
    frequency = vec_frequency2[i])
}
