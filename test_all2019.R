vec_paths <- c(
  "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/",
  "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byhour/",
  "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/by15min/")

for (i in seq_along(vec_paths)) {
  vec_files <- list.files(
    vec_paths[i],
    pattern = "2019",
    full.names = FALSE)
  
  df_2019 <- map_df(
    paste0(vec_paths[i], vec_files),
    readr::read_delim, delim = "other", escape_double = FALSE, 
    col_names = FALSE, trim_ws = TRUE)
  
  write.table(df_2019,
              paste0(vec_paths[i], "all_2019.csv"),
              row.names = FALSE, col.names = FALSE)
}
