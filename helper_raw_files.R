# list all "zip" files in the folder
vec_zip <- list.files(
  path_files,
  pattern = ".zip",
  full.names = FALSE)

# create 
helper_zip <- list()
for (i in seq_along(vec_zip)) {
  helper_zip[[i]] <- tibble(
    "path" = paste0(path_files, vec_zip[i]),
    "txt" = 
      unzip(paste0(path_files, vec_zip[i]), list = TRUE)$Name,
    "zip" = 
      rep(
        vec_zip[i], 
        length(unzip(paste0(path_files, vec_zip[i]),list = TRUE)$Name))
  )
}

helper_zip <- bind_rows(helper_zip)

helper_zip <- helper_zip %>% 
  mutate(area = str_sub(txt, star = 5, end = 6),
         year = str_sub(txt, start = 8, end = 11),
         month = str_sub(txt, start = 12, end = 13),
         day = str_sub(txt, start = 14, end = 15))

table(helper_zip$year, helper_zip$month)

remove(i, vec_zip)
