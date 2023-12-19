# definir dia de início e dia de fim
# definir intervalo entre data de início e fim
# abrir o banco de dados de volumes por hora
# separar os caracteres nas variáveis de interesse: data, local e volume
# filtrar para o dia de início e fim
# agrupar base em dia<>local, somando os volumes
# retornar base e/ou salvar

dia_inicio <- datas[3]
dia_fim <- datas[3]

loc_vol_tot_dia <- function(
  dia_inicio = NULL,
  dia_fim = NULL,
  path_files = NULL,
  return_df = FALSE,
  save_df = TRUE,
  path_to_save = NULL){
  
  if (is.null(dia_inicio) | is.null(dia_fim)) {
    print("Defina dia de início e dia de fim para a busca.")
  }else{
    dia_inicio_aux <- as.Date(dia_inicio, "%Y%m%d")
    dia_fim_aux <- as.Date(dia_fim, "%Y%m%d")
    
    intervalo <- str_replace_all(
      as.character(
        seq.Date(
          dia_inicio_aux, 
          dia_fim_aux, by = 1)), 
      "-", "")
    
    files_radares <- list.files(path_files, ".csv")
    
    files_to_read <- files_radares[str_detect(files_radares, intervalo)]
    
    if(length(files_to_read) == 0){
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    }else{
      df_radares <- map_df(
        paste0(path_files, files_to_read),
        readr::read_delim, delim = "other", escape_double = FALSE, 
        col_names = FALSE, trim_ws = TRUE)
      
      df_radares <- df_radares %>% 
        mutate(
          data  = substr(X1, 1, 8),
          local = substr(X1, 9, 12),
          volume = as.numeric(substr(X1, 13, 18))) %>%
        select(-X1)
      
      vec_locais <- unique(df_radares$local)
      
      for(loc in vec_locais){
        if (!dir.exists(paste0(path_to_save, loc))) {
          dir.create(paste0(path_to_save, loc))
        }
        
        df_aux <- df_radares %>% 
          filter(local == loc) %>% 
          mutate(data = str_pad(data, 8, "left"), #sprintf("% 8s", vec)
                 local = str_pad(local, 4, "left"), 
                 volume = str_pad(volume, 6, "left")) %>%  # máximo 999 999 veículos em 15 min)
          mutate(X1 = paste0(data, local, volume)) %>% 
          select(X1)
        
        # salvar arquivo sem delimitação de colunas
        write.table(df_aux,
                    paste0(
                      path_to_save, 
                      loc, "/", 
                      loc, "_",
                      ifelse(
                        dia_inicio == dia_fim, 
                        dia_inicio,
                        paste0(dia_inicio,"-", dia_fim)),
                      ".csv"),
                    row.names = FALSE, col.names = FALSE)
      }
    }
  }
}

path_to_save <- "C:/Users/econaplicada.B03-030BRP/Dropbox/Academico/Pacotes_R/radares_sp/DATA/loc_byday/"
path_files <- "C:/Users/econaplicada.B03-030BRP/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/"

datas <- c(paste0("2019010", c(1:9)),
           paste0("201901", c(10:31)))

for(i in seq_along(datas)){
  loc_vol_tot_dia(
    dia_inicio = datas[i],
    dia_fim = datas[i],
    path_files = path_files,
    return_df = FALSE,
    save_df = TRUE,
    path_to_save = path_to_save)
  
  gc()
  
  print(datas[i])
}



vec_locais <- list.dirs(path_to_save,
                        full.names = FALSE,
                        recursive = FALSE)
for(loc in vec_locais){
  files_to_read <- list.files(paste0(path_to_save, loc))
  
  df_loc <- map_df(
    paste0(paste0(path_to_save, loc, "/", files_to_read)),
    readr::read_delim, delim = "other", escape_double = FALSE, 
    col_names = FALSE, trim_ws = TRUE)
  
  map(paste0(paste0(path_to_save, loc, "/", files_to_read)), 
      file.remove)
  
  write.table(df_loc,
              paste0(
                path_to_save, 
                loc, "/", 
                loc, "_201901.csv"),
              row.names = FALSE, col.names = FALSE)
  
}

