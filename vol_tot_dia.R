# definir dia de início e dia de fim
# definir intervalo entre data de início e fim
# abrir o banco de dados de volumes por hora
# separar os caracteres nas variáveis de interesse: data, local e volume
# filtrar para o dia de início e fim
# agrupar base em dia<>local, somando os volumes
# retornar base e/ou salvar

dia_inicio <- datas[1]
dia_fim <- datas[1]

vol_tot_dia <- function(
  dia_inicio = NULL,
  dia_fim = NULL,
  path_files = NULL,
  return_df = FALSE,
  save_df = TRUE){
  
  if (is.null(path_files) & !is.null(dia_inicio) & !is.null(dia_fim)) {
    print("Defina diretório para buscar arquivos.")
  } else if (is.null(dia_inicio) | is.null(dia_fim)) {
    print("Defina dia de início e dia de fim para a busca.")
  }else{
    intervalo <- str_replace_all(
      as.character(
        seq.Date(
          as.Date(dia_inicio, "%Y%m%d")-1, 
          as.Date(dia_fim, "%Y%m%d")+1, 
          by = 1)), 
      "-", "")
    
    helper_zip_aux <- helper_zip %>% 
      filter(str_detect(txt, paste(intervalo, collapse = "|")))
    
    vec_zip <- helper_zip_aux %>% 
      pull(zip)
    
    # files_radares <- list.files(path_files, ".csv")
    
    files_to_read <- helper_zip_aux %>% 
      pull(txt)
    
    # unzip files to a temporary directory
    unzip_dir <- "DATA/temp/"
    
    for (i in seq_along(vec_zip)) {
      vec_files <- unzip(paste0(path_files, vec_zip[i]), list = TRUE)$Name[str_detect(
        unzip(paste0(path_files, vec_zip[i]), list = TRUE)$Name,
        paste(files_to_read, collapse = "|"))]
      
      if (sum(file.exists(paste0(unzip_dir, vec_files[i]))) < length(vec_files)) {
        unzip(paste0(path_files, vec_zip[i]), 
              files = vec_files, 
              overwrite = TRUE,
              exdir = unzip_dir)
      }
    }
    
    if(length(files_to_read) == 0){
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    }else{
      df_radares <- map_df(
        paste0(unzip_dir, files_to_read),
        readr::read_delim, delim = "other", escape_double = FALSE, 
          col_names = FALSE, trim_ws = TRUE)
      
      # L      data hora   loc  fx n_reg    tp placa   tiv com vel tocup  vm
      # L1 20181231 185905 6605 1  00281484 0  DKZ7783 1   0   037 036   02384 000
      df_radares <- df_radares %>% 
        filter(nchar(X1) == 53) 
        mutate(
          data  = substr(X1, 3, 10),
          cod_unico = substr(X1, 17, 20),
          volume = 1)) %>%
        select(-X1) 
      
      df_radares <- df_radares %>% 
        left_join(helper_ids %>% select(cod_familia, cod_unico),
                  by = "cod_unico") %>% 
        mutate(local = ifelse(is.na(cod_familia), cod_unico, cod_familia))
      
      df_radares <- df_radares %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)]) %>% 
        group_by(data, local) %>% 
        summarise(volume = sum(volume, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(data = str_pad(data, 8, "left"), 
               local = str_pad(local, 4, "left"), 
               volume = str_pad(volume, 6, "left")) # máximo 999 999 veículos no dia
      
      if(!return_df & save_df){
        df_radares <- df_radares %>% 
          ungroup() %>% 
          mutate(X1 = paste0(data, local, volume)) %>% 
          select(X1)
        
        # salvar arquivo sem delimitação de colunas
        write.table(df_radares,
                    paste0(
                      path_to_save,
                      ifelse(
                        dia_inicio == dia_fim, 
                        dia_inicio,
                        paste0(dia_inicio,"-", dia_fim)),
                      ".csv"),
                    row.names = FALSE, col.names = FALSE)
      }else if(return_df & save_df){
        write.table(df_radares %>% 
                      ungroup() %>% 
                      mutate(X1 = paste0(data, local, volume)) %>% 
                      select(X1),
                    paste0(
                      path_to_save,
                      ifelse(
                        dia_inicio == dia_fim, 
                        dia_inicio,
                        paste0(dia_inicio,"-", dia_fim)),
                      ".csv"),
                    row.names = FALSE, col.names = FALSE)
      }else if(return_df & !save_df){
        return(df_radares)
      }
    }
  }
}

path_to_save <- "C:/Users/econaplicada.B03-030BRP/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/"
path_files <- "C:/Users/econaplicada.B03-030BRP/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byhour/"

datas <- c(paste0("2019010", c(1:9)),
           paste0("201901", c(10:31)))

for(i in seq_along(datas)){
  vol_tot_dia(
    dia_inicio = datas[i],
    dia_fim = datas[i],
    path_files = path_files,
    return_df = FALSE,
    save_df = TRUE)
  
  gc()
  
  print(datas[i])
}
