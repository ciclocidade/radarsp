# definir dia de início e dia de fim
# definir intervalo entre data de início e fim
# abrir o banco de dados de volumes por hora
# separar os caracteres nas variáveis de interesse: data, local e volume
# filtrar para o dia de início e fim
# agrupar base em dia<>local, somando os volumes
# retornar base e/ou salvar

vol_tot_dia <- function(
  dia_inicio = NULL,
  dia_fim = NULL,
  path_files = NULL,
  path_save = NULL,
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
          as.Date(dia_inicio, "%Y%m%d"), 
          as.Date(dia_fim, "%Y%m%d"), 
          by = 1)), 
      "-", "")
    
    files_to_read <- list.files(
      path = path_files,
      pattern = paste(intervalo, collapse = "|"))
    
    if(length(files_to_read) == 0){
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    }else{
      df_radares <- map_df(
        paste0(path_files, files_to_read),
        readr::read_delim, delim = "other", escape_double = FALSE, 
          col_names = FALSE, trim_ws = TRUE)
      
      # id      data  hora   volume
      # 1111 20160101 00    163      39.29 8.68
      df_radares <- df_radares %>% 
        mutate(
          id = substr(X1, 1, 4),
          data  = substr(X1, 5, 12),
          volume = substr(X1, 13, 16)) %>%
        select(-X1) 
      
      df_radares <- df_radares %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)]) %>% 
        group_by(data, id) %>% 
        summarise(volume = sum(as.numeric(volume), na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(id = str_pad(id, 4, "left"), 
               data = str_pad(data, 8, "left"),
               volume = str_pad(volume, 6, "left")) # máximo 999 999 veículos no dia
      
      if(!return_df & save_df){
        df_radares <- df_radares %>% 
          ungroup() %>% 
          mutate(X1 = paste0(id, data, volume)) %>% 
          select(X1)
        
        # salvar arquivo sem delimitação de colunas
        write.table(df_radares,
                    paste0(
                      path_save,
                      ifelse(
                        dia_inicio == dia_fim, 
                        dia_inicio,
                        paste0(dia_inicio,"-", dia_fim)),
                      ".csv"),
                    row.names = FALSE, col.names = FALSE)
      }else if(return_df & save_df){
        write.table(df_radares %>% 
                      ungroup() %>% 
                      mutate(X1 = paste0(id, data, volume)) %>% 
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

path_save <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/"
path_files <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byhour/"

datas <- helper_zip %>% 
  mutate(data = paste0(year, month, day)) %>% 
  distinct(data) %>% 
  pull(data)

for(i in seq_along(datas)){
  vol_tot_dia(
    dia_inicio = datas[i],
    dia_fim = datas[i],
    path_files = path_files,
    path_save = path_save,
    return_df = FALSE,
    save_df = TRUE)
  
  gc()
  
  print(datas[i])
}
