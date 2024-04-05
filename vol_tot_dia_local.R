# definir dia de início e dia de fim
# definir intervalo entre data de início e fim
# abrir o banco de dados de volumes por hora
# separar os caracteres nas variáveis de interesse: data, local e volume
# filtrar para o dia de início e fim
# agrupar base em dia<>local, somando os volumes
# retornar base e/ou salvar

vol_tot_dia_local <- function(
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
          id = substr(X1, 1, 4)) 
      
      vec_id <- unique(df_radares$id)
      
      for(loc in vec_id){
        if (!dir.exists(paste0(path_save, loc))) {
          dir.create(paste0(path_save, loc))
        }
        
        df_aux <- df_radares %>% 
          filter(id == loc) %>% 
          select(X1)
        
        # salvar arquivo sem delimitação de colunas
        write.table(df_aux,
                    paste0(
                      path_save, 
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

# by day
path_save <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday_id/"
path_files <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/"

vol_tot_dia_local(
  dia_inicio = "20190101",
  dia_fim = "20191231",
  path_files = path_files,
  path_save = path_save)
  
# by hour
path_save <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byhour_id/"
path_files <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byhour/"

vol_tot_dia_local(
  dia_inicio = "20190101",
  dia_fim = "20191231",
  path_files = path_files,
  path_save = path_save)

# by 15 min
path_save <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/by15min_id/"
path_files <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/by15min/"

vol_tot_dia_local(
  dia_inicio = "20190101",
  dia_fim = "20191231",
  path_files = path_files,
  path_save = path_save)
