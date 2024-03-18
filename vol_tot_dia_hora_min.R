# gerar uma base por local
# salvar a base por dia ou por código
    # um arquivo para o radar 6774 para um ano todo

# definir dia de início e dia de fim
# incluir +1 e -1 dia ao intervalo 
# abrir o banco de dados
# ler o número de caracteres
# criar base a parte com os que não tem 53 caracteres
# separar os caracteres em variáveis
# filtrar para o dia de início e fim
# agrupar base em dia<>local<>hora
# agrupar base de 30 caracteres em dia<>local<>hora
# juntar à base orginal locais com volume zero
# retornar base

vol_tot_dia_hora_min <- function(
  dia_inicio = NULL,
  dia_fim = NULL,
  path_files = NULL,
  path_to_save = NULL,
  return_df = FALSE,
  save_df = TRUE){
  
  if (is.null(path_files) & !is.null(dia_inicio) & !is.null(dia_fim)) {
    print("Defina diretório para buscar arquivos.")
  } else if (is.null(dia_inicio) | is.null(dia_fim)) {
    print("Defina dia de início e dia de fim para a busca.")
  } else {
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
    
    files_to_read <- helper_zip_aux %>% 
      pull(txt)
    
    # unzip files to a temporary directory
    unzip_dir <- "DATA/temp/"
    
    for (z in seq_along(vec_zip)) {
      vec_files <- unzip(paste0(path_files, vec_zip[z]), list = TRUE)$Name[str_detect(
        unzip(paste0(path_files, vec_zip[z]), list = TRUE)$Name,
        paste(files_to_read, collapse = "|"))]
      
      vec_files <- vec_files[!file.exists(paste0(unzip_dir, vec_files))]
      
      if (length(vec_files>0)) {
        unzip(paste0(path_files, vec_zip[z]), 
              files = vec_files, 
              overwrite = TRUE,
              exdir = unzip_dir)
      }
    }

    if (length(files_to_read) == 0) {
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    } else {
      df_radares <- map_df(
        paste0(unzip_dir, files_to_read),
        readr::read_delim, delim = "other", escape_double = FALSE, 
        col_names = FALSE, trim_ws = TRUE)
      
      df_radares <- df_radares %>% 
        mutate(
          data  = substr(X1, 3, 10),                    # YYYYMMDD
          cod_unico = substr(X1, 17, 20)) %>%           # id de local   
        filter(
          data %in% intervalo[2:(length(intervalo)-1)]) %>%
        left_join(
          helper_ids %>% select(id, cod_unico),
          by = "cod_unico") %>% 
        mutate(hora  = substr(X1, 11, 12),              # só hora completa
               
               min  = substr(X1, 13, 14),               # só minuto
               min = 
                 as.numeric(min),
               min =
                 case_when(min >= 0 & min < 15 ~ "1",
                           min >=15 & min < 30 ~ "2",
                           min >=30 & min < 45 ~ "3",
                           min >=45 & min <=59 ~ "4"),
               
               sem_cod_familia = 
                 ifelse(is.na(id), 1, 0),
               id = 
                 ifelse(is.na(id), cod_unico, id))
      
      nchar30 <- df_radares %>% 
        filter(nchar(X1) < 53)
      
      df_radares <- df_radares %>% 
        filter(nchar(X1) == 53) %>% 
        # 01.02_limpar_e_padronizar... >> linha 620
        # L1 20210915 053214 6965 3 00008577 0 EBG5042 1 0 042 072 00548 000 # 53 caracteres (8 números de registro)
        # L      data   hora  loc fx   n_reg tp  placa tiv com vel tocup  vm
        mutate(
          # data  = substr(X1, 3, 10),
          # hora_min  = substr(X1, 11, 16), # hora HHMMSS
          # hora  = substr(X1, 11, 12), # só hora completa
          # min  = substr(X1, 13, 14), # só minuto
          # cod_unico = substr(X1, 17, 20),
          # faixa = substr(X1, 21, 21),
          # n_reg = substr(X1, 22, 29),
          # tipo_reg  = substr(X1, 30, 30),
          # placa = substr(X1, 31, 37),
          # tipo_veic = substr(X1, 38, 38),
          # class = substr(X1, 39, 39),
          # comp  = substr(X1, 40, 42),
    # converter velocidade de décimo de metros/segundo para km/h: (60*60/10000)
          vel_p = as.numeric(substr(X1, 43, 45))*0.36, 
          # tocup = substr(X1, 46, 50),
          # vel_m = substr(X1, 51, 53)
          ) %>%
        dplyr::select(-X1)
      
      df_radares <- df_radares %>% 
        group_by(data, id, hora, min) %>% 
        summarise(volume = n(),
                  vel_p_sd = sd(as.numeric(vel_p), na.rm = TRUE),
                  vel_p = mean(as.numeric(vel_p), na.rm = TRUE),
                  vel_p_sd = sprintf('%.2f',round(vel_p_sd, 2)),
                  vel_p = sprintf('%.2f', round(vel_p, 2)))
      
      nchar30 <- nchar30 %>% 
        count(data, id, hora, min, name = "obs") %>% 
        left_join(df_radares, by = c("data", "id", "hora", "min")) %>% 
        filter(is.na(volume)) %>% 
        mutate(volume = 0,
               vel_p = NA_character_,
               vel_p_sd = NA_character_) %>% 
        select(-obs)
      
      df_radares <- df_radares %>% 
        rbind(nchar30) %>%
        ungroup() %>% 
        mutate(id = str_pad(id, 4, "left"), 
               data = str_pad(data, 8, "left"),
               hora = str_pad(hora, 2, "left"), 
               min = str_pad(min, 1, "left"), 
               volume = str_pad(volume, 4, "left"), # máximo 9 999 veículos em 15 min
               vel_p = str_pad(as.character(vel_p), 5, "left"), # xx.xx (contando ponto)
               vel_p_sd = str_pad(as.character(vel_p_sd), 5, "left"))
      
      if(!return_df & save_df){
        df_radares <- df_radares %>% 
          ungroup() %>% 
          mutate(X1 = paste0(id, data, hora, min, volume, vel_p, vel_p_sd)) %>% 
          select(X1)
        
        nome_arquivo <- paste0(
          path_to_save,
          ifelse(
            dia_inicio == dia_fim, 
            dia_inicio,
            paste0(dia_inicio,"-", dia_fim)),
          ".csv")
        
        # salvar arquivo sem delimitação de colunas
        write.table(df_radares,
                    nome_arquivo,
                    row.names = FALSE, col.names = FALSE)
      }else if(return_df & save_df){
        write.table(df_radares %>% 
                      ungroup() %>% 
                      mutate(X1 = paste0(data, hora, local, min, volume, vel_p, vel_p_sd)) %>% 
                      select(X1),
                    nome_arquivo,
                    row.names = FALSE, col.names = FALSE)
      }else if(return_df & !save_df){
        return(df_radares)
      }
      
      gc()
    }
    
    # delete files from temporary folder
    sapply(list.files(unzip_dir, full.names = TRUE), file.remove)
  }
}


# path_to_save <- "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/by15min/"

datas <- helper_zip %>% 
  filter(year == 2020) %>% 
  mutate(data = paste0(year, month, day)) %>% 
  distinct(data) %>% 
  pull(data)

# salva um arquivo por dia
for(i in seq_along(datas)){
  vol_tot_dia_hora_min(
    dia_inicio = datas[i],
    dia_fim = datas[i],
    path_files = "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/UAB/tese/ch_overpass/data/input/radares_bruto/",
    path_to_save = "/Users/tainasouzapacheco/Library/CloudStorage/Dropbox/Academico/Pacotes_R/radares_sp/DATA/by15min/",
    return_df = FALSE,
    save_df = TRUE)
  
  print(datas[i])
}
