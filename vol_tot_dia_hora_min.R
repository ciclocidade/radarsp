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

dia_inicio <- "20190102"
dia_fim <- "20190103"

path_to_save <- "C:/Users/econaplicada.B03-030BRP/Dropbox/Academico/Pacotes_R/radares_sp/DATA/byday/"

datas <- c(paste0("2019010", c(1:9)),
           paste0("201901", c(10:31)))

vol_tot_dia_hora_min <- function(
  dia_inicio = NULL,
  dia_fim = NULL,
  path_files = "data/radares/",
  return_df = FALSE,
  save_df = TRUE){
  
  if (is.null(dia_inicio) | is.null(dia_fim)) {
    print("Defina dia de início e dia de fim para a busca.")
  }else{
    dia_inicio_aux <- as.Date(dia_inicio, "%Y%m%d")-1
    dia_fim_aux <- as.Date(dia_fim, "%Y%m%d")+1
    
    intervalo <- str_replace_all(
      as.character(
        seq.Date(
          dia_inicio_aux, dia_fim_aux, by = 1)), 
      "-", "")
    
    files_radares <- list.files("data/radares/", ".txt")
    
    files_to_read <- str_subset(files_radares, intervalo)
    
    if(length(files_to_read) == 0){
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    }else{
      df_radares <- map_df(
        paste0(path_files, files_to_read),
        readr::read_delim, delim = "other", escape_double = FALSE, 
        col_names = FALSE, trim_ws = TRUE)
      
      df_radares <- df_radares %>% 
        mutate(nchar = nchar(X1))
      
      nchar30 <- df_radares %>% 
        filter(nchar < 53)
      
      df_radares <- df_radares %>% 
        filter(nchar(X1) == 53) %>% 
        # 01.02_limpar_e_padronizar... >> linha 620
        # L1 20210915 053214 6965 3 00008577 0 EBG5042 1 0 042 072 00548 000 # 53 caracteres (8 números de registro)
        # L      data   hora  loc fx   n_reg tp  placa tiv com vel tocup  vm
        mutate(
          data  = substr(X1, 3, 10),
          # hora_min  = substr(X1, 11, 16), # hora HHMMSS
          hora  = substr(X1, 11, 12), # só hora completa
          min  = substr(X1, 13, 14), # só minuto
          local = substr(X1, 17, 20),
          # faixa = substr(X1, 21, 21),
          # n_reg = substr(X1, 22, 29),
          # tipo_reg  = substr(X1, 30, 30),
          # placa = substr(X1, 31, 37),
          # tipo_veic = substr(X1, 38, 38),
          # class = substr(X1, 39, 39),
          # comp  = substr(X1, 40, 42),
          vel_p = as.numeric(substr(X1, 43, 45))/(60*60/1000), # verificar
          # tocup = substr(X1, 46, 50),
          # vel_m = substr(X1, 51, 53)
          ) %>%
        dplyr::select(-X1, -nchar)
      
      df_radares <- df_radares %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)]) %>% 
        mutate(min = as.numeric(min),
               min =
                 case_when(min >= 0 & min < 15 ~ "01",
                           min >=15 & min < 30 ~ "02",
                           min >=30 & min < 45 ~ "03",
                           min >=45 & min <=59 ~ "04"))
      
      nchar30 <- nchar30 %>% 
        # L      data   hora  loc fx   n_reg tp
        # L1 20190101 062800 6601 1 00001055 2
        mutate(
          data  = substr(X1, 3, 10),
          # hora_min  = substr(X1, 11, 16), # hora HHMMSS
          hora  = substr(X1, 11, 12), # só hora completa
          min  = substr(X1, 13, 14), # só minuto
          local = substr(X1, 17, 20),
          # faixa = substr(X1, 21, 21),
          # n_reg = substr(X1, 22, 29),
          # tipo_reg  = substr(X1, 30, 30)
          ) %>% 
        dplyr::select(-X1, -nchar)
      
      nchar30 <- nchar30 %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)]) %>% 
        mutate(min = as.numeric(min),
               min =
                 case_when(min >= 0 & min < 15 ~ "01",
                           min >=15 & min < 30 ~ "02",
                           min >=30 & min < 45 ~ "03",
                           min >=45 & min <=59 ~ "04"))
      
      df_radares <- df_radares %>% 
        group_by(data, local, hora, min) %>% 
        summarise(volume = n(),
                  vel_p_sd = sd(as.numeric(vel_p), na.rm = TRUE),
                  vel_p = mean(as.numeric(vel_p), na.rm = TRUE),
                  
                  vel_p_sd = sprintf('%.2f',round(vel_p_sd, 2)),
                  vel_p = sprintf('%.2f', round(vel_p, 2)))
      
      nchar30 <- nchar30 %>% 
        count(data, local, hora, min, name = "obs") %>% 
        left_join(df_radares, by = c("data", "local", "hora", "min")) %>% 
        filter(is.na(volume)) %>% 
        mutate(volume = 0,
               vel_p = NA_integer_,
               vel_p_sd = NA_integer_) %>% 
        select(-obs)
      
      df_radares <- df_radares %>% 
        rbind(nchar30) %>%
        ungroup() %>% 
        mutate(data = str_pad(data, 8, "left"), #sprintf("% 8s", vec)
               local = str_pad(local, 4, "left"), 
               hora = str_pad(hora, 2, "left"), 
               min = str_pad(min, 2, "left"), 
               volume = str_pad(volume, 6, "left"), 
               vel_p = str_pad(as.character(vel_p, 6, "left")), # 6 se o ponto contar
               vel_p_sd = str_pad(as.character(vel_p_sd, 6, "left")))
      
      if(!return_df & save_df){
        df_radares <- df_radares %>% 
          ungroup() %>% 
          mutate(X1 = paste0(data, local, hora, min, volume, vel_p, vel_p_sd)) %>% 
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
                      mutate(X1 = paste0(data, local, hora, min, volume, vel_p, vel_p_sd)) %>% 
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
      # converter velocidade de metros/segundo para km/h
    }
  }
}

for(i in seq_along(datas)){
  vol_tot_dia_hora_min(
    dia_inicio = datas[i],
    dia_fim = datas[i],
    path_files = "data/radares/",
    return_df = FALSE,
    save_df = TRUE)
  
  print(datas[i])
}


df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(min = factor(min, levels = c("04", "03", "02", "01"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = min))+
  geom_bar(stat="identity")+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(min = factor(min, levels = c("04", "03", "02", "01"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = min))+
  geom_bar(stat="identity", position = "fill")+
  geom_hline(yintercept = 0.25, linetype = "dashed", size = 0.25)+
  geom_hline(yintercept = 0.50, linetype = "dashed", size = 0.25)+
  geom_hline(yintercept = 0.75, linetype = "dashed", size = 0.25)+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(type = ifelse(min %in% c("01", "02"), "early", "late"),
         type = factor(type, levels = c("late", "early"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = type))+
  geom_bar(stat="identity", position = "fill")+
  geom_hline(yintercept = 0.50, linetype = "dashed", size = 0.25)+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
