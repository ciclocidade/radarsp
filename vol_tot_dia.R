# definir dia de início e dia de fim
# incluir +1 e -1 dia ao intervalo 
# abrir o banco de dados
# ler o número de caracteres
# criar base a parte com os que não tem 53 caracteres
# separar os caracteres em variáveis
# filtrar para o dia de início e fim
# agrupar base em dia<>local
# agrupar base de 30 caracteres em dia<>local
# juntar à base orginal locais com volume zero
# retornar base

dia_inicio <- "20190102"
dia_fim <- "20190103"

vol_tot_dia <- function(
  dia_inicio = NULL,
  dia_fim = NULL){
  
  if (is.null(dia_inicio) | is.null(dia_fim)) {
    print("Defina dia de início e dia de fim para a busca.")
  }else{
    dia_inicio <- as.Date(dia_inicio, "%Y%m%d")-1
    dia_fim <- as.Date(dia_fim, "%Y%m%d")+1
    
    intervalo <- str_replace_all(
      as.character(
        seq.Date(
          dia_inicio, dia_fim, by = 1)), 
      "-", "")
    
    files_radares <- list.files("data/radares/", ".txt")
    
    files_to_read <- files_radares[str_detect(files_radares, intervalo)]
    
    if(length(files_to_read) == 0){
      print("Não há dados para o período especificado. \nDados vão de XXXX a XXXX")
    }else{
      df_radares <- map_df(
        paste0("data/radares/", files_to_read),
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
        hora_min  = substr(X1, 11, 16), # hora HHMMSS
        hora  = substr(X1, 11, 12), # só hora completa
        local = substr(X1, 17, 20),
        faixa = substr(X1, 21, 21),
        n_reg = substr(X1, 22, 29),
        tipo_reg  = substr(X1, 30, 30),
        placa = substr(X1, 31, 37),
        tipo_veic = substr(X1, 38, 38),
        class = substr(X1, 39, 39),
        comp  = substr(X1, 40, 42),
        vel_p = substr(X1, 43, 45),
        tocup = substr(X1, 46, 50),
        vel_m = substr(X1, 51, 53)) %>%
        dplyr::select(-X1, -nchar)
      
      df_radares <- df_radares %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)])
      
      nchar30 <- nchar30 %>% 
          # L      data   hora  loc fx   n_reg tp
          # L1 20190101 062800 6601 1 00001055 2
        mutate(
          data  = substr(X1, 3, 10),
          hora_min  = substr(X1, 11, 16), # hora HHMMSS
          hora  = substr(X1, 11, 12), # só hora completa
          local = substr(X1, 17, 20),
          faixa = substr(X1, 21, 21),
          n_reg = substr(X1, 22, 29),
          tipo_reg  = substr(X1, 30, 30)) %>% 
        dplyr::select(-X1, -nchar)
      
      nchar30 <- nchar30 %>% 
        filter(data %in% intervalo[2:(length(intervalo)-1)])
      
      by_day_loc <- df_radares %>% 
        count(data, local, name = "volume")
      
      nchar30_day_loc <- nchar30 %>% 
        count(data, local, name = "obs") %>% 
        left_join(by_day_loc, by = c("data", "local")) %>% 
        filter(is.na(volume)) %>% 
        mutate(volume = 0) %>% 
        select(-obs)
      
      by_day_loc <- by_day_loc %>% 
        rbind(nchar30_day_loc)
      
      return(by_day_loc)
    }
  }
}


by_day_loc %>% 
  group_by(data) %>% 
  arrange(volume) %>% 
  mutate(ordem = row_number()) %>%
  ungroup() %>% 
  ggplot(aes(x = ordem, y = volume))+
  geom_bar(stat="identity")+
  facet_wrap(~data)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","))+
  labs(x = "Local",
       y = "Volume") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

