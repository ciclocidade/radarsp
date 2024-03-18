# Agrupa a quantidade de registros de classificações de veículos por placa mês
# a mês. Requer todos os arquivos diários criados no script anterior

library('tidyverse')
library('tidylog')
library('data.table')


# Variável principal - modificar cada vez que for rodar, por lote e ano
lote_ano  <- 'L2_2018'
lote_ano2 <- str_c(str_sub(lote_ano, 1, 2), str_sub(lote_ano, 4, 7))


# Pastas de arquivos
pasta_origem  <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares7'
pasta_placas  <- sprintf('%s/03_PLACAS/PLC_%s', pasta_origem, lote_ano2)
pasta_plc_mes <- sprintf('%s/03_PLACAS/PLC_MES_%s', pasta_origem, lote_ano2)
dir.create(pasta_plc_mes,  recursive = TRUE, showWarnings = TRUE)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Abre arquivos da ṕasta e agrupa em um dataframe único conforme o regex definido
abrir_e_juntar_arquivos <- function(pasta, padrao_regex) {

  # Criar dataframe com lista de arquivos da pasta, de acordo com regex definido
  arquivos_radares <-
    list.files(pasta, pattern = padrao_regex, recursive = TRUE, full.names = TRUE) %>%
    as.data.frame() %>%
    setNames('arqs')

  # Abrir arquivos da ṕasta e agrupá-los em um dataframe único
  placas <- lapply(X = arquivos_radares, FUN = read_delim, delim = ';', col_types = "ciiii")
  placas <- placas %>% rbindlist(fill = TRUE)

}

# Agrupa todos os registros de classificação de veículos por placa
agrupar_registros <- function(df_placas) {

  df_placas <-
    df_placas %>%
    # filter(placa %in% c('AAA3054', 'AAA3245')) %>%
    group_by(placa) %>%
    summarize(moto     = sum(moto),
              passeio  = sum(passeio),
              onibus   = sum(onibus),
              caminhao = sum(caminhao))

}

# Agrupa todas as placas conforme os registros de classificação de veículos
agrupar_placas <- function(placas) {

  # Separar placas em grupos de tamanhos similares para dividir o processamento
  grupo1 <- placas %>% filter(str_starts(placa, '[A-D]'))
  grupo2 <- placas %>% filter(str_starts(placa, '[E]'))
  grupo3 <- placas %>% filter(str_starts(placa, '[F]'))
  grupo4 <- placas %>% filter(str_starts(placa, '[G-K]'))
  grupo5 <- placas %>% filter(str_starts(placa, '[L-Z]'))
  # nrow(grupo1) + nrow(grupo2) + nrow(grupo3) + nrow(grupo4) + nrow(grupo5) == nrow(placas)

  # Agrupar quantidade de registros de classificação de veículos por placa
  grupo1 <- agrupar_registros(grupo1)
  grupo2 <- agrupar_registros(grupo2)
  grupo3 <- agrupar_registros(grupo3)
  grupo4 <- agrupar_registros(grupo4)
  grupo5 <- agrupar_registros(grupo5)

  # Substitutir dataframe original de placas pelos resultados dos agrupamentos
  placas <- rbind(grupo1, grupo2, grupo3, grupo4, grupo5)

}


# ------------------------------------------------------------------------------
# Agrupar arquivos por mês
# ------------------------------------------------------------------------------
for (mes in sprintf('%02d', 1:12)) {
  # mes <- '02'

  # Gerar regex de acordo com o mês
  f_pattern <- sprintf('^PLC_%s%s[0-9]{2}.csv', lote_ano, mes)
  print(f_pattern)

  # Abrir arquivos do mês em um dataframe
  placas <- abrir_e_juntar_arquivos(pasta_placas, f_pattern)
  # head(placas, 20)

  # Agrupar todos os registros de classificação de veículos por placa
  placas <- agrupar_placas(placas)

  # Gravar resultados
  out_file <- sprintf('%s/PLC_%s%s.csv', pasta_plc_mes, lote_ano, mes)
  write_delim(placas, out_file, delim = ';')

  # Limpar ambiente
  rm(f_pattern, placas, out_file)
  gc(T)

}


# placas <-
#   placas %>%
#   # sample_n(20) %>%
#
#   # Puxar nome da coluna que apresenta o valor máximo por linha - considerar somente colunas numéricas
#   # https://stackoverflow.com/questions/67533157/how-to-use-dplyr-to-get-column-with-max-value-for-each-row
#   # mutate(Class = names(.)[max.col(.)])
#   # mutate(max = names(cur_data())[which.max(c_across(everything()))])
#   mutate(class_pred = names(across(where(is.numeric)))[max.col(across(where(is.numeric)))]) %>%
#
#   # Contar quantas colunas possuem zero em cada linha - considerar somente colunas numéricas
#   # https://stackoverflow.com/questions/11797216/count-number-of-zeros-per-row-and-remove-rows-with-more-than-n-zeros
#   # DF[rowSums(DF == 0)]
#   mutate(class_unic = rowSums(across(where(is.numeric)) != 0)) %>%
#
#   # Calcular acurácia da classificação - para cada linha, somar a quantidade
#   # de ocorrências e calcular a proporção do valor que mais aparece
#   rowwise() %>%
#   mutate(n_ocurr = moto + passeio + onibus + caminhao,
#          acuracia = round(max(moto, passeio, onibus, caminhao) / n_ocurr * 100, 2)) %>%
#
#   # Remover o rowwise
#   ungroup() %>%
#
#   # Reordenar colunas
#   select(placa, moto, passeio, onibus, caminhao, n_ocurr, class_unic, class_pred, acuracia)

