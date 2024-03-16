# Agrupa a quantidade de registros de classificações de veículos por placa mês
# a mês. Requer todos os arquivos diários criados no script anterior

# Limpar memória da sessão do RStudio, com as variáveis
# rm(list = ls())
# .rs.restartR()

library('tidyverse')
library('tidylog')
library('data.table')


# Variável principal - modificar cada vez que for rodar, por lote e ano
lote_ano  <- 'L4_2022'
lote_ano2 <- str_c(str_sub(lote_ano, 1, 2), str_sub(lote_ano, 4, 7))


# Pastas de arquivos
pasta_origem  <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares7'
pasta_plc_mes <- sprintf('%s/03_PLACAS/TMP/PLC_MES_%s', pasta_origem, lote_ano2)
pasta_plc_ano <- sprintf('%s/03_PLACAS/TMP/PLC_ANO_%s', pasta_origem, lote_ano2)
dir.create(pasta_plc_ano,  recursive = TRUE, showWarnings = TRUE)


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

  # Limpar ambiente para liberar memória
  rm(placas)
  gc(T)

  # Agrupar quantidade de registros de classificação de veículos por placa
  grupo1 <- agrupar_registros(grupo1)
  grupo2 <- agrupar_registros(grupo2)
  grupo3 <- agrupar_registros(grupo3)
  grupo4 <- agrupar_registros(grupo4)
  grupo5 <- agrupar_registros(grupo5)

  # Substitutir dataframe original de placas pelos resultados dos agrupamentos
  placas <- rbind(grupo1, grupo2, grupo3, grupo4, grupo5)

}

# Gravar arquivo - junta a arquivo existente se existir, senão cria um novo
gravar_arquivo <- function(out_arq, df) {
  if (file.exists(out_arq)) {
    write_delim(df, out_arq, delim = ';', append = TRUE)
  } else {
    write_delim(df, out_arq, delim = ';', append = FALSE)
  }
}


# ------------------------------------------------------------------------------
# Agrupar arquivos temporários por grupo e trimestre, para caber na memória
# ------------------------------------------------------------------------------

# Criar padrões para grupos de arquivos temporários
pattern1 <- sprintf('^PLC_%s0[1-3].csv', lote_ano)
pattern2 <- sprintf('^PLC_%s0[4-6].csv', lote_ano)
pattern3 <- sprintf('^PLC_%s0[7-9].csv', lote_ano)
pattern4 <- sprintf('^PLC_%s1[0-2].csv', lote_ano)
patterns <- c(pattern1, pattern2, pattern3, pattern4)
rm(pattern1, pattern2, pattern3, pattern4)


# Agrupar arquivos por trimestre
for (pattern in patterns) {
  # pattern <- patterns[1]
  print(pattern)

  # Abrir grupos de arquivos do trimestre em um dataframe
  placas <- abrir_e_juntar_arquivos(pasta_plc_mes, pattern)
  # head(placas, 20)

  # Separar placas em grupos de tamanhos similares para dividir o processamento
  grupo1 <- placas %>% filter(str_starts(placa, '[A-B]'))
  grupo2 <- placas %>% filter(str_starts(placa, '[C]'))
  grupo3 <- placas %>% filter(str_starts(placa, '[D]'))
  grupo4 <- placas %>% filter(str_starts(placa, '[E]'))
  grupo5 <- placas %>% filter(str_starts(placa, '[F]'))
  grupo6 <- placas %>% filter(str_starts(placa, '[G-H]'))
  grupo7 <- placas %>% filter(str_starts(placa, '[I-J]'))
  grupo8 <- placas %>% filter(str_starts(placa, '[K-N]'))
  grupo9 <- placas %>% filter(str_starts(placa, '[O]'))
  grupo10 <- placas %>% filter(str_starts(placa, '[P-R]'))
  grupo11 <- placas %>% filter(str_starts(placa, '[S-U]'))
  grupo12 <- placas %>% filter(str_starts(placa, '[V-Z]'))

  # Gravar resultados - ir juntando os trimestres em um único arquivo de saída
  out_file1 <- sprintf('%s/PLC_%s_tmp_AB.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file1, grupo1)

  out_file2 <- sprintf('%s/PLC_%s_tmp_CC.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file2, grupo2)

  out_file3 <- sprintf('%s/PLC_%s_tmp_DD.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file3, grupo3)

  out_file4 <- sprintf('%s/PLC_%s_tmp_EE.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file4, grupo4)

  out_file5 <- sprintf('%s/PLC_%s_tmp_FF.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file5, grupo5)

  out_file6 <- sprintf('%s/PLC_%s_tmp_GH.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file6, grupo6)

  out_file7 <- sprintf('%s/PLC_%s_tmp_IJ.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file7, grupo7)

  out_file8 <- sprintf('%s/PLC_%s_tmp_KN.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file8, grupo8)

  out_file9 <- sprintf('%s/PLC_%s_tmp_OO.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file9, grupo9)

  out_file10 <- sprintf('%s/PLC_%s_tmp_PR.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file10, grupo10)

  out_file11 <- sprintf('%s/PLC_%s_tmp_SU.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file11, grupo11)

  out_file12 <- sprintf('%s/PLC_%s_tmp_VZ.csv', pasta_plc_ano, lote_ano)
  gravar_arquivo(out_file12, grupo12)

  # Limpar ambiente
  rm(placas, grupo1, grupo2, grupo3, grupo4, grupo5, grupo6, grupo7, grupo8,
     grupo9, grupo10, grupo11, grupo12)
  gc(T)

}

rm(pattern, patterns,
   out_file1, out_file2, out_file3, out_file4, out_file5,
   out_file6, out_file7, out_file8, out_file9, out_file10,
   out_file11, out_file12)


# ------------------------------------------------------------------------------
# 2. Consolidar agrupamento de placas por grupo de letras
# ------------------------------------------------------------------------------


f_pattern <- sprintf('^PLC_%s_tmp_[A-Z]{2}.csv', lote_ano)
arquivos_placas <-
  list.files(pasta_plc_ano, pattern = f_pattern, recursive = FALSE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')

for (arq in arquivos_placas$arqs) {
  # arq <- arquivos_placas %>% slice(1) %>% pull()
  print(arq)

  # Abrir arquivo do grupo de placas
  grupo <- read_delim(arq, delim = ';', col_types = "ciiii")
  # head(grupo)

  # Agrupar quantidade de registros de classificação de veículos por placa
  grupo <- agrupar_registros(grupo)

  # Gravar resultados, removendo o '_tmp_' do nome do arquivo
  out_file <- str_replace(arq, '(PLC_L[1-4]_20[0-9]{2})_tmp_([A-Z]{2})', '\\1_\\2')
  write_delim(grupo, out_file, delim = ';')

  # Apagar arquivo temporário e limpar memória
  file.remove(arq)
  rm(grupo)
  gc(T)

}


