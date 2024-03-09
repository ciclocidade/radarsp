library('tidyverse')
library('tidylog')
library('data.table')

# Mostra valores sem notação científica
options(scipen = 999)

# Pastas de arquivos
# pasta_origem   <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares6'
pasta_origem   <- '/media/livre/Expansion/Radar/PROCREV'
pasta_volume   <- sprintf('%s/02_VOLUME', pasta_origem)
pasta_graficos <- sprintf('%s/04_VOLGRA', pasta_origem)
pasta_grf_hist <- sprintf('%s/VOL_HIST', pasta_graficos)
dir.create(pasta_grf_hist, recursive = TRUE, showWarnings = TRUE)

# Listar arquivos a serem processados
f_pattern <- sprintf('^VOL_[0-9]{4}.csv')
arquivos_volumes <-
  list.files(pasta_origem, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')


# TODO: Atualizar aqui
ano_inicial <- '2016'; ano_final <- '2020'

# Abrir todos os dados de volume por ano
# TODO: Atualizar aqui
vol2016 <- read_delim(arquivos_volumes[[1]][1], delim = ';', col_types = cols(.default = "i"))
vol2017 <- read_delim(arquivos_volumes[[1]][2], delim = ';', col_types = cols(.default = "i"))
vol2018 <- read_delim(arquivos_volumes[[1]][3], delim = ';', col_types = cols(.default = "i"))
vol2019 <- read_delim(arquivos_volumes[[1]][4], delim = ';', col_types = cols(.default = "i"))
vol2020 <- read_delim(arquivos_volumes[[1]][5], delim = ';', col_types = cols(.default = "i"))

# Delimitar somente datas referentes ao ano de interesse - isso vai derrubar algumas
# poucas linhas de erro e de dias limites aos anos anterior/posterior
vol2016 <- vol2016 %>% mutate(data = as.character(data)) %>% filter(str_starts(data, '2016'))
vol2017 <- vol2017 %>% mutate(data = as.character(data)) %>% filter(str_starts(data, '2017'))
vol2018 <- vol2018 %>% mutate(data = as.character(data)) %>% filter(str_starts(data, '2018'))
vol2019 <- vol2019 %>% mutate(data = as.character(data)) %>% filter(str_starts(data, '2019'))
vol2020 <- vol2020 %>% mutate(data = as.character(data)) %>% filter(str_starts(data, '2020'))

# Todos os códigos de local presentes nas bases de todos os anos
# TODO: Atualizar aqui
cods2016 <- data.frame(cod_local = names(vol2016))
cods2017 <- data.frame(cod_local = names(vol2017))
cods2018 <- data.frame(cod_local = names(vol2018))
cods2019 <- data.frame(cod_local = names(vol2019))
cods2020 <- data.frame(cod_local = names(vol2020))

# Criar um dataframe com todas as entradas únicas de cod_local
# TODO: Atualizar aqui
all_cods <-
  rbind(cods2016, cods2017, cods2018, cods2019, cods2020) %>%
  distinct() %>%
  filter(cod_local != 'data') %>%
  arrange(cod_local)


# all_dates <-
#   rbind(vol2017 %>% select(data),
#         vol2018 %>% select(data),
#         vol2019 %>% select(data)) %>%
#   distinct()
#
# all_cods %>%
#   mutate(nada = '') %>%
#   pivot_wider(names_from = cod_local,
#               values_from = nada) %>%
#   mutate(data = all_dates$data)
#
#
# all_dates %>% cbind(all_cols)
#
#
# as.Date('20170101', format = "%Y%m%d")
# seq(as.Date('20170101', format = '%Y%m%d'), as.Date('20170201', format = '%Y%m%d'), 1)
#


# ------------------------------------------------------------------------------
# Criar gráficos para cada local
# ------------------------------------------------------------------------------

# Criar um dataframe com todos os dias do ano de dados vazios de volume
create_dummy_df <- function(ano_base, cod_local) {
  # ano_base <- '2017'
  data_inicio <- sprintf('%s0101', ano_base)
  data_final  <- sprintf('%s1231', ano_base)

  this <-
    # Criar uma linha para cada dia do ano
    data.frame(data = seq(as.Date(data_inicio, format = '%Y%m%d'),
                          as.Date(data_final, format = '%Y%m%d'),
                          1),
               # Deixar dados de volumes vazios
               cod = as.integer(NA)) %>%
    # Remover hífens do formato de data
    mutate(data = str_replace_all(data, '-', '')) %>%
    # Renomear colunas para que a de volume fique com o nome do código de local
    setNames(c('data', {{cod_local}}))
}


# Gerar gráficos para todos os códigos de local presentes no histórico da base
detach("package:tidylog")

for (cod in all_cods$cod_local) {
  # (cod <- all_cods %>% slice(200) %>% pull())
  print(cod)

  # Gerar dataframes temporários, de volumes por ano
  # TODO: Atualizar aqui
  if (cod %in% names(vol2016)) {
    tmp_2016 <- vol2016 %>% select(data, all_of(cod))
  } else {
    tmp_2016 <- create_dummy_df('2016', cod)
  }

  if (cod %in% names(vol2017)) {
    tmp_2017 <- vol2017 %>% select(data, all_of(cod))
  } else {
    tmp_2017 <- create_dummy_df('2017', cod)
  }

  if (cod %in% names(vol2018)) {
    tmp_2018 <- vol2018 %>% select(data, all_of(cod))
  } else {
    tmp_2018 <- create_dummy_df('2018', cod)
  }

  if (cod %in% names(vol2019)) {
    tmp_2019 <- vol2019 %>% select(data, all_of(cod))
  } else {
    tmp_2019 <- create_dummy_df('2019', cod)
  }

  if (cod %in% names(vol2020)) {
    tmp_2020 <- vol2020 %>% select(data, all_of(cod))
  } else {
    tmp_2020 <- create_dummy_df('2020', cod)
  }

  # Dataframe temporário são os dias de todos os anos e respectivos volumes
  # TODO: Atualizar aqui
  tmp_all <- rbind(tmp_2016, tmp_2017, tmp_2018, tmp_2019, tmp_2020)

  # Gerar gráficos
  png(filename = sprintf('%s/VOL_%s.png', pasta_grf_hist, cod))

  tmp_all %>%
    # Criar uma coluna de index, que vai virar o eixo X do gráfico
    add_column(index = 1:nrow(.), .after = 'data') %>%
    # Manter somente colunas de index e volume
    select(index, all_of(cod)) %>%
    # Plotar gráficos com pontos pequenos
    plot(main = sprintf('Volumetria %s-%s: Local %s', ano_inicial, ano_final, cod),
         xlab = 'dias',
         ylab = 'volume',
         cex = 0.25,
         cex.axis = 0.8,
         las = 1)

  dev.off()

}

# (one_cod <- all_cods %>% slice(200) %>% pull())
#
# tmp_2017 <- vol2017 %>% select(data, all_of(one_cod))
# tmp_2018 <- vol2018 %>% select(data, all_of(one_cod))
# tmp_2019 <- vol2019 %>% select(data, all_of(one_cod))
# tmp_all <- rbind(tmp_2017, tmp_2018, tmp_2019)
#
# tmp_all %>%
#   # head() %>%
#   mutate(data = 1:nrow(.)) %>%
#   plot(main = sprintf('Volumetria: Local %s', one_cod),
#        xlab = 'data',
#        ylab = paste0({{one_cod}}),
#        cex = 0.25,
#        cex.axis = 0.8,
#        las = 1)

# ------------------------------------------------------------------------------
# Criar gráficos para volume total ao longo dos anos
# ------------------------------------------------------------------------------

# Soma todas as colunas numéricas
calcular_totais <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    mutate(vol_total = rowSums(., na.rm = TRUE)) %>%
    select(vol_total)
}


# 2016
datas2016 <- vol2016 %>% select(data)
total2016 <- calcular_totais(vol2016)
total2016 <- cbind(datas2016, total2016)

# 2017
datas2017 <- vol2017 %>% select(data)
total2017 <- calcular_totais(vol2017)
total2017 <- cbind(datas2017, total2017)

# 2018
datas2018 <- vol2018 %>% select(data)
total2018 <- calcular_totais(vol2018)
total2018 <- cbind(datas2018, total2018)

# 2019
datas2019 <- vol2019 %>% select(data)
total2019 <- calcular_totais(vol2019)
total2019 <- cbind(datas2019, total2019)

# 2020
datas2020 <- vol2020 %>% select(data)
total2020 <- calcular_totais(vol2020)
total2020 <- cbind(datas2020, total2020)


# Juntar tudo
totais <- rbind(total2016, total2017, total2018, total2019, total2020)
# Deixar totais em milhões
totais <- totais %>% mutate(vol_total = vol_total / 1000000)

# Gerar gráfico
png(filename = sprintf('%s/VOL_TOTAL.png', pasta_graficos))

totais <-
  totais %>%
  # Criar uma coluna de index, que vai virar o eixo X do gráfico
  add_column(index = 1:nrow(.), .after = 'data') %>%
  # Manter somente colunas de index e volume
  select(index, vol_total)

# Plotar gráficos com pontos pequenos
plot(totais,
     main = sprintf('Volumetria total: %s-%s', ano_inicial, ano_final),
     xlab = 'dias',
     ylab = 'volume (em milhões)',
     cex = 0.25,
     cex.axis = 0.8,
     las = 1)

# Adicionar uma linha de tendência, que por sua vez é uma regressão linear
# https://stackoverflow.com/questions/35742754/how-to-build-a-trendline-on-a-graph-in-r
lines(predict(lm(totais$vol_total ~ totais$index)),col = 'red')

dev.off()
