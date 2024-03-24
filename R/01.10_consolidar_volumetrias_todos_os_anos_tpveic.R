# # Limpar memória da sessão do RStudio, com as variáveis
# rm(list = ls())
# .rs.restartR()
# detach("package:tidylog")

library('tidyverse')
library('tidylog')
library('data.table')

# Variável principal - modificar cada vez que for rodar, por lote e ano
ano <- '2022'

# Pastas de arquivos
pasta_origem  <- '/media/livre/Expansion/Radar/PROCREV'
pasta_volume2 <- sprintf('%s/02_VOLUME/VOL_TPVC', pasta_origem)

# Listar arquivos a serem processados
f_pattern <- '^VOL_TPVC_REV_L[1-4]_20[0-9]{6}.csv'
arquivos_volumes <-
  list.files(pasta_volume2, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')

# ------------------------------------------------------------------------------
# Agrupar volumetrias
# ------------------------------------------------------------------------------

agrupar_volumetrias <- function(df_arquivos, string_pattern) {
  # df_arquivos <- arquivos_volumes; string_pattern <- sprintf('VOL_TPVC_REV_L1_%s', ano)

  # Filtrar segmento de interesse (por lote) para processamento em paralelo
  volumes <- df_arquivos %>% filter(str_detect(arqs, string_pattern))


  # Juntar todos os arquivos de volumetria em um único dataframe
  volumes <-
    lapply(X = volumes, FUN = read_delim, delim = ';', col_types = 'cciiii') %>%
    rbindlist(fill = TRUE)

  # Agrupar resultados por dia e local
  volumes <-
    volumes %>%
    filter(str_detect(local, '[0-9]{4}')) %>%
    group_by(data, local) %>%
    # tally() %>% filter(n> 3) %>% head()
    summarise('0' = sum(`0`),
              '1' = sum(`1`),
              '2' = sum(`2`),
              '3' = sum(`3`)) %>%
    # filter(data == '20171102' & local == '6798')
    ungroup() # %>%
    # pivot_wider(id_cols = data, names_from = local, values_from = total)

}

# Agrupar volumetrias por lote
volumes_L1 <- agrupar_volumetrias(arquivos_volumes, sprintf('VOL_TPVC_REV_L1_%s', ano))
volumes_L2 <- agrupar_volumetrias(arquivos_volumes, sprintf('VOL_TPVC_REV_L2_%s', ano))
volumes_L3 <- agrupar_volumetrias(arquivos_volumes, sprintf('VOL_TPVC_REV_L3_%s', ano))
volumes_L4 <- agrupar_volumetrias(arquivos_volumes, sprintf('VOL_TPVC_REV_L4_%s', ano))

# Juntar todas as volumetrias
volumes_out <- rbind(volumes_L1, volumes_L2) #, volumes_L3, volumes_L4)

# Agrupar resultados por dia e local
volumes_out <-
  volumes_out %>%
  group_by(data, local) %>%
  summarise('0' = sum(`0`),
            '1' = sum(`1`),
            '2' = sum(`2`),
            '3' = sum(`3`)) %>%
    ungroup()


# Remover anos não relacionados a este
volumes_out <- volumes_out %>% filter(str_starts(data, ano))

# Puxar todos os dias deste ano
datas <- volumes_out %>% select(data) %>% distinct()

# Selecionar todos os locais a partir dos nomes das colunas
locais <- volumes_out %>% select(local) %>% distinct()
locais %>% sample_n(10)


# Gerar um gráfico de volume registrado por dia para cada local
for (loc in locais$local) {
  # loc <- '6788'

  png(filename = sprintf('%s/VOL_%s_%s.png', pasta_graficos, local, ano))

  volumes_out %>%
    filter(local == loc) %>%
    right_join(datas, by = 'data') %>%
    add_column(index = 1:nrow(.), .after = 'data') %>%
    select(index, '0') %>%
    plot(main = sprintf('Volumetria: Local %s // Ano %s', loc, ano),
         xlab = 'dias do ano',
         ylab = 'volume',
         cex = 0.25,
         cex.axis = 0.8,
         las = 1)

  dev.off()
}

