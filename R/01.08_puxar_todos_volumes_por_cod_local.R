library('tidyverse')
library('tidylog')
library('data.table')

# Mostra valores sem notação científica
options(scipen = 999)

# Pastas de arquivos
pasta_origem   <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares6'
# pasta_origem   <- '/media/livre/Expansion/Radar/PROCREV'
pasta_volume   <- sprintf('%s/02_VOLUME', pasta_origem)
pasta_graficos <- sprintf('%s/04_VOLGRA', pasta_origem)
pasta_grf_hist <- sprintf('%s/VOL_HIST', pasta_graficos)
pasta_sem_estudos <- sprintf('%s/VOL_NOEST', pasta_graficos)
dir.create(pasta_sem_estudos, recursive = TRUE, showWarnings = TRUE)

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



# ------------------------------------------------------------------------------
# Remover colunas "duplicadas" .x e .y
# ------------------------------------------------------------------------------

# Remove colunas duplicadas (demarcadas com .x e .y), selecionando só a que tem
# maior quantidade de registros
remover_colunas_duplicadas <- function(df) {
  # df <- vol2017

  # Alguns códigos de local estão vindo repetidos de lotes diferentes, provavelmente
  # por erros nos registros. Exemplos são cod_local 0001, 0002 e 2443
  cods_repetidos <- data.frame(cod_local = names(df)) %>% filter(str_detect(cod_local, 'x'))
  cods_repetidos

  # Para cada um desses códigos, descartar coluna com menos ocorrências
  if (nrow(cods_repetidos) > 0) {
    for (cod in cods_repetidos$cod_local) {
      # cod <- cods_repetidos$cod_local[1]

      # cod é o código com o x depois: 2443.x
      print(cod)
      # Substituir o x por y: 2443.y
      cod2 <- str_replace(cod, 'x', 'y')

      # Comparar quantas vezes aquele código aparece em cada coluna
      comparativo1 <- df %>% select(all_of(cod))  %>% distinct() %>% nrow()
      comparativo2 <- df %>% select(all_of(cod2)) %>% distinct() %>% nrow()

      # Descartar coluna com menos ocorrência
      if (comparativo1 > comparativo2) {
        df <- df %>% select(-cod2)

      } else if (comparativo2 > comparativo1) {
        df <- df %>% select(-cod)

      } else {
        # Se as duas colunas têm a mesma quantidade, ambas são um erro, tanto faz
        df <- df %>% select(-cod)

      }

    }

    # Renomear coluna que fica para cod_local sem .x ou .y
    names(df) <- str_replace(names(df), '.[xy]', '')
    return(df)

  } else {
    return(df)
  }

}

# Remover as colunas duplicadas
vol2016 <- remover_colunas_duplicadas(vol2016)
vol2017 <- remover_colunas_duplicadas(vol2017)
vol2018 <- remover_colunas_duplicadas(vol2018)
vol2019 <- remover_colunas_duplicadas(vol2019)
vol2020 <- remover_colunas_duplicadas(vol2020)


# ------------------------------------------------------------------------------
# Somar todos os volumes de todos os locais
# ------------------------------------------------------------------------------

# Soma todas as colunas numéricas
calcular_totais <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    mutate(vol_total = rowSums(., na.rm = TRUE)) %>%
    select(vol_total)
}


# Transpor dataframe em duas etapas: (1) trazer todas as colunas menos a
# selecionada como linhas e (2) puxar os valores da coluna que estava
# selecionada como as novas distribuições de colunas
# https://community.rstudio.com/t/how-to-divide-rows-by-another-row/53265/5
vol2016 <- vol2016 %>% pivot_longer(-data) %>% pivot_wider(name, names_from = data)
vol2017 <- vol2017 %>% pivot_longer(-data) %>% pivot_wider(name, names_from = data)
vol2018 <- vol2018 %>% pivot_longer(-data) %>% pivot_wider(name, names_from = data)
vol2019 <- vol2019 %>% pivot_longer(-data) %>% pivot_wider(name, names_from = data)
vol2020 <- vol2020 %>% pivot_longer(-data) %>% pivot_wider(name, names_from = data)
head(vol2020)

# Juntar todos os volumes de todos os locais
all_vols <-
  vol2016 %>%
  full_join(vol2017, by = 'name') %>%
  full_join(vol2018, by = 'name') %>%
  full_join(vol2019, by = 'name') %>%
  full_join(vol2020, by = 'name')


# Calcular soma dos volumes de todos os dias, por local
all_cods   <- all_vols %>% select(cod = name)
tot_gerais <- calcular_totais(all_vols)
tot_gerais <- cbind(all_cods, tot_gerais)
head(tot_gerais)


# ------------------------------------------------------------------------------
# Remover prováveis ruídos da base
# ------------------------------------------------------------------------------

# Definir limite de corte: quantos registros um equipamento tem que ter, no mínimo?
# Sugesão: pelo menos 100 passagens de veículo por dia - cada dia é uma coluna
# em all_vols
limite_min <- (length(names(all_vols)) - 1) * 100

# Selecionar locais com registros acima ou igual ao limite de corte
# tot_gerais %>% filter(vol_total < limite_min) %>% arrange(vol_total) %>% tail(20)
tot_gerais <- tot_gerais %>% filter(vol_total >= limite_min)

tot_gerais %>% select(vol_total) %>% summary()
head(tot_gerais)


# ------------------------------------------------------------------------------
# Buscar por estudos técnicos de todos os códigos e guardar em dataframe
# ------------------------------------------------------------------------------

# Local de busca:
file_paths <- '/home/livre/Desktop/Base_GtsRegionais/Pedidos_LAI/LAI_Localizacao_Radares/75331_Estudos_e_levantamentos_tecnicos'

# Definir localização do find e do grep no sistema
find_bin_path <- sprintf("/usr/bin/find")
grep_bin_path <- sprintf("/bin/grep")

# Criar dataframe vazio para guardar resultados
out_df <- data.frame()

# Buscar estudos técnicos para todos os códigos
for (cod in tot_gerais$cods) {
  # cod <- '6652' # tem resultado;
  # cod <- '6655' # vazio
  print(cod)

  # Criar busca por find com grep pelo nome do arquivo
  # https://stackoverflow.com/questions/5914370/list-file-names-based-on-a-filename-pattern-and-file-content
  # Conando: find [path] -type f | grep "2872"
  arg_1 <- sprintf('%s -type f | ', file_paths)
  arg_2 <- sprintf('%s "%s"', grep_bin_path, cod)
  # Resultado da busca - resultado do system2 para objeto do R
  # https://stackoverflow.com/questions/45324791/pass-output-from-system2-into-r-object
  resp  <- system2(command = find_bin_path, args = c(arg_1, arg_2), stdout = TRUE)

  # Se não houver resposta, deixar resposta como NA
  if (length(resp) == 0) {
    # Juntar resultado ao dataframe de saída
    out_df <- out_df %>% rbind(data.frame(cod = cod,
                                          arq = NA))

  } else {
    # Simplificar resposta: queremos sé o nome do arquivo e a pasta onde estava
    resp <- str_replace(resp, sprintf('%s/', file_paths), '')

    # Juntar resultado ao dataframe de saída
    out_df <- out_df %>% rbind(data.frame(cod = cod,
                                          arq = resp))
  }


}

# Ordenar codigos de locais
out_df <- out_df %>% arrange(cod)
# Juntar com dados de volume
out_df <- out_df %>% left_join(tot_gerais, by = 'cod')
out_df <- out_df %>% relocate(vol_total, .after = 'cod')
head(out_df)

# out_df %>% filter(is.na(arq))

# Gravar resultados
out_file <- sprintf('%s/VOL_vs_ESTUDOS.csv', pasta_graficos)
write_delim(out_df, out_file, delim = ';')


# Copiar gráficos de volume dos locais que não possuem estudos técnicos para
# uma pasta de checagem
sem_estudos <- out_df %>% filter(is.na(arq))

for (cod in sem_estudos$cod) {
  file_orig <- sprintf('%s/VOL_%s.png', pasta_grf_hist, cod)
  file.copy(file_orig, pasta_sem_estudos)
}
