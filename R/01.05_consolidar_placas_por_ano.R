# Requer todos os arquivos anuais por grupo de placas criados nos scripts anteriores

# Agrupa a quantidade de registros de classificações de veículos por placa,
# considerando um ano todo e todos os lotes.

# Limpar memória da sessão do RStudio, com as variáveis
# rm(list = ls())
# .rs.restartR()

library('tidyverse')
library('tidylog')
library('data.table')
# library('arrow')


# Variável principal - modificar cada vez que for rodar, por ano
ano  <- '2022'

# Pastas de arquivos
# pasta_origem  <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares7'
# pasta_placas   <- sprintf('%s/03_PLACAS', pasta_origem)
pasta_origem  <- '/media/livre/Expansion/Radar/PROCREV'
pasta_placas   <- sprintf('%s/03_PLACAS', pasta_origem)
pasta_placas_final <- sprintf('%s/PLC_%s', pasta_placas, ano)
dir.create(pasta_placas_final,  recursive = TRUE, showWarnings = TRUE)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# 1. Agrupar as bases de placas de todos os lotes, a partir dos grupos de placas
# ------------------------------------------------------------------------------

# Puxar lista inicial com todos os arquivos, para, dali, extrair os grupos de
# placas a partir dos nomes
f_pattern <- sprintf('^PLC_L[1-4]_%s_[A-Z]{2}.csv', ano)
arquivos_placas <-
  list.files(pasta_placas, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')

# Puxar divisão de grupos vindas dos nomes de arquivos
grupos <-
  arquivos_placas %>%
  mutate(conj_letras = str_extract(arqs, '[A-Z]{2}.csv'),
         conj_letras = str_replace(conj_letras, '.csv', '')) %>%
  select(conj_letras) %>%
  distinct()

# Limpar ambiente
rm(f_pattern, arquivos_placas)


# Para cada grupo, consolidar base de placas
for (sigla_grupo in grupos$conj_letras) {
  # sigla_grupo <- grupos %>% slice(1) %>% pull()
  print(sigla_grupo)

  # Abrir arquivos do grupo de placas
  f_pattern <- sprintf('^PLC_L[1-4]_%s_%s.csv', ano, sigla_grupo)
  arquivos_placas <-
    list.files(pasta_placas, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
    as.data.frame() %>%
    setNames('arqs')

  # Abrir os arquivos deste grupo para todos os 4 lotes
  arq1 <- read_delim(arquivos_placas %>% slice(1), delim = ';', col_types = 'ciiii')
  arq2 <- read_delim(arquivos_placas %>% slice(2), delim = ';', col_types = 'ciiii')
  arq3 <- read_delim(arquivos_placas %>% slice(3), delim = ';', col_types = 'ciiii')
  arq4 <- read_delim(arquivos_placas %>% slice(4), delim = ';', col_types = 'ciiii')
  # head(arq1)

  # Juntar
  grupo <- rbind(arq1, arq2, arq3, arq4)

  # Limpar ambiente
  rm(arq1, arq2, arq3, arq4)
  gc(T)

  # Remover espaços em branco nas placas
  grupo <- grupo %>% mutate(placa = trimws(placa))

  # Agrupar quantidade de registros de classificação de veículos por placa
  grupo <- agrupar_registros(grupo)
  gc(T)

  # Gravar resultados temporários
  out_file <- sprintf('%s/PLC_%s_%s_tmp.csv', pasta_placas, ano, sigla_grupo)
  write_delim(grupo, out_file, delim = ';')

  # Apagar arquivo temporário e limpar memória
  rm(grupo)
  gc(T)

}



# ------------------------------------------------------------------------------
# 2. Calcular classificação predominante dos veículos
# ------------------------------------------------------------------------------

# Puxar lista inicial com todos os arquivos, para, dali, extrair os grupos de
# placas a partir dos nomes
f_pattern <- sprintf('^PLC_%s_[A-Z]{2}_tmp.csv', ano)
arquivos_placas <-
  list.files(pasta_placas, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')

# Puxar divisão de grupos vindas dos nomes de arquivos
grupos <-
  arquivos_placas %>%
  mutate(conj_letras = str_extract(arqs, '[A-Z]{2}_tmp.csv'),
         conj_letras = str_replace(conj_letras, '_tmp.csv', '')) %>%
  select(conj_letras) %>%
  distinct()

# Limpar ambiente
rm(f_pattern, arquivos_placas)


# Calcular classificação predominante e distribuição da quantidade de vezes que
# os veículos passaram pelos radares
for (sigla_grupo in grupos$conj_letras) {
  # sigla_grupo <- grupos %>% slice(12) %>% pull()
  print(sigla_grupo)

  # Abrir arquivos do grupo de placas
  f_pattern <- sprintf('^PLC_%s_%s_tmp.csv', ano, sigla_grupo)
  arquivos_placas <-
    list.files(pasta_placas, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
    as.data.frame() %>%
    setNames('arqs')

  # Abrir os arquivos deste grupo para todos os 4 lotes
  grupo <- read_delim(arquivos_placas, delim = ';', col_types = 'ciiii')

  # Puxar classificação predominante e quantos tipos de classificação a placa recebeu
  grupo <-
    grupo %>%
    # sample_n(20) %>%

    # Puxar nome da coluna que apresenta o valor máximo por linha - considerar somente colunas numéricas
    # https://stackoverflow.com/questions/67533157/how-to-use-dplyr-to-get-column-with-max-value-for-each-row
    # mutate(Class = names(.)[max.col(.)])
    # mutate(max = names(cur_data())[which.max(c_across(everything()))])
    mutate(class_pred = names(across(where(is.numeric)))[max.col(across(where(is.numeric)))]) %>%

    # Contar quantas colunas diferem de zero em cada linha - considerar somente colunas numéricas
    # https://stackoverflow.com/questions/11797216/count-number-of-zeros-per-row-and-remove-rows-with-more-than-n-zeros
    # DF[rowSums(DF == 0)]
    mutate(class_unic = rowSums(across(where(is.numeric)) != 0))


  # Calcular quantidade de ocorrências e percentual de acurácia da classificação
  grupo <-
    grupo %>%
    # Calcular acurácia da classificação - para cada linha, somar a quantidade
    # de ocorrências e calcular a proporção do valor que mais aparece
    rowwise() %>%
    mutate(n_ocurr = moto + passeio + onibus + caminhao,
           acuracia = round(max(moto, passeio, onibus, caminhao) / n_ocurr * 100, 2)) %>%

    # Remover o rowwise
    ungroup()

  # Limpar memória
  gc(T)

  # Reordenar colunas
  grupo <- grupo %>% select(placa, moto, passeio, onibus, caminhao, n_ocurr, class_unic, class_pred, acuracia)
  # head(grupo)

  # Gravar resultados
  out_file <- sprintf('%s/PLC_%s_%s.csv', pasta_placas_final, ano, sigla_grupo)
  write_delim(grupo, out_file, delim = ';')


  # Separar base só com placas e quantidade de ocorrências
  grupo <- grupo %>% select(placa, n_ocurr)
  # head(grupo)

  # Gravar resultados - se arquivo já existir, só complementá-lo
  out_file2 <- sprintf('%s/PLC_%s_00_dist_tmp.csv', pasta_placas_final, ano)
  if (file.exists(out_file2)) {
    write_delim(grupo, out_file2, delim = ';', append = TRUE)
  } else {
    write_delim(grupo, out_file2, delim = ';', append = FALSE)
  }


  # Remover arquivo temporário
  # file.remove(arquivos_placas$arqs)

  # Limpar memória
  gc(T)

  }



# ------------------------------------------------------------------------------
# 3. Distribuição total de veículos por passagem em radares
# ------------------------------------------------------------------------------

open_file <- sprintf('%s/PLC_%s_00_dist_tmp.csv', pasta_placas_final, ano)
placas <- read_delim(open_file, delim = ';', col_types = 'ci')
head(placas)

# # Quantidade de placas únicas no ano
# n_placas_unicas <- nrow(placas)

# Agrupamentos
placas <- placas %>% mutate(placa = str_sub(placa, 1, 1))
placas_grouped <-
  placas %>%
  group_by(placa) %>%
  tally() %>%
  # Adicionar soma de todas as placas únicas
  add_row(placa = 'AZ (todas)', n = sum(.$n))

gc(T)


# Criar dataframe vazio para abrigar distribuições
tmp_df <- data.frame()

# Gerar distribuiõçes de passagens por radar por letra de início da placa
for (letter in LETTERS[(seq(1, 26))]) {
  # letter <- LETTERS[(seq(1, 1))]
  print(letter)
  this <- placas %>% filter(placa == letter)

  # Se não houver placas daquela letra, criar um com zero ocorrências
  if (nrow(this) == 0) {
    this <- data.frame(placa = letter, n_ocurr = 0)
  }

  # Calcular distribuição das placas
  this <-
    this %>%
    # Calcular quantis para a letra
    select(n_ocurr) %>%
    quantile(probs = c(0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.96, 0.97, 0.975, 0.98, 0.985, 0.99, 0.995), na.rm = TRUE) %>%
    # Transformar em dataframe, nomear a coluna e transpor, mantendo como dataframe
    as.data.frame() %>%
    setNames(letter) %>%
    t() %>%
    as.data.frame() %>%
    # Inserir um nome para a primeira coluna (que havia ficado como um .index)
    rownames_to_column(var = 'placa')

  tmp_df <- rbind(tmp_df, this)
}

# Complementar distribuições com o total geral
that <-
  placas %>%
  # Calcular quantis para a letra
  select(n_ocurr) %>%
  quantile(probs = c(0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.96, 0.97, 0.975, 0.98, 0.985, 0.99, 0.995), na.rm = TRUE) %>%
  # Transformar em dataframe, nomear a coluna e transpor, mantendo como dataframe
  as.data.frame() %>%
  setNames('AZ (todas)') %>%
  t() %>%
  as.data.frame() %>%
  # Inserir um nome para a primeira coluna (que havia ficado como um .index)
  rownames_to_column(var = 'placa')

tmp_df <- rbind(tmp_df, that)
tail(tmp_df)


# Juntar qtds de registros de placa única com distribuições de pssagem nos radares
df_out <-
  tmp_df %>%
  left_join(placas_grouped, by = 'placa')

df_out

# Gravar resultados
out_file3 <- sprintf('%s/PLC_%s_00_dist.csv', pasta_placas_final, ano)
write_delim(df_out, out_file3, delim = ';', append = TRUE)



# lote_ano  <- 'L4_2019'
# lote_ano2 <- str_c(str_sub(lote_ano, 1, 2), str_sub(lote_ano, 4, 7))
#
# pasta_base <- sprintf('%s/PLC_ANO_%s', pasta_placas, lote_ano2)
# open_file <- sprintf('%s/PLC_%s_AC.csv', pasta_base, lote_ano)
# arq <- read_delim(open_file, delim = ';', col_types = 'ciiii')
#
# arq1 <- arq %>% filter(str_starts(placa, "[AB]"))
# arq2 <- arq %>% filter(str_starts(placa, "[C]"))
#
# # arq1 <- arq %>% filter(str_starts(placa, "[STU]"))
# # arq2 <- arq %>% filter(str_starts(placa, "[WXYZ]"))
#
# out_file1 <- sprintf('%s/PLC_%s_AB.csv', pasta_base, lote_ano)
# write_delim(arq1, out_file1, delim = ';')
#
# out_file2 <- sprintf('%s/PLC_%s_CC.csv', pasta_base, lote_ano)
# write_delim(arq2, out_file2, delim = ';')
#
# file.remove(open_file)
#
# open_file <- sprintf('%s/PLC_%s_SZ.csv', pasta_base, lote_ano)
# arq <- read_delim(open_file, delim = ';', col_types = 'ciiii')
#
# arq1 <- arq %>% filter(str_starts(placa, "[S-U]"))
# arq2 <- arq %>% filter(str_starts(placa, "[W-Z]"))
#
# # arq1 <- arq %>% filter(str_starts(placa, "[STU]"))
# # arq2 <- arq %>% filter(str_starts(placa, "[WXYZ]"))
#
# out_file1 <- sprintf('%s/PLC_%s_SU.csv', pasta_base, lote_ano)
# write_delim(arq1, out_file1, delim = ';')
#
# out_file2 <- sprintf('%s/PLC_%s_WZ.csv', pasta_base, lote_ano)
# write_delim(arq2, out_file2, delim = ';')
#
# file.remove(open_file)