# # Limpar memória da sessão do RStudio, com as variáveis
# rm(list = ls())
# .rs.restartR()
# detach("package:tidylog")

library('tidyverse')
# library('tidylog')

# Pastas de arquivos
# pasta_origem  <- '/media/livre/Expansion/Radar/PROCREV'
pasta_origem <- '/media/livre/SSD120GB/tmp_radares2'
pasta_procrev <- sprintf('%s/01_PRCREV', pasta_origem)

# Listar arquivos a serem processados
f_pattern <- '^REV_L[1-4]_20[0-9]{6}.txt'
arquivos_radares <-
  list.files(pasta_procrev, pattern = f_pattern, recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')


for (arq in arquivos_radares$arqs) {
  # arq <- arquivos_radares %>% slice(1) %>% pull()

  # ----------------------------------------------------------------------------
  # Estrutura de pastas a partir do nome do arquivo
  # ----------------------------------------------------------------------------

  # Registrar nome do arquivo
  basename <- str_replace(arq, sprintf('%s/', pasta_procrev), '')
  # Se padrão for "L12014_TXT/L1_20140101.txt", separar pela barra e pegar segunda
  # parte da string
  if (str_detect(basename, '\\/')) { basename <- str_split(basename, '\\/')[[1]][2] }
  print(basename)

  # Variável principal - modificar cada vez que for rodar, por lote e ano
  lote_ano  <- substr(basename, 5, 11)
  lote_ano2 <- str_c(str_sub(lote_ano, 1, 2), str_sub(lote_ano, 4, 7))

  # Criar novas pastas, se necessário
  pasta_volume1 <- sprintf('%s/02_VOLUME/VOL_TUDO/VOL_%s', pasta_origem, lote_ano2)
  pasta_volume2 <- sprintf('%s/02_VOLUME/VOL_TPVC/VOL_TPVC_%s', pasta_origem, lote_ano2)
  # dir.create(pasta_volume1,  recursive = TRUE, showWarnings = FALSE)
  dir.create(pasta_volume2,  recursive = TRUE, showWarnings = FALSE)


  # Definir arquivo de saída
  out_vol_file <- sprintf('%s/VOL_TPVC_%s', pasta_volume2, str_replace(basename, 'txt', 'csv'))

  # Só rodar se arquivo de saída não existir ainda
  if (!file.exists(out_vol_file)) {

    # Ler arquivo e separar colunas de interesse
    dados <- read_delim(arq, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)
    head(dados)

    # Separar string grande de texto em colunas
    dados <-
      dados %>%
      mutate(# lote  = substr(X1, 1, 2),
        data  = substr(X1, 3, 10),
        # hora  = substr(X1, 11, 16), # hora HHMMSS
        # hora  = substr(X1, 11, 12), # só hora completa
        local = substr(X1, 17, 20),
        # faixa = substr(X1, 21, 21),
        # n_reg = substr(X1, 22, 29),
        tipo_reg  = substr(X1, 30, 30),
        # placa = substr(X1, 31, 37),
        tipo_veic = substr(X1, 38, 38),
        # class = substr(X1, 39, 39),
        # comp  = substr(X1, 40, 42),
        # vel_p = substr(X1, 43, 45),
        # tocup = substr(X1, 46, 50),
        # vel_m = substr(X1, 51, 53),
      ) %>%
      select(-X1)

    # head(dados)

    # ----------------------------------------------------------------------------
    # Puxar volumetria por local e tipo de veículo
    # ----------------------------------------------------------------------------

    # Para criar as volumetrias por tipo de veículo, não precisamos dos dados sem
    # registros de veículos
    tp_reg_nao_2 <-
      dados %>%
      filter(tipo_reg != '2') %>%
      group_by(data, local, tipo_veic) %>%
      tally() %>%
      ungroup() %>%
      pivot_wider(id_cols = c(data, local),
                  names_from = tipo_veic,
                  values_from = n)

    # Puxar todos os dias e locais da base
    dias_locais <- dados %>% select(data, local) %>% distinct()

    # Juntar volumetria por tipo de veículo
    volumes <-
      dias_locais %>%
      left_join(tp_reg_nao_2, by = c('data', 'local')) %>%
      # Trocar NAs por 0
      mutate(across(where(is.numeric), ~replace_na(.x, 0)))

    # Checar por todas as colunas de tipo de veículo possíveis - se algum tipo
    # estiver faltando, incluir
    vol_col_names <- names(volumes)
    if (!'0' %in% vol_col_names) {
      volumes <- volumes %>% add_column('0' = 0, .after = 'local')
    }
    if (!'1' %in% vol_col_names) {
      volumes <- volumes %>% add_column('1' = 0, .after = '0')
    }
    if (!'2' %in% vol_col_names) {
      volumes <- volumes %>% add_column('2' = 0, .after = '1')
    }
    if (!'3' %in% vol_col_names) {
      volumes <- volumes %>% add_column('3' = 0, .after = '2')
    }

    # Garantir que somente os tipos válidos 0, 1, 2 e 3 vão estar na base final
    volumes <- volumes %>% select(data, local, '0', '1', '2', '3')

    # Escrever arquivo de volumetria
    write_delim(volumes, out_vol_file, delim = ';')

    # Limpar ambiente
    rm(dados, volumes, vol_col_names, dias_locais, tp_reg_nao_2, out_vol_file,
       basename, lote_ano, lote_ano2, pasta_volume1, pasta_volume2)

    gc(T)

  } else {

    print(sprintf('Arquivo %s já foi processado, pulando...', basename))

    # Limpar ambiente
    rm(basename, lote_ano, lote_ano2, pasta_volume1, pasta_volume2)

  }

}

