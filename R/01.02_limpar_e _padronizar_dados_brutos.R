# # Limpar memória da sessão do RStudio, com as variáveis
# rm(list = ls())
# .rs.restartR()
# detach("package:tidylog")

library('tidyverse')
# library('tidylog')

# Instância paralela: se script estiver sendo rodado em mais de uma instância
# do R ao mesmo tempo, alterar este número - é ele quem define o nome do
# arquivo temporário de descartes de linhas com muitos caracteres
inst_paralela <- 1

# Variável principal - modificar cada vez que for rodar, por lote e ano
lote_ano  <- 'L4_2022'
lote_ano2 <- str_c(str_sub(lote_ano, 1, 2), str_sub(lote_ano, 4, 7))

# Pastas de arquivos
pasta_origem <- '/media/livre/SSD120GB/tmp_radares2'
# pasta_origem <- '/media/livre/SSD120GB/tmp_radares2'
pasta_destino  <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/api_radares_dados/tmp_brutos_radares/tmp_radares7'
pasta_logreg  <- sprintf('%s/00_LOGREG', pasta_destino)
pasta_procrev <- sprintf('%s/01_PRCREV/REV_%s', pasta_destino, lote_ano2)
pasta_descart <- sprintf('%s/01_PRCREV/DSC/DSC_%s', pasta_destino, lote_ano2)
pasta_volume  <- sprintf('%s/02_VOLUME/VOL_%s', pasta_destino, lote_ano2)
pasta_placas  <- sprintf('%s/03_PLACAS/TMP/PLC_%s', pasta_destino, lote_ano2)
# pasta_analise <- sprintf('%s/04_VELMED/VM_%s_TXT', pasta_destino, lote_ano2)
dir.create(pasta_logreg,  recursive = TRUE, showWarnings = TRUE)
dir.create(pasta_procrev, recursive = TRUE, showWarnings = TRUE)
dir.create(pasta_descart, recursive = TRUE, showWarnings = TRUE)
dir.create(pasta_volume,  recursive = TRUE, showWarnings = TRUE)
dir.create(pasta_placas,  recursive = TRUE, showWarnings = TRUE)
# dir.create(pasta_analise, recursive = TRUE, showWarnings = TRUE)

# Arquivo de log
out_log_file <- sprintf('%s/LOG_%s.csv', pasta_logreg, lote_ano)

# Listar arquivos a serem processados
f_pattern <- '^L[1-4]_20[0-9]{6}.txt'
arquivos_radares <-
  list.files(pasta_origem, pattern = f_pattern, recursive = FALSE, full.names = TRUE) %>%
  as.data.frame() %>%
  setNames('arqs')

# Filtrar segmento de interesse (por lote) para processamento em paralelo
arquivos_radares <- arquivos_radares %>% filter(str_detect(arqs, lote_ano))

# Se o script estiver sendo rodado em uma segunda instância do R, inverter ordem
if (inst_paralela == 2) { arquivos_radares <- arquivos_radares %>% arrange(desc(arqs)) }
# tail(arquivos_radares)

# Limpar ambiente
rm(lote_ano2, f_pattern)


for (arq in arquivos_radares$arqs) {
  # arq <- arquivos_radares %>% slice(128) %>% pull()
  # arq <- arquivos_radares %>% sample_n(1) %>% pull()

  # Registrar nome do arquivo
  basename <- str_replace(arq, sprintf('%s/', pasta_origem), '')
  # Se padrão for "L12014_TXT/L1_20140101.txt", separar pela barra e pegar segunda
  # parte da string
  if (str_detect(basename, '\\/')) { basename <- str_split(basename, '\\/')[[1]][2] }
  print(basename)


  # --------------------------------------------------------------------------
  # Checar se arquivo já foi processado
  # --------------------------------------------------------------------------

  # Se arquivo de log já existe, abri-lo e checar se esta base já foi processada
  if (file.exists(out_log_file)) {
    log <- read_delim(out_log_file, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)

    if (basename %in% log$X1) {
      print(sprintf('Arquivo %s já foi processado, pulando...', basename))
      next
    }
  }

  # ----------------------------------------------------------------------------
  # Processar arquivo, caso ainda não tenha sido
  # ----------------------------------------------------------------------------

  # Abrir arquivo - se arquivo não abrir, pode ser por erro de cadeia de caracteres
  # com nul (\0) incluso, o que não é lido pelo R (quando rodado no Jupyter) e
  # gera um aviso. Lidar com esta exceção será (1) abrir o arquivo original como
  # raw, (2) substituir o nul por caractere unicode de espaço, (3) salvar o
  # resultado como arquivo temporário e (4) ler este arquivo temporário em vez
  # do original. Ao final do script, apagar arquivo temporário
  dados <- tryCatch(
    expr = {
      # Tentar ler arquivo normalmente, como um txt comum
      read_delim(arq, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)
    },
    error = function(e) {
      # Criar novo nome para arquivo temporário com '_v2.txt' ao final
      # '/media/livre/SSD120GB/tmp_radares1/L4_20150612.txt_v2.txt'
      arq_tmp <- str_c(arq, '_v2.txt')

      # Ler arquivo como raw e substituir nul por espaço
      # https://stackoverflow.com/questions/34214859/removing-nul-characters-within-r
      # https://utf8-chartable.de/unicode-utf8-table.pl?utf8=0x
      r <- readBin(arq, raw(), file.info(arq)$size)
      r[r == as.raw(0)] = as.raw(0x20)

      # Salvar arquivo temporário
      writeBin(r, arq_tmp)
      rm(r)

      # Esperar até que arquivo termine de escrever para lê-lo
      Sys.sleep(5)

      # Ler novo arquivo com write_delim()
      # str(readLines("00staff.txt"))
      dados <- read_delim(arq_tmp, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)

      return(dados)
    }
  )
  # sample_n(dados, 20)

  # Registrar quantidade de linhas originais
  reg_linhas1 <- nrow(dados)

  # Substituir caracteres ASCII / Unicode eventualmente presentes na base - não
  # achei um jeito de fazer isso direto por REGEX, então vai por listagem mesmo.
  # Lista de caracteres: http://www.endmemo.com/unicode/ascii.php
  dados <-
    dados %>%
    # filter(str_detect(X1, '\\\f')) %>%
    mutate(X1 = str_replace_all(X1, '\\\f', ''),
           X1 = str_replace_all(X1, '\\\b', ''),
           X1 = str_replace_all(X1, '\\\006', ''),
           # Esta linha não funciona, pular
           # X1 = str_replace_all(X1, '\\\u0000', ''),
           X1 = str_replace_all(X1, '\\\u0001', ''),
           X1 = str_replace_all(X1, '\\\u0002', ''),
           X1 = str_replace_all(X1, '\\\u0003', ''),
           X1 = str_replace_all(X1, '\\\u0004', ''),
           X1 = str_replace_all(X1, '\\\u0005', ''),
           X1 = str_replace_all(X1, '\\\u0006', ''),
           X1 = str_replace_all(X1, '\\\u0007', ''),
           X1 = str_replace_all(X1, '\\\u0008', ''),
           X1 = str_replace_all(X1, '\\\u0009', ''),
           X1 = str_replace_all(X1, '\\\u000a', ''),
           X1 = str_replace_all(X1, '\\\u000b', ''),
           X1 = str_replace_all(X1, '\\\u000c', ''),
           X1 = str_replace_all(X1, '\\\u000d', ''),
           X1 = str_replace_all(X1, '\\\u000e', ''),
           X1 = str_replace_all(X1, '\\\u000f', ''),
           X1 = str_replace_all(X1, '\\\u0010', ''),
           X1 = str_replace_all(X1, '\\\u0011', ''),
           X1 = str_replace_all(X1, '\\\u0012', ''),
           X1 = str_replace_all(X1, '\\\u0013', ''),
           X1 = str_replace_all(X1, '\\\u0014', ''),
           X1 = str_replace_all(X1, '\\\u0015', ''),
           X1 = str_replace_all(X1, '\\\u0016', ''),
           X1 = str_replace_all(X1, '\\\u0017', ''),
           X1 = str_replace_all(X1, '\\\u0018', ''),
           X1 = str_replace_all(X1, '\\\u0019', ''),
           X1 = str_replace_all(X1, '\\\u001a', ''),
           X1 = str_replace_all(X1, '\\\u001b', ''),
           X1 = str_replace_all(X1, '\\\u001c', ''),
           X1 = str_replace_all(X1, '\\\u001d', ''),
           X1 = str_replace_all(X1, '\\\u001e', ''),
           X1 = str_replace_all(X1, '\\\u001f', ''),
           # Retirar aspas eventuais que podem estar presentes na linha
           X1 = str_replace_all(X1, '\\\"', ''),
           X1 = str_replace_all(X1, '-', '')
    )

  # Para casos em que havia muitas linhas que eram só quebra de espaço (\n) e
  # o mode, a seguir, vai ser influenciado por isso ficando 0 ou número baixo
  dados <- dados %>% filter(X1 != '')

  # Há um caso incrível de uma base em que as linhas começam com NAL10219...etc:
  # NAL12019010721104366893000927520EYC74471003905600666000 -
  # remover um ou dois caracteres iniciais para deixar só a string principal
  # dados <- dados %>% mutate(X1 = str_replace(X1, ".?.?(L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})", "\\1"))

  # E outro caso em que inseriram "EFX" no meio do registro
  # L1201512090009506601100128010DNT47251003606901435000
  # L1201512090011176601100128030       0000910200776000
  # L12015120900133166011EFX00128040       1004309701185777
  # L12015120900142766011EFX00128050       2108107501924777
  # dados %>% filter(str_detect(X1, ".?.?(L[1234]([0-9]){19})([A-Z]){3}(([0-9]){8}([A-Z0-9 ]){7}([0-9 ]){16})")) %>% head()

  # 0---------1-----------2------------3-------------4-----------5----
  # 12 34567890 123456 7890 1 23456789 0 1234567 8 9 012 345 67890 123
  # L1 20210915 053214 6965 3 00008577 0 EBG5042 1 0 042 072 00548 000 # 53 caracteres (8 números de registro)
  # L      data   hora  loc fx   n_reg tp  placa tiv com vel tocup  vm

  # Caso com espaço extra antes de entrar no número de registro (e registro
  # começando com E00, sugerindo radar de entrefaixa, mas só aconteceu no
  # L1 e primeiros meses de 2017 - já ajustando para rodar os outros lotes)
  # L1201701191808006960 5E00000032       1000000000000000
  # L1201701191844006960 5E00004770FKO08541006512203450000

  # boo <- data.frame(X1 = 'L1201701191844006960 5E00004770FKO08541006512203450000')
  # boo %>% mutate(X1 = str_replace(X1, ".?.?(L[1234][0-9]{18}) ?([0-9])([A-Z]{3})?([A-Z0-9][0-9]{7}[A-Z0-9 ]{7}[0-9 ]{16})", "\\1\\2\\4"))

  # ... fazer alteração para os três casos detectados
  # dados <- dados %>% mutate(X1 = str_replace(X1, ".?.?(L[1234][0-9]{19})([A-Z]{3})?([0-9]{8}[A-Z0-9 ]{7}[0-9 ]{16})", "\\1\\3"))
  dados <- dados %>% mutate(X1 = str_replace(X1, ".?.?(L[1234][0-9]{18}) ?([0-9])([A-Z]{3})?([A-Z0-9][0-9]{7}[A-Z0-9 ]{7}[0-9 ]{16})", "\\1\\2\\4"))


  # Qual a extensão das strings mais presente na base (qual a moda)?
  # https://www.geeksforgeeks.org/mean-median-and-mode-in-r-programming/
  # Dados possíveis são 52, 53 ou eventualmente 30:
  mode <- function() { return(sort(-table(nchar(dados$X1)))[1]) }
  # A moda é o nome da coluna resultante na tabela
  mode <- as.integer(names(mode()))


  # --------------------------------------------------------------------------
  # Separar bases de dados: linhas com tamanho = mode; linhas maiores que mode
  # --------------------------------------------------------------------------

  # Há linhas que estão com mais de um registro. Qual a maior extensão de linha?
  # https://stackoverflow.com/questions/73235593/find-row-with-longest-string-in-r
  max_char1 <- dados %>% slice(which.max(nchar(X1))) %>% pull() %>% nchar()
  # dados %>% slice(which.min(nchar(X1))) %>% pull() %>% nchar()

  # Linhas que possuem mais caracteres do que o esperado (mode) serão separadas
  # para tratamento, enquanto as que estão ok permanecem no dataframe dados
  dados_tmp <- dados %>% filter(nchar(X1) > mode)
  dados <- dados %>% filter(nchar(X1) <= mode)
  # head(dados_tmp)

  # Gravar linhas grandes em arquivo de texto temporário para tratamento no gsub
  out_file <- sprintf('%s/tmp_linhas_extensas_v%i.txt', pasta_destino, inst_paralela)
  write_delim(dados_tmp, out_file, delim = ';')



  # Alguns arquivos de descarte ficam enormes, podendo ter com várias linhas de
  # registros reais. Como as substituições por gsub demoram para serem feitas
  # em arquivos grandes, arquivos com mais de 30 MB terão suas linhas quebradas
  # com o sed. Isso porque muitos deles contém uma única linha gigantesca com
  # todos os registros concatenados, sem quebra de parágrafo entre eles
  if (file.info(out_file)$size > 30000000) {

    # Definir um novo arquivo de saída
    out_file_tmp <- sprintf('%s/tmp_linhas_extensas_v%i_tmp.txt', pasta_destino, inst_paralela)

    # Inserir quebras de linha no arquivo, a partir da sigla do lote
    # cat arquivo_com_linhas_extensas.txt | sed -e 's/L4/\nL4/g' > arquivo_saida.txt
    cat_path <- sprintf("/bin/cat")
    sed_path <- sprintf("/bin/sed")
    lote <- str_sub(lote_ano, 1, 2)
    cat_args <- sprintf('"%s" | %s -e \'s/%s/\\n%s/g\' > "%s"', out_file, sed_path, lote, lote, out_file_tmp)
    system2(command = cat_path, args = c(cat_args))

    # Limpar ambiente
    rm(cat_path, sed_path, lote, cat_args)

    # Ler arquivo de texto como string, reconhecendo quebras de parágrafo
    file_str <- readLines(out_file_tmp)

    # Padrões de saída para 53 e 30 caracteres (com e sem registros de veículos)
    output_pattern1 <- '(L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})'
    output_pattern2 <- '(L[1234]([0-9]){27}2)'

    # Separar arquivos que já estão no padrão dos que não estão
    out_file_tmp_ok <- sprintf('%s/tmp_linhas_extensas_v%i_tmp_OK.txt', pasta_destino, inst_paralela)
    out_file_tmp_nao_ok <- sprintf('%s/tmp_linhas_extensas_v%i_tmp_NOK.txt', pasta_destino, inst_paralela)

    for (line in file_str) {

      # Se arquivo tem 53 caracteres e está no padrão esperado, salvar como OK
      if (nchar(line) == 53 & str_detect(line, output_pattern1)) {
        # print(line)
        cat(line, sep = '\n', file = out_file_tmp_ok, append = TRUE)

        # Se arquivo tem 30 caracteres e está no padrão esperado, salvar como OK
      } else if (nchar(line) == 30 & str_detect(line, output_pattern2)) {
        # print(line)
        cat(line, sep = '\n', file = out_file_tmp_ok, append = TRUE)

        # Do contrário, salvar em arquivo de não OK para tratamento
      } else {
        cat(line, sep = '\n', file = out_file_tmp_nao_ok, append = TRUE)
      }

    }

    # Com alguma esperança, as linhas com erro são bem menores do que as sem
    # erro. A primeira coisa a fazer é ler o arquivo dos erros e juntar as linhas
    # que haviam sido separadas no lote mas que o L1, L2, L3 ou L4 podem ser uma
    # parte da placa, logo apṕos as 2 primeiras letras. Exemplo:
    # L42021070613392225392001293390LI
    # L47821003709400456000
    file_str <- paste(readLines(out_file_tmp_nao_ok), collapse = '\n')

    # Juntar registros cuja sigla do lote fazia parte da placa
    file_str <- gsub('(L[1234]([0-9]){28}([A-Z]){2})\n(L[1234]([0-9]){3}([0-9 ]){16})', '\\1\\4', file_str, perl = TRUE)

    # Sobrescrever arquivo de 'não OK'
    cat(file_str, file = out_file_tmp_nao_ok)

    # Refazer filtro anterior: se linha do arquivo está no padrão de 53
    # caracteres, juntá-la ao arquivo de OK
    # Ler arquivo de texto como string, reconhecendo quebras de parágrafo
    file_str <- readLines(out_file_tmp_nao_ok)
    file.remove(out_file_tmp_nao_ok)

    for (line in file_str) {

      # Se arquivo tem 53 caracteres e está no padrão esperado, salvar como OK
      if (nchar(line) == 53 & str_detect(line, output_pattern1)) {
        # print(line)
        cat(line, sep = '\n', file = out_file_tmp_ok, append = TRUE)

        # Do contrário, salvar de volta em arquivo de não OK
      } else {
        cat(line, sep = '\n', file = out_file_tmp_nao_ok, append = TRUE)
      }

    }

    # Ler arquivo temporário das linhas que estão OK, agora como dataframe
    dados_tmp <- read_delim(out_file_tmp_ok, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)
    # head(dados_tmp)

    # Ler arquivo temporário das linhas que não estão OK como dataframe
    dados_tmp2 <- read_delim(out_file_tmp_nao_ok, delim = ';', col_types = cols(.default = "c"), col_names = FALSE)
    # head(dados_tmp2)

    # Remover arquivos temporários
    file.remove(out_file, out_file_tmp, out_file_tmp_ok, out_file_tmp_nao_ok)
    # Limpar ambiente
    rm(output_pattern1, output_pattern2, line, out_file,
       out_file_tmp, out_file_tmp_ok, out_file_tmp_nao_ok)


  } else {
    # Se arquivo for menor do que 30MB, ler direto como string e inserir quebras de parágrafo com REGEX via gsub
    file_str <- paste(readLines(out_file), collapse = '\n')

    # Reconhecer strings com 53 caracteres
    file_str <- gsub('(L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})', '\\1\n', file_str, perl = TRUE)
    # Reconhecer strings com 52 caracteres - isso vai quebrar o zero final das
    # strings acima, deixando linhas com parágrafo-0-parágrafo
    file_str <- gsub('(L[1234]([0-9]){27}([A-Z0-9 ]){7}([0-9 ]){16})', '\\1\n', file_str, perl = TRUE)
    # Colar os zeros que ficaram isolados nas linhas às strings da linha anterior
    file_str <- gsub('(L[1234]([0-9]){27}([A-Z0-9 ]){7}([0-9 ]){16})\n0\n', '\\10\n', file_str, perl = TRUE)
    # Remover quebras de espaço duplas
    file_str <- gsub('\n+', '\n', file_str, perl = TRUE)


    # Quebrar string tipo a abaixo, em que uma marcação do caractere 30, referente
    # à passagem ou não de veículos é 2 (sem passagem de veículos) e está seguida
    # pelo começo de um novo registro
    # L42016022523244025791000275482L4201602252324542579100
    # 0274470EFW05072113310601312000L42016022521283925791000274480GBG72702123010002289000
    file_str <- gsub('(L[1234]([0-9]){27}2)(L[1234]([0-9]){21})', '\\1\n\\3', file_str, perl = TRUE)
    # Separada a parte de cima, precisamos separar a parte de baixo
    # L42016022521281025791000274462
    # L4201602252128182579100
    # 0274470EFW05072113310601312000L42016022521283925791000274480GBG72702123010002289000
    file_str <- gsub('(([0-9]){7}([A-Z0-9 ]){7}([0-9 ]){16})(L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})', '\\1\n\\5', file_str, perl = TRUE)
    # E, finalmente, juntar as linhas que são um outro registro
    # L42016022521281025791000274462
    # L4201602252128182579100
    # 0274470EFW05072113310601312000
    file_str <- gsub('(L[1234]([0-9]){21})\n(([0-9]){7}([A-Z0-9 ]){7}([0-9 ]){16})', '\\1\\3', file_str, perl = TRUE)

    # Remover aspas duplas do arquivo, para que não sejam confundidas com
    # demarcação de começo/final do texto (o read_delim vai ler tudo que estiver
    # entre aspas duplas como sendo uma coluna só, ignorando, por exemplo o \n)
    file_str <- gsub('\\"', '', file_str, perl = TRUE)


    # Para finalizar, desgarrar strings de 53 caracteres que correspodem ao padrão
    # de demais textos, inserindo uma quebra de parágrafo antes delas
    file_str <- gsub('(L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})', '\n\\1\n', file_str, perl = TRUE)
    # Remover quebras de espaço duplas
    file_str <- gsub('\n+', '\n', file_str, perl = TRUE)

    # Gravar resultados em arquivo temporário, substituindo o anterior
    cat(file_str, file = out_file)

    # Ler arquivo temporário, agora como dataframe
    dados_tmp <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
    # head(dados_tmp)

    # Remover arquivo temporário
    file.remove(out_file)

    # Todas as linhas com string igual o menor que mode serão juntadas ao dataframe
    # principal (dados)
    dados_tmp2 <- dados_tmp %>% filter(nchar(X1) > mode)
    dados_tmp  <- dados_tmp %>% filter(nchar(X1) <= mode)

  }


  # Juntar dados de dados_tmp ao dataframe principal dados
  dados <- rbind(dados, dados_tmp)

  # Registrar quantidade de linhas pós primeiro corte
  reg_linhas2 <- nrow(dados)
  reg_linhas3 <- nrow(dados_tmp2)

  # Gravar linhas grandes remanescentes (caso existam)
  out_file <- sprintf('%s/DESCARTE_%s', pasta_descart, basename)
  if (nrow(dados_tmp2) > 0) {
    write_delim(dados_tmp2, out_file, delim = ';')
  }


  # Limpar ambiente antes de prosseguir
  rm(out_file, file_str, dados_tmp, dados_tmp2)


  # Quantidade máxima de caracteres nas linhas
  max_char2 <- dados %>% slice(which.max(nchar(X1))) %>% pull() %>% nchar()
  # dados %>% slice(which.min(nchar(X1))) %>% pull() %>% nchar()
  # sample_n(dados, 20)

  # ---------------------12345678------------------------ # ----------------------
  # L12014090412390066092 01056740FEV03241000000001035000 # 52 caracteres (7 números de registro)
  # L12021091505321469653000085770EBG50421004207200548000 # 53 caracteres (8 números de registro)


  # # Se ainda houver linhas com mais de 53 caracteres na base, separar as linhas
  # # que precisam passar por tratamento em um dataframe temporário, que será
  # # juntado ao principal após a finalização
  # if (max_char2 > 53) {
  #   # Separar linhas que não têm a quantidade de caracteres válidos (52 ou 53 caracteres)
  #   dados_tmp <- dados %>% filter(nchar(X1) != mode)
  #   linhas_tmp1 <- nrow(dados_tmp)
  #   # sample_n(dados_tmp, 20)
  #
  #   # Manter somente linhas com a quantidade de caracteres válidos (52 ou 53 caracteres)
  #   dados <- dados %>% filter(!dados$X1 %in% dados_tmp$X1)
  #   linhas_alter2 <- nrow(dados)
  #   # sample_n(dados, 20)
  #
  #
  #   # Tomar cuidado nos arquivos de 2014 - o 2 que identifica que não houve
  #   # passagem de veículos está entrando no local da placa como caracter extra
  #   # e resulta em uma linha com 1 caracter a mais do que deveria
  #   # --------------------------------------------------------------------------
  #   # L12014090409360067071000012252       1000000000000000
  #   # L1201409041725186608100049691GGZ20001005712500450000L120140904183…
  #   # L1201409041002216609201010310EMP76423004513900323000L120140904172…
  #   # L12014090421160067081000014002       1000000000000000
  #
  #   # TODO: VERIFICAR SE HÁ DIFERENÇAS PARA 2014 E DEMAIS ANOS NA POSIÇÃO
  #   # Linhas com 30 caracteres em que o 30.o é '2' são um registro de que o radar
  #   # estava funcionando mas não havia veículos passando naqueles 4 minutos
  #   # referentes àquele registro
  #   dados_tmp2 <- dados_tmp %>% filter(nchar(X1) == 30 & str_ends(X1, '2'))
  #   linhas_tmp2 <- nrow(dados_tmp2)
  #   # dados_tmp %>% filter(nchar(X1) == 30)
  #
  #   if (nrow(dados_tmp2) > 0) {
  #     # Atualizar dados_tmp de forma a retirar as linhas de dados_tmp2
  #     dados_tmp <- dados_tmp %>% filter(!dados_tmp$X1 %in% dados_tmp2$X1)
  #
  #     # Atualizar dados com as linhas de dados_tmp2
  #     dados <- dados %>% rbind(dados_tmp2)
  #   }
  #
  #   rm(dados_tmp2)
  #
  #
  #   # Ainda temos linhas maiores do que o esperado?
  #   max_char3 <- dados_tmp %>% slice(which.max(nchar(X1))) %>% pull() %>% nchar()
  #
  #
  #   # --------------------------------------------------------------------------
  #   # Separar strings em colunas e juntá-las depois como linhas em um df só
  #   # --------------------------------------------------------------------------
  #
  #   # Registrar quantidade máxima de colunas, arredondada para cima
  #   max_cols <- ceiling(max_char3/mode)
  #
  #   # Separar linhas longas em diferentes colunas com REGEX e positive lookbehind -
  #   # vamos usar 500 colunas como máximo, pois além disso o processamento sofre -
  #   # considerar cenários para linhas com 52 ou 53 caracteres
  #   # https://stackoverflow.com/questions/64496828/add-line-breaks-to-a-long-string-in-r-tidyverse-soultion
  #   if (mode == 52 & max_char3 > mode) {
  #
  #     # O que muda é o número dentro da primeira chave, aqui = {27}
  #     dados_tmp <- dados_tmp %>% separate(X1,
  #                                         "(?<=L[1234]([0-9]){27}([A-Z0-9 ]){7}([0-9 ]){16})",
  #                                         into = c(sprintf('X%i', seq(1, max_cols))))
  #
  #   } else if (mode == 53 & max_char3 > mode) {
  #
  #     # O que muda é o número dentro da primeira chave, aqui = {28}
  #     dados_tmp <- dados_tmp %>% separate(X1,
  #                                         "(?<=L[1234]([0-9]){28}([A-Z0-9 ]){7}([0-9 ]){16})",
  #                                         into = c(sprintf('X%i', seq(1, max_cols))))
  #
  #   }
  #
  #   # Junta as colunas de um ou dois dataframes como linhas em um dataframe só
  #   bind_xs <- function(df1, df2 = df1, var1, var2 = var1) {
  #     df_out <- rbind(df1 %>% select(X1 = {{ var1 }}), df2 %>% select(X1 = {{ var2 }}))
  #   }
  #
  #   # Criar dataframe vazio para abrigar resultados
  #   lala  <- data.frame()
  #
  #   # Sequência de colunas de acordo com a extensão da linha
  #   # cols <- sprintf('X%i', seq(1,2))
  #   cols <- sprintf('X%i', seq(1, max_cols))
  #
  #   # Transformar todas as colunas em linhas de um único dataframe
  #   for (col in cols) { lala <- bind_xs(dados_tmp, var1 = X1, var2 = col) }
  #
  #   # Substituir dataframe temporário
  #   dados_tmp <- lala
  #
  #   # Limpar ambiente
  #   rm(lala, col, cols, bind_xs)
  #   # sample_n(dados_tmp, 20)
  #
  #   # Remover linhas vazias
  #   dados_tmp <- dados_tmp %>% filter(!X1 == '')
  #   dados_tmp <- dados_tmp %>% filter(!X1 == '0' & !X1 == '00')
  #
  #   # Registrar linhas após filtros
  #   linhas_tmp3 <- nrow(dados_tmp)
  #
  #
  #   # --------------------------------------------------------------------------
  #   # Ainda dados de 2014...
  #   # --------------------------------------------------------------------------
  #   # L1201409040446456609300949280       1003813600279000
  #   # L1201409041346356609201080090EAD17713004008900441000
  #   # L12014090411520067081000012592       100000000000000 *
  #   # L1201409040127296608100040202       0000000000000000 *
  #   # L1201409041239006609201056740FEV03241000000001035000
  #   # L12021091505321469653000085770EBG50421004207200548000
  #
  #   # TODO - revisar esse trecho
  #   # Checar se 30.o ou 31.o caracter é um número; se sim, ele está entrando no
  #   # espaço que deveria ser da placa. Para consertar, remover primeiro número do
  #   # registro (caracter 22) e inserir um 0 ao final (caracter == mode)
  #   if (mode == 52) {
  #     # Registrar linhas a serem alteradas
  #     linhas_alter3 <- dados_tmp %>% filter(str_starts(X1, 'L[1-4]') & str_detect(substr(X1, 30, 30),'[0-9]')) %>% nrow()
  #
  #     # Fazer alteração
  #     dados_tmp <-
  #       dados_tmp %>%
  #       mutate(X1 = ifelse(
  #         str_starts(X1, 'L[1-4]') & str_detect(substr(X1, 30, 30),'[0-9]'),
  #         str_c(substr(X1, 1, 21), substr(X1, 23, mode), '0'),
  #         X1))
  #
  #   } else if (mode == 53) {
  #     # Registrar linhas a serem alteradas
  #     linhas_alter3 <- dados_tmp %>% filter(str_starts(X1, 'L[1-4]') & str_detect(substr(X1, 31, 31),'[0-9]')) %>% nrow()
  #
  #     # Fazer alteração
  #     dados_tmp <-
  #       dados_tmp %>%
  #       mutate(X1 = ifelse(
  #         str_starts(X1, 'L[1-4]') & str_detect(substr(X1, 31, 31),'[0-9]'),
  #         str_c(substr(X1, 1, 21), substr(X1, 23, mode), '0'),
  #         X1))
  #   }
  #
  #
  #   # Se string resultante tiver 1 caracter a mais do que o esperado e não começar
  #   # com L1, L2, L3 ou L4, remover primeiro caracter
  #   # Registrar linhas a serem alteradas
  #   linhas_alter4 <- dados_tmp %>% filter(nchar(X1) == mode + 1 & !str_starts(X1, 'L[1-4]')) %>% nrow()
  #
  #   # Fazer alteração
  #   dados_tmp <-
  #     dados_tmp %>%
  #     mutate(X1 = ifelse(nchar(X1) == mode + 1 & !str_starts(X1, 'L[1-4]'),
  #                        substr(X1, 2, mode + 1),
  #                        X1))
  #
  #   # Juntar às linhas que estavam com a quantidade de caracteres corretos
  #   dados <- dados %>% rbind(dados_tmp)
  #   linhas_alter5 <- nrow(dados)
  #   rm(dados_tmp)
  #
  # } else {
  #   # Caso não haja linhas com mais de 53 caracteres, declarar nulas as
  #   # seguintes variáveis de log:
  #   max_cols <- NA; max_char3 <- NA;
  #   linhas_tmp1 <- NA; linhas_tmp2 <- NA;linhas_tmp3 <- NA;
  #   linhas_alter2 <- NA; linhas_alter3 <- NA; linhas_alter4 <- NA; linhas_alter5 <- NA
  # }

  # ----------------------------------------------------------------------------
  # Remoção de linhas que podem ter problemas
  # ----------------------------------------------------------------------------

  # Garantir que não há linhas duplicadas
  dados <- dados %>% distinct()
  reg_linhas4 <- nrow(dados)

  # Linhas sem nada
  dados <- dados %>% filter(!is.na(X1))

  # Linhas com caracteres inválidos - não parece estar tendo efeito
  # validUTF8('y��cL��=��s�2cqL12014090103080067081000000482       1000000000000000') = TRUE
  dados <- dados %>% filter(validUTF8(X1))
  reg_linhas5 <- nrow(dados)

  # dados %>% filter(nchar(X1) != 52)
  # dados %>% filter(!str_starts(X1, 'L[1-4]'))

  # Todas as linhas devem começar com L1, L2, L3 ou L4 e ter 30, 52 ou 53 caracteres
  dados <- dados %>% filter(str_starts(X1, 'L[1-4]') & (nchar(X1) == 30 | nchar(X1) == 52 | nchar(X1) == 53))
  reg_linhas6 <- nrow(dados)

  # tail(dados, 20)
  # sample_n(dados, 20)
  # dados %>% filter(nchar(X1) == 30) %>% sample_n(10)


  # ------------------------------------------------------------------------------
  # Finalizar - todas as linhas devem ter 53 ou 30 caracteres
  # ------------------------------------------------------------------------------

  # if (max_char_linhas == 53) {
  #   # Transformar de 53 caracteres para 50 caracteres - seriam os 53 menos os
  #   # três finais, que são de velocidade média e estão todos zerados
  #   dados <- dados %>% mutate(X1 = ifelse(nchar(X1) == 53,
  #                                         substr(X1, 1, 50),
  #                                         X1))
  #
  # } else if (max_char_linhas == 52) {
  #   # Transformar de 52 caracteres para 53 e, depois, 51 caracteres - para isso,
  #   # inserir um 0 como primeiro dígito de número de registro e descartar os
  #   # três zeros finais, que são de velocidade média
  #   dados <- dados %>% mutate(X1 = ifelse(nchar(X1) == 52,
  #                                         str_c(substr(X1, 1, 21), '0', substr(X1, 22, 49)),
  #                                         X1))
  # }

  # ---------------------12345678------------------------ # ----------------------
  # L12014090412390066092 01056740FEV03241000000001035000 # 52 caracteres (7 números de registro)
  # L12021091505321469653000085770EBG50421004207200548000 # 53 caracteres (8 números de registro)
  # L1201902181241346634NF_000632180       10049038031350

  # Transformar de 52 caracteres para 53
  dados <- dados %>% mutate(X1 = ifelse(nchar(X1) == 52,
                                        str_c(substr(X1, 1, 21), '0', substr(X1, 22, 52)),
                                        X1))
  # sample_n(dados, 20)

  # Todas as linhas devem ter agora 30 ou 53 caracteres
  max_char_linhas <- dados %>% slice(which.max(nchar(X1))) %>% pull() %>% nchar()


  # Linhas que são marcação de que o radar estava funcionando mas que não houve
  # fluxo de veículos (caracter 30 == '2') devem ficar com 30 caracteres
  dados <- dados %>% mutate(X1 = ifelse(substr(X1, 30, 30) == '2',
                                        str_replace(X1, '(L[1234]([0-9]){27}2)( ){7}([0-9 ]){16}', '\\1'),
                                        X1))
  # dados %>% filter(substr(X1, 30, 30) == '2') %>% sample_n(20)

  # Menor extensão das linhas deve ser agora 30 caracteres
  min_char_linhas <- dados %>% slice(which.min(nchar(X1))) %>% pull() %>% nchar()

  # sample_n(dados, 20)

  # Ver na tela checagem básica sobre extensões das linhas
  if (max_char_linhas == mode & (min_char_linhas == mode | min_char_linhas == 30)) {
    sprintf('min_char_linhas = %i, max_char_linhas = %i', min_char_linhas, max_char_linhas)
  } else {
    print('ATENÇÃO - PROBLEMA NA EXTENSÃO DAS LINHAS')
  }


  # ----------------------------------------------------------------------------
  # Validação via colunas de tipo_reg, tipo_veic e class_veic
  # ----------------------------------------------------------------------------

  # TODO - Incluir linhas descartadas aqui no arquivo de descarte?

  # Separar bases com 30 e 53 caracteres
  dados_30char <- dados %>% filter(nchar(X1) == 30)
  dados_53char <- dados %>% filter(nchar(X1) == 53)

  # Validação: as colunas de tipo_reg, tipo_veic e class_veic devem ter somente
  # os caracteres esperados. Além disso, primeiro caracter de placa deve ser
  # letra ou espaço - remover todo o resto
  dados_53char <- dados_53char %>% filter(str_detect(substr(X1, 30, 30),'[01]')
                                          & str_detect(substr(X1, 31, 31),'[A-Z ]')
                                          & str_detect(substr(X1, 38, 38),'[0123 ]')
                                          & str_detect(substr(X1, 39, 39),'[01 ]'))


  # Juntar dados para exportar
  dados <- rbind(dados_53char, dados_30char)

  # Registrar quantidade de linhas e limpar ambiente
  reg_linhas7 <- nrow(dados)
  rm(dados_30char, dados_53char)


  # ----------------------------------------------------------------------------
  # Gravar bases resultantes - para nuvem e para análises
  # ----------------------------------------------------------------------------

  # 0---------1-----------2------------3-------------4-----------5----
  # 12 34567890 123456 7890 1 23456789 0 1234567 8 9 012 345 67890 123
  # L1 20210915 053214 6965 3 00008577 0 EBG5042 1 0 042 072 00548 000 # 53 caracteres (8 números de registro)
  # L      data   hora  loc fx   n_reg tp  placa tiv com vel tocup  vm

  # # Novos dados brutos menores - tudo menos letra L do lote, número geral de
  # # registro e velocidade média (41 caracteres)
  # dados_brutos_novos <- dados %>% mutate(X1 = str_c(substr(X1, 2, 21),
  #                                                   substr(X1, 30, 50)))

  # # Novos dados brutos menores - tudo menos letra L do lote e velocidade média
  # # (49 caracteres)
  # dados_brutos_novos <- dados %>% mutate(X1 = substr(X1, 2, 50))
  #
  # # Escrever arquivo final, com todos os tratamentos
  # out_file_procrev <- sprintf('%s/REV_%s', pasta_procrev, basename)
  # write_delim(dados_brutos_novos, out_file_procrev, col_names = FALSE, delim = '')
  #
  #
  # # Escrever arquivo resumido, para análises
  # # 320181231235612508020EES485110086
  # # 320181231235647508020       10125
  #
  # # Para análises de velocidades médias - ficam lote sem letra, timestamp, local,
  # # faixa, tipo de registro, placa, tipo de veículo, classificação do veículo,
  # # e velocidade pontual (33 caracteres)
  # dados_analises <- dados %>% mutate(X1 = str_c(substr(X1, 2, 21),
  #                                               substr(X1, 30, 39),
  #                                               substr(X1, 43, 45)))
  #
  # # dados_analises <- dados_analises %>% filter(substr(X1, 21, 21) != '2')
  # # tail(dados) %>% mutate(X1 = str_c(substr(X1, 2, 21), substr(X1, 30, 39), substr(X1, 43, 45)))
  #
  # out_file_analise <- sprintf('%s/VM_%s', pasta_analise, basename)
  # write_delim(dados_analises, out_file_analise, col_names = FALSE, delim = '')
  #
  #
  # # Limpar ambiente
  # rm(out_file_procrev, out_file_analise, dados_analises, dados_brutos_novos)


  # Escrever arquivo final, com todos os tratamentos
  out_file_procrev <- sprintf('%s/REV_%s', pasta_procrev, basename)
  write_delim(dados, out_file_procrev, col_names = FALSE, delim = '')

  # Limpar ambiente
  rm(out_file_procrev)


  # Qual a extensão das strings mais presente na base (qual a moda)?
  # https://www.geeksforgeeks.org/mean-median-and-mode-in-r-programming/
  # Dados possíveis são 52, 53 ou eventualmente 30:
  mode_final <- function() { return(sort(-table(nchar(dados$X1)))[1]) }
  # A moda é o nome da coluna resultante na tabela
  mode_final <- as.integer(names(mode_final()))



  # ----------------------------------------------------------------------------
  # Puxar volumetria por local
  # ----------------------------------------------------------------------------

  # Fazer backup para testar processamento
  # dados2 <- dados

  # Separar string grande de texto em colunas
  dados <-
    dados %>%
    mutate(# lote  = substr(X1, 1, 2),
      data  = substr(X1, 3, 10),
      # hora  = substr(X1, 11, 16), # hora HHMMSS
      # hora  = substr(X1, 11, 12), # só hora completa
      local = substr(X1, 17, 20),
      faixa = substr(X1, 21, 21),
      # n_reg = substr(X1, 22, 29),
      tipo_reg  = substr(X1, 30, 30),
      placa = substr(X1, 31, 37),
      tipo_veic = substr(X1, 38, 38),
      class = substr(X1, 39, 39),
      # comp  = substr(X1, 40, 42),
      # vel_p = substr(X1, 43, 45),
      # tocup = substr(X1, 46, 50),
      vel_m = substr(X1, 51, 53),
    ) %>%
    select(-X1)

  # head(dados)


  # dados %>% select(data) %>% distinct() # devem ser 2 a 4 dias
  # dados %>% select(local) %>% distinct() # cerca de 200-220 locais por lote
  # dados %>% select(faixa) %>% distinct() # deveriam ser poucas, mas pelo visto tem local com até 9 faixas
  # dados %>% select(tipo_reg) %>% distinct() # 0, 1 ou 2
  # dados %>% select(tipo_veic) %>% distinct() # 0, 1, 2, 3, " " (espaço) ou "" (vazio)
  # dados %>% select(class) %>% distinct() # 0, 1, " " (espaço) ou "" (vazio)
  # dados %>% select(vel_m) %>% distinct() # 000, "   " ou "" (vazios)
  qtd_datas <- dados %>% select(data) %>% distinct() %>% nrow()
  qtd_local <- dados %>% select(local) %>% distinct() %>% nrow()
  qtd_faixa <- dados %>% select(faixa) %>% distinct() %>% nrow()
  qtd_tpreg <- dados %>% select(tipo_reg) %>% distinct() %>% nrow()
  qtd_tveic <- dados %>% select(tipo_veic) %>% distinct() %>% nrow()
  qtd_class <- dados %>% select(class) %>% distinct() %>% nrow()
  qtd_vel_m <- dados %>% select(vel_m) %>% distinct() %>% nrow()

  # dados %>% filter(faixa > 5) %>% select(local) %>% distinct()


  # Para criar as volumetrias, onde o tipo_reg é 2, o volume daquele dia será
  # zero (em vez de NA) desde que não haja volume registrado naquele dia e local
  # (tipo_reg != 2). Para isso, separar as duas situações
  tp_reg_2     <- dados %>% filter(tipo_reg == '2') %>% group_by(data, local) %>% tally() %>% ungroup()
  tp_reg_nao_2 <- dados %>% filter(tipo_reg != '2') %>% group_by(data, local) %>% tally() %>% ungroup()

  # NA são as que tiveram 0 veículos registrados
  #   data     local    n.x   n.y
  #   <chr>    <chr>  <int> <int>
  # 1 20190108 6935     69     1
  # 2 20190108 6938     81     1

  #   data     local    n.x   n.y
  #   <chr>    <chr>  <int> <int>
  # 1 20190108 7137     NA     6
  # 2 20190108 7162     NA     3

  # Juntar os dois dados agrupados - com isso, linhas em que n.x ficarem como
  volumes <- full_join(tp_reg_nao_2, tp_reg_2, by = c('data', 'local'))

  # Fazer alteração: demarcar com 0 o volume onde realmente não houve registros
  volumes <- volumes %>% mutate(n = ifelse(!is.na(n.x), n.x, 0)) %>% select(data, local, n)
  # head(volumes)

  # dados %>% sample_n(20)
  # dados %>% filter(substr(X1, 17, 20) == '6824') %>% sample_n(20)

  # Escrever arquivo de volumetria
  out_vol_file <- sprintf('%s/VOL_%s', pasta_volume, str_replace(basename, 'txt', 'csv'))
  write_delim(volumes, out_vol_file, delim = ';')

  # Limpar ambiente
  rm(volumes, tp_reg_nao_2, tp_reg_2, out_vol_file)


  # ----------------------------------------------------------------------------
  # Gravar dados de placas e classificação de veículos
  # ----------------------------------------------------------------------------

  # head(dados)
  placas <- dados %>% filter(placa != '       ' & placa != '')

  # Quardar dados para log
  reg_linhas8 <- nrow(placas)
  perc_placas <- round(reg_linhas8 / reg_linhas7 * 100, 1)

  # Agrupar veículos por placa e tipo de veículo - quantas classificações cada
  # veículo teve?
  placas <- placas %>% group_by(placa, tipo_veic) %>% tally() %>% ungroup()
  placas <- placas %>% pivot_wider(placa, names_from = tipo_veic, values_from = n)
  # sample_n(placas, 20)

  placas <-
    placas %>%
    # sample_n(20) %>%

    # Substituir NAs por zero nas colunas numéricas
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%

    # # Puxar nome da coluna que apresenta o valor máximo por linha - considerar somente colunas numéricas
    # # https://stackoverflow.com/questions/67533157/how-to-use-dplyr-to-get-column-with-max-value-for-each-row
    # # mutate(Class = names(.)[max.col(.)])
    # # mutate(max = names(cur_data())[which.max(c_across(everything()))])
    # mutate(class_pred = names(across(where(is.numeric)))[max.col(across(where(is.numeric)))]) %>%

    # Renomear tipos de veículos, para facilitar
    # rename(moto = `0`, passeio = `1`, onibus = `2`, caminhao = `3`) %>%
    select(placa, moto = `0`, passeio = `1`, onibus = `2`, caminhao = `3`)

  # # Contar quantas colunas possuem zero em cada linha - considerar somente colunas numéricas
  # # https://stackoverflow.com/questions/11797216/count-number-of-zeros-per-row-and-remove-rows-with-more-than-n-zeros
  # # DF[rowSums(DF == 0)]
  # mutate(class_unic = rowSums(across(where(is.numeric)) != 0)) %>%

  # # Calcular acurácia da classificação - para cada linha, somar a quantidade
  # # de ocorrências e calcular a proporção do valor que mais aparece
  # rowwise() %>%
  # mutate(n_ocurr = moto + passeio + onibus + caminhao,
  #        acuracia = round(max(moto, passeio, onibus, caminhao) / n_ocurr * 100, 2)) %>%
  #
  # # Remover o rowwise
  # ungroup() %>%
  #
  # # Reordenar colunas
  # select(placa, moto, passeio, onibus, caminhao, n_ocurr, class_unic, class_pred, acuracia)

  # sample_n(placas, 20)

  # Escrever arquivo de placas
  out_plac_file <- sprintf('%s/PLC_%s', pasta_placas, str_replace(basename, 'txt', 'csv'))
  write_delim(placas, out_plac_file, delim = ';')

  # Limpar ambiente
  rm(placas, out_plac_file)


  # ----------------------------------------------------------------------------
  # Gravar resultados - arquivo de log, volumetria e arquivo padronizado
  # ----------------------------------------------------------------------------

  log <- data.frame(X1  = basename,
                    X2  = qtd_datas, # devem ser 2 a 4 dias
                    X3  = qtd_local, # cerca de 200-220 locais por lote
                    X4  = qtd_faixa, # (até 9) deveriam ser poucas, mas pelo visto tem local com até 9 faixas
                    X5  = qtd_tpreg, # (3) 0, 1 ou 2
                    X6  = qtd_tveic, # (5-6) 0, 1, 2, 3 ou " ", "" (vazios)
                    X7  = qtd_class, # (3-4) 0, 1 ou " ", "" (vazios)
                    X8  = qtd_vel_m, # (2) 000, "   ",  " (vazios)
                    X9  = reg_linhas7,     # qtd final de linhas após checar tipo_reg, tipo_veic, class_veic e primeiro caracter de placa
                    X10 = reg_linhas8,      # qtd de linhas que possuem registro de placas
                    X11 = perc_placas,      # percentual de linhas com placas, frente ao total
                    X12 = mode_final,      # extensão das strings mais presente na base ao final
                    X13 = mode,            # extensão das strings mais presente na base inicial
                    X14 = min_char_linhas, # qtd mínima de caracteres após filtros de remoções
                    X15 = max_char_linhas, # qtd máxima de caracteres após filtros de remoções
                    X16 = reg_linhas1,     # qtd de linhas originais
                    X17 = reg_linhas2,     # qtd de linhas após separação de strings grandes
                    X18 = reg_linhas3,     # qtd de linhas grandes/gigantes que foram descartadas (deve ser tudo lixo)
                    X19 = reg_linhas4,     # qtd de linhas após remoção de linhas duplicadas
                    X20 = reg_linhas5,     # qtd de linhas após remoção de linhas vazias e com caracteres inválidos
                    X21 = reg_linhas6,     # qtd de linhas após remoção das que não começam com L[1-4] ou possuem 30, 52 ou 53 caracteres
                    X22 = max_char1,       # maior extensão inicial de linha
                    X23 = max_char2        # maior extensão de linha após separação de strings grandes
  )

  # head(log)

  # Escrever arquivo de log
  # Se arquivo de log já existe, abri-lo e checar se esta base já foi processada
  if (file.exists(out_log_file)) {
    write_delim(log, out_log_file, delim = ';', append = TRUE)
  } else {
    write_delim(log, out_log_file, delim = ';', append = FALSE)
  }



  # ----------------------------------------------------------------------------
  # Limpeza geral de ambiente, variáveis e eventuais arquivos temporários
  # ----------------------------------------------------------------------------

  # Se variável 'arq_tmp' foi declarada, ou seja, se arquivo teve problema com
  # caracteres nul e um arquivo temporário teve de ser criado para que pudesse
  # ser aberto, apagar este arquivo temporário
  if (file.exists(str_c(arq, '_v2.txt'))) { file.remove(str_c(arq, '_v2.txt')) }

  # Limpar ambiente
  rm(dados, log, basename,
     max_char1, max_char2, max_char_linhas, min_char_linhas,
     reg_linhas1, reg_linhas2, reg_linhas3, reg_linhas4, reg_linhas5,
     reg_linhas6, reg_linhas7, reg_linhas8, perc_placas, mode, mode_final,
     qtd_datas, qtd_local, qtd_faixa, qtd_tpreg, qtd_tveic, qtd_class, qtd_vel_m)

  gc(T)

}



# ----------------------------------------------------------------------------
# Ordenar arquivo de log (caso tenha rodado o script em mais de uma instância)
# ----------------------------------------------------------------------------

# # Abrir arquivo de log e ordenar por data (nome de arquivo)
# log <- read_delim(out_log_file, delim = ';', col_types = cols(.default = "c"))
# log <- log %>% arrange(X1) %>% distinct()
# tail(log) %>% select(X1)
#
# # Sobrescrever arquivo original
# write_delim(log, out_log_file, delim = ';')
#
# # Qual o percentual total de registros de placas frente ao total de registros?
# this <- log %>% mutate(X9  = as.numeric(X9),
#                        X10 = as.numeric(X10))
#
# round(sum(this$X10) / sum(this$X9) * 100, 2)
