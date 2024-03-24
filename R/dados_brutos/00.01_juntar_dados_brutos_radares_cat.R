# Estrutura dos nomes dos arquivos e do conteúdo registrado (ver pgs 99 e 100)
# https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/chamadas/anexo-a_1381338248.pdf
# Nome do arquivo:
#   DT - L1 - 6601 - 087216 - 20200901 - 000800 - .TXT
#   DT - L1 - 6601 - 087218 - 20200901 - 001600 - .TXT
#   DT - L1 - 1234 - 123456 - AAAAMMDD - HHMMSS - .TXT
# Conteúdo arquivo:
#   L1 - 20200901 - 000800 - 6601 - 1 - 000000 - 052 -         - 1 - 0 - 000 - 000 - 00000 - 000
#   L1 - 20200901 - 000153 - 6601 - 1 - 000000 - 020 - LMJ8897 - 3 - 0 - 041 - 080 - 01124 - 000
#   L1 - AAAAMMDD - HHMMSS - 1234 - 1 - 123456 - 1 -   AAA9999 - 1 - 1 - 123 - 123 - 12345 - 123


library('tidyverse')


# Nome base para gravar o arquivo
basename <- 'L4_2019'

# Estrutura de nomes e pastas
pasta_base <- '/media/livre/SSD120GB/tmp_brutos_radares4'

# Os arquivos devem ter sido descompactados em uma pasta única, checar
pasta_meses <- list.dirs(pasta_base, full.names = FALSE, recursive = FALSE)
pasta_base_mes  <- sprintf('%s/%s', pasta_base, pasta_meses[[1]])

# Cada dia deve estar em uma pasta única - a quantidade de pastas
pastas_dias <- list.dirs(pasta_base_mes, full.names = FALSE, recursive = FALSE)
# dia <- pastas_dias[[1]]

# Guardar pasta original do script
pasta_orig <- getwd()

# ------------------------------------------------------------------------------
# Processar todas as pastas de cada dia do mês
# ------------------------------------------------------------------------------

for (dia in pastas_dias) {

  # Entrar em cada pasta de dia
  pasta_base_dia <- sprintf('%s/%s', pasta_base_mes, dia)
  print(pasta_base_dia)

  # Juntar todos os arquivos .txt .TXT daquele dia em um só
  find_path <- sprintf("/usr/bin/find")
  find_arg1 <- sprintf("%s", pasta_base_dia)
  find_arg2 <- "-name \"*.[Tt][Xx][Tt]\" -exec"
  cat_path <- sprintf("/bin/cat")
  cat_arg  <- sprintf("{} > %s/%s%s%s.txt", pasta_base, basename, pasta_meses, dia)
  final_arg <- "\\;"
  sprintf('%s %s %s %s %s %s', find_path, find_arg1, find_arg2, cat_path, cat_arg, final_arg)
  system2(command = find_path, args = c(find_arg1, find_arg2, cat_path, cat_arg, final_arg))

  # Limpar ambiente
  # rm(find_path, find_arg1, find_arg2, cat_path, cat_arg, final_arg, dia)

  # Remover a pasta do dia recém processada
  unlink(pasta_base_dia, recursive = TRUE)

}

# for (dia in pastas_dias) {
#
#   # Entrar em cada pasta de dia
#   pasta_base_dia <- sprintf('%s/%s', pasta_base_mes, dia)
#   print(pasta_base_dia)
#
#   setwd(pasta_base_dia)
#
#
#   # Juntar todos os arquivos .txt .TXT daquele dia em um só
#   find_path <- sprintf("/usr/bin/find")
#   find_arg <- ". -name \"*.[Tt][Xx][Tt]\" -exec"
#   cat_path <- sprintf("/bin/cat")
#   cat_arg  <- sprintf("{} > ../../%s%s%s.txt", basename, pasta_meses, dia)
#   final_arg <- "\\;"
#   sprintf('%s %s %s %s %s', find_path, find_arg, cat_path, cat_arg, final_arg)
#   system2(command = find_path, args = c(find_arg, cat_path, cat_arg, final_arg))
#
#   # Limpar ambiente
#   # rm(find_path, find_arg, cat_path, cat_arg, final_arg)
#
#   # Voltar para a pasta original
#   setwd(pasta_orig)
# }



# ------------------------------------------------------------------------------
# Transformar arquivos de texto em .csv com separação de colunas
# ------------------------------------------------------------------------------

# # Entrar na pasta_base
# setwd(pasta_base)
#
# # Arquivos de texto processados
# txt_files <- list.files(pasta_base, pattern = '*\\.txt', full.names = FALSE)
# this_file <- txt_files[[1]]
#
# # Processar todos os arquivos de texto
# for (txt in txt_files) {
#
#   # Abrir arquivo txt
#   this <-  read_delim(txt, delim = ';', col_names = FALSE, col_types = cols(.default = "c"))
#
#   # Remover linhas com problemas de encoding
#   this <- this %>% filter(validUTF8(X1))
#
#   # Separar string grande de texto em colunas
#   this <-
#     this %>%
#     mutate(lote  = substr(X1, 1, 2),
#            data  = substr(X1, 3, 10),
#            hora  = substr(X1, 11, 16),
#            local = substr(X1, 17, 20),
#            faixa = substr(X1, 21, 21),
#            n_reg = substr(X1, 22, 29),
#            t_reg = substr(X1, 30, 30),
#            placa = substr(X1, 31, 37),
#            tipo  = substr(X1, 38, 38),
#            class = substr(X1, 39, 39),
#            comp  = substr(X1, 40, 42),
#            vel_p = substr(X1, 43, 45),
#            tocup = substr(X1, 46, 50),
#            vel_m = substr(X1, 51, 53)) %>%
#     select(-X1)
#
#
#   # Salvar arquivo .csv
#   out_name <- str_replace(txt, '\\.txt', '.csv')
#   out_file <- sprintf('%s/%s', pasta_base, out_name)
#   write_delim(this, out_file, delim = ';')
#
#   # Limpar ambiente
#   # rm(this, out_name, out_file, txt)
#
#   # Apagar arquivos .txt da pasta
#   file.remove(txt)
#
# }
#
#
# # Voltar para a pasta original
# setwd(pasta_orig)


# setwd(pasta_base)
# test_file <- 'L3_20191202.csv'
# this <-  read_delim(test_file, delim = ';', col_types = cols(.default = "c"))
# this <- this %>% select(-n_seq)
# write_delim(this, test_file, delim = ';')
# setwd(pasta_orig)


# -----------------------------------------------------------------------------
# Compactar resultados
# -----------------------------------------------------------------------------

# Remover a pasta base do mês recém processada
unlink(pasta_base_mes, recursive = TRUE)

# Padrão de arquivos por mês
basename_mes <- sprintf('%s%s', basename, pasta_meses)

# # Compactar arquivos - 7z
# sevenzip_path <- sprintf("/usr/bin/7z")
# arg1 <- "a -mx=9"
# arg2 <- sprintf("%s/%s.7z", pasta_base, basename_mes)
# arg3 <- sprintf("%s/%s*.csv", pasta_base, basename_mes)
# sprintf('%s %s %s %s', sevenzip_path, arg1, arg2, arg3)
# system2(command = sevenzip_path, args = c(arg1, arg2, arg3))

# Compactar arquivos - Zip
zip_path <- sprintf("/usr/bin/zip")
arg1 <- sprintf("%s/%s.zip", pasta_base, basename_mes)
arg2 <- sprintf("%s/%s*.txt", pasta_base, basename_mes)
sprintf('%s %s %s', zip_path, arg1, arg2)
system2(command = zip_path, args = c(arg1, arg2))
