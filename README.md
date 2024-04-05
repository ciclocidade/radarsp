# radares_sp: Baixe dados de volumetria e velocidade dos radares da cidade de São Paulo [DEFINIR LOGO]

**radares_sp** é um pacote que disponibilza dados de volumetria e velocidade dos radares (equipamentos de fiscalização eletrônica) da cidade de São Paulo. O pacote se apoia na [plataforma Arrow](https://arrow.apache.org/docs/r/), que permite que as pessoas trabalhem com dados maiores-do-que-a-memória RAM de seus computadores usando as funções do pacote [{dplyr}](https://arrow.apache.org/docs/r/articles/arrow.html#analyzing-arrow-data-with-dplyr). 


## Instalação

```R
# install from CRAN
install.packages("radares_sp")

# or use the development version with latest features
utils::remove.packages('radares_sp')
remotes::install_github("ciclocidade/radares_sp", ref="dev")
library(censobr)
```


## Uso

The package currently includes 6 main functions to download & read census data:

1. `read_day()`
2. `read_hour()`
3. `read_15min()`

**radares_sp** inclui também uma função de suporte que ajudam as pessoas a entenderem a origem dos dados, apresentados informação detalhada sobre cada radar (equipamento de fiscaliza'ão eletrônica):

4. `dicionario_radares_sp()`

Finalmente, o pacote possui duas funções que ajudam as pessoas a gestionar o dados armazenados (*cached*) localmente.

10. `radares_sp_cache()` 
11. `set_radares_sp_cache_dir()`

A sintaxe de todas as funções para ler dados do pacote **radares_sp** operam sobre a mesma lógica, de forma que é simples e intuitivo baixar dados agregados em diferentes frequências temporais com apenas uma linha de código. Assim:

```
read_hour(
  start                  # início do intervalo de tempo (dia) no formato "YYYY/MM/DD"
  end = NULL             # fim do intervalo de tempo (dia) no formato "YYYY/MM/DD"
  id_to_filter = NULL.   # identificador(es) de radar(es) para restringir a busca
  as_data_frame = FALSE  # retorna um conjunto de dados Arrow ou um data.frame
  show_progress = TRUE   # mostra barra de progresso do download
  cache = TRUE           # armazena dados localmente para acesso mais veloz no futuro
  )
```

***Nota:*** todos os conjunto de dados do **radares_sp** possuem o identificador único do local de fiscalização. Esse `id` pode ser integrado à tabela gerada pela função `dicionario_radares_sp()`, que traz as coordenadas geográficas de cada local. Isso permite que as pessoas integrem esses dados a outras bases georreferenciadas, como as disponibilizadas no portal [geosampa](https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx).

### Armazenamento local de dados

A primeira vez que as pessoas usarem alguma função, o pacote **radares_sp** vai baixar dados e armazena-los localmente. Assim, os dados só precisam ser baixados uma única vez. Quando o parâmetro `cache` está definido como `TRUE` (escolha padrão), a função irá ler os dados que já foram armazenados, tornando a resposta muito mais rápida. 

- `radares_sp_cache()`: pode ser usada para listar e/ou remover dados armazenados localmente
- `set_radares_sp_cache_dir()`: pode ser usada para definir uma pasta personalizada para o armazenamento dos dados

## Dados maiores-do-que-a-memória

Para grandes intervalos de tempo, e alta frequência temporal (15 minutos ou 1 hora), os dados de volumetria e velocidade da cidade de São Paulo podem ser muito grandes para serem lidos e carregados na memória RAM do computador das pessoas. Para evitar esse problema, o padrão do pacote **radares_sp** será retornar uma tabela (ou lista de tabelas) no formato [Arrow](https://arrow.apache.org/docs/r/articles/arrow.html#tabular-data-in-arrow), que pode ser analisada como um `data.frame` normal usando o pacote `dplyr` sem carregar todos os dados na memória.

Mais informações podem ser encontradas nas [vignette](https://ipeagit.ciclocidade/radares_sp/) do pacote.


## Contribua com o **radares_sp**
Se você quiser contribuir com **radares_sp**, te pedimos que abra um *issue* explicando sua ideia de contribuição. 

-----

## Créditos <img align="right" src="man/figures/logo_ciclo.jpeg?raw=true" alt="logo" width="180"></img>

O pacote **radares_sp** é desenvolvido por um time de pesquisadores e pesquisadoras afiliados a [Ciclocidade](https://www.ciclocidade.org.br), a Associação dos Ciclistas Urbanos de São Paulo. Se você quiser citar o pacote, você pode usar a seguinte citação:

- Pacheco, Tainá S.; Soares, Flávio (2024) radares_sp: Baixe dados dos equipamentos de fiscalização eletrônica da cidade de São Paulo. R package version v0.0.1, INSERIR LINK PARA O CRAN.

```
bibentry(
  bibtype  = "Manual",
  title       = "radares_sp: Baixe dados dos equipamentos de fiscalização eletrônica da cidade de São Paulo",
  author      = "Tainá S. Pacheco [aut, cre] and Flávio Soares [aut]",
  year        = 2024,
  version     = "v0.0.1",
  url         = "INSERIR LINK PARA O CRAN",
  textVersion = "Pacheco, Tainá S.; Soares, Flávio (2024) radares_sp: Baixe dados dos equipamentos de fiscalização eletrônica da cidade de São Paulo. R package version v0.0.1, INSERIR LINK PARA O CRAN."
)

```
