# juntar a base categorizada com a base bruta

# carregando pacotes
library(dplyr)
library(readr)
library(googlesheets4)
library(janitor)
library(writexl)
library(stringr)
# install.packages('abjutils')
library(abjutils)

# lendo a base completa
base_completa <-
  read_rds('dados_brutos/04-unir-bases/base_tweets_completa.rds')

url_google_sheet <-
  'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_categorizada <-
  googlesheets4::read_sheet(url_google_sheet, 'tweets-mais-de-cem-likes')

nome_categoria <-
  googlesheets4::read_sheet(url_google_sheet, 'definicao-categoria') %>%
  janitor::clean_names()

base_categorizada_completa <-
  dplyr::left_join(base_categorizada, base_completa, by = 'id') %>%
  dplyr::left_join(nome_categoria, by = "cod_tweets_magalu") %>%
  dplyr::filter(
    !autor_username %in% c(
      'magalu',
      'luizatrajano',
      'HaddadDebochado',
      'direitasiqueira',
      'LUIZPATRIOTA39'
    ),
    orientacao_categoria != '-'
  ) %>%
  dplyr::mutate(cod_tweets_magalu = as.character(cod_tweets_magalu))

sumarizacao_categorias <- base_categorizada_completa %>%
  dplyr::group_by(orientacao_categoria, cod_tweets_magalu, categoria) %>%
  dplyr::summarise(
    qtd_tweets = dplyr::n(),
    soma_like = sum(like_count, na.rm = TRUE),
    soma_retweet = sum(retweet_count, na.rm = TRUE),
    soma_respostas_tweet = sum(reply_count, na.rm = TRUE),
    soma_quotes = sum(quote_count, na.rm = TRUE),
    usuarios = paste0(unique(autor_username), collapse = ', ')
  ) %>%
  dplyr::arrange(desc(soma_like))

readr::write_rds(
  base_categorizada_completa,
  'dados_output/06-explorando-categorias/base_categorizada_completa.rds'
)

writexl::write_xlsx(
  sumarizacao_categorias,
  'dados_output/06-explorando-categorias/sumaricao_categorias.xlsx'
)



# Jonas explorando a base: ---
# separando a lista apenas com os posicionamentos contrários da base_categorizada_completa

library(tidyverse)
library(writexl)
library(googlesheets4)

tweets_contrarios <- base_categorizada_completa %>%
  filter(orientacao_categoria != 'favoravel') %>%
  filter(orientacao_categoria != 'outros') %>%
  select(id, autor_username, author_description)

write_xlsx(
  tweets_contrarios,
  'dados_output/06-explorando-categorias/tweets_contrarios.xlsx'
)

# separando a lista apenas com os posicionamentos favoraveis da base_categorizada_completa

tweets_favoraveis <- base_categorizada_completa %>%
  filter(orientacao_categoria != 'contrario') %>%
  filter(orientacao_categoria != 'outros') %>%
  select(id, autor_username, author_description)

write_xlsx(
  tweets_favoraveis,
  'dados_output/06-explorando-categorias/tweets_favoraveis.xlsx'
)

# procurando perfis em mais de uma categoria

contrarios <- read_sheet(url_google_sheet, 'tweets_contrarios')
favoraveis <- read_sheet(url_google_sheet, 'tweets_favoraveis')

# vou juntar as bases para descobrir quais nomes se repetem a fim de reavaliar a classificação de cada um:
base_posicionamento_completa <-
  left_join(contrarios, favoraveis, by = 'autor_username')
