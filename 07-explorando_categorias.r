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
base_completa <- read_rds('data/base_tweets_completa.rds') 

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_categorizada <- read_sheet(url_google_sheet, 'tweets-mais-de-cem-likes')

nome_categoria <- read_sheet(url_google_sheet, 'definicao-categoria') %>% 
  clean_names()

base_categorizada_completa <- left_join(base_categorizada, base_completa, by = 'id') %>% 
  left_join(nome_categoria, by = "cod_tweets_magalu") %>% 
  filter(!autor_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39'), 
         orientacao_categoria != '-') %>% 
  mutate(cod_tweets_magalu = as.character(cod_tweets_magalu)) 

sumarizacao_categorias <- base_categorizada_completa %>% 
  group_by(orientacao_categoria, cod_tweets_magalu, categoria) %>% 
  summarise(qtd_tweets = n(),
            soma_like = sum(like_count, na.rm = TRUE),
            soma_retweet = sum(retweet_count, na.rm = TRUE),
            soma_respostas_tweet = sum(reply_count, na.rm = TRUE),
            soma_quotes = sum(quote_count, na.rm = TRUE),
            usuarios = paste0(unique(autor_username), collapse = ', ')) %>% 
  arrange(desc(soma_like))

write_rds(base_categorizada_completa, 'data/base_categorizada_completa.rds')

write_xlsx(sumarizacao_categorias, 'data/sumaricao_categorias.xlsx')



# Jonas explorando a base: 
# separando a lista apenas com os posicionamentos contrários da base_categorizada_completa

tweets_contrarios <- base_categorizada_completa %>% 
  filter(orientacao_categoria != 'favoravel') %>% 
  filter(orientacao_categoria != 'outros') %>% 
  select(id, autor_username, author_description)

write_xlsx(tweets_contrarios, 'data/tweets_contrarios.xlsx')

# separando a lista apenas com os posicionamentos favoraveis da base_categorizada_completa

tweets_favoraveis <- base_categorizada_completa %>% 
  filter(orientacao_categoria != 'contrario') %>% 
  filter(orientacao_categoria != 'outros') %>% 
  select(id, autor_username, author_description)

write_xlsx(tweets_favoraveis, 'data/tweets_favoraveis.xlsx')

# procurando perfis em mais de uma categoria

contrarios <- read_sheet(url_google_sheet, 'tweets_contrarios')
favoraveis <- read_sheet(url_google_sheet, 'tweets_favoraveis')

# vou juntar as bases para descobrir quais nomes se repetem a fim de reavaliar a classificação de cada um:
base_posicionamento_completa <- left_join(contrarios, favoraveis, by = 'autor_username')

