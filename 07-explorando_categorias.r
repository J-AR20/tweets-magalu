# juntar a base categorizada com a base bruta

# carregando pacotes
library(dplyr)
library(readr)
library(googlesheets4)
library(janitor)

# lendo a base completa
base_completa <- read_rds('data/data-bia/base_tweets_completa.rds') %>% 
  filter(!author_username %in% c('magalu', 'luizatrajano')) # remover tweets magalu e luizatrajano

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_categoriza <- read_sheet(url_google_sheet, 'tweets-mais-de-cem-likes')
nome_categoria <- read_sheet(url_google_sheet, 'definicao-categoria') %>% 
  clean_names()

base_categorizada_completa <- left_join(base_categoriza, base_completa, by = 'id') %>% 
  left_join(nome_categoria, by = "cod_tweets_magalu") %>% 
  filter(!autor_username %in% c('magalu', 'luizatrajano'), orientacao_categoria != '-') %>% 
  mutate(cod_tweets_magalu = as.character(cod_tweets_magalu)) 

base_categorizada_completa %>% 
  group_by(orientacao_categoria, cod_tweets_magalu, categoria) %>% 
  summarise(qtd_tweets = n(),
            soma_like = sum(like_count),
            soma_retweet = sum(retweet_count),
            soma_respostas_tweet = sum(reply_count),
            soma_quotes = sum(quote_count),
            usuarios = paste0(unique(autor_username), collapse = ', ')) %>% 
  arrange(desc(soma_like)) %>% 
  View()
