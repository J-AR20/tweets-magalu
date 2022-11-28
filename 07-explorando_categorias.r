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
base_completa <- read_rds('data/data-bia/base_tweets_completa.rds') %>% 
  filter(!author_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39')) 
# removi acima os tweets obviamente partidários da magalu e luizatrajano, e aqueles de perfis de humor

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_categoriza <- read_sheet(url_google_sheet, 'tweets-mais-de-cem-likes')
nome_categoria <- read_sheet(url_google_sheet, 'definicao-categoria') %>% 
  clean_names()

base_categorizada_completa <- left_join(base_categoriza, base_completa, by = 'id') %>% 
  left_join(nome_categoria, by = "cod_tweets_magalu") %>% 
  filter(!autor_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39'), 
         orientacao_categoria != '-') %>% 
  mutate(cod_tweets_magalu = as.character(cod_tweets_magalu)) 

sumarizacao_categorias <- base_categorizada_completa %>% 
  group_by(orientacao_categoria, cod_tweets_magalu, categoria) %>% 
  summarise(qtd_tweets = n(),
            soma_like = sum(like_count),
            soma_retweet = sum(retweet_count),
            soma_respostas_tweet = sum(reply_count),
            soma_quotes = sum(quote_count),
            usuarios = paste0(unique(autor_username), collapse = ', ')) %>% 
  arrange(desc(soma_like))

write_xlsx(sumarizacao_categorias, 'data/sumaricao_categorias.xlsx')

# DETECTANDO A EXTREMA-DIREITA --------------------------------------------

extrem_words <- c('patriota', 'bolsonaro', 'armamentista', 'anti-esquerda', 
                  'cristao de direita', 'cristao', 'deus, familia, patria','direita', 'anti comunas', 
                  'desesquerdizando', 'liberdade', 'bolsonarista', 'brasil acima de tudo e deus acima de todos', 
                  'pro-vida','e conhecerao a verdade, e a verdade os libertara','conservador', '2️⃣2️⃣' ) %>% 
  paste0(collapse = '|')

base_categorizada_completa %>% 
  filter(orientacao_categoria != 'outros') %>% 
  mutate(author_description_clean = str_to_lower(author_description) %>% rm_accent(),
         author_description_extrem = str_detect(author_description_clean, extrem_words))

base_categorizada_completa %>% 
  filter(orientacao_categoria != 'outros') %>% 
  mutate(author_name_clean = str_to_lower(author_name) %>% rm_accent(),
         author_name_extrem = str_detect(author_name_clean, extrem_words)) %>% 
  View()
