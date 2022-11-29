# lendo a base completa
base_completa <- read_rds('data/data-bia/base_tweets_completa.rds') %>% 
  filter(!author_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39')) 
# removi acima os tweets obviamente partidários da magalu e luizatrajano, e aqueles de perfis de humor

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'

base_categorizada <- read_sheet(url_google_sheet, 'tweets-mais-de-cem-likes')
nome_categoria <- read_sheet(url_google_sheet, 'definicao-categoria') %>% 
  clean_names()

base_categorizada_completa <- left_join(base_categorizada, base_completa, by = 'id') %>% 
  left_join(nome_categoria, by = "cod_tweets_magalu") %>% 
  filter(!autor_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39'), 
         orientacao_categoria != '-') %>% 
  mutate(cod_tweets_magalu = as.character(cod_tweets_magalu)) 
  
  # separando a lista dos tweets classificados como materias jornalisticas, 'cod_tweet_magalu = 98'

media_tweets <- base_categorizada_completa %>% 
  filter(cod_tweets_magalu == '98') %>%  
  group_by(grupos_de_midia, proximidade_ideologica_grupos_de_midia) %>% 
  summarise(qtd_tweets = n(),
            soma_like = sum(like_count),
            veiculos = paste0(unique(autor_name), collapse = ', ')) %>%
  arrange(desc(soma_like)) %>% 
  as.data.frame()

write_xlsx(media_tweets, 'data/media_tweets.xlsx')
