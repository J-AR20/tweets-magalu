# separando a lista dos tweets classificados como materias jornalisticas, 'cod_tweet_magalu = 98'

# carregando pacotes
library(dplyr)
library(readr)
library(googlesheets4)
library(writexl)

base_inicial <- readr::read_rds("data/base_tweets_completa.rds")

# buscando perfis de jornais

url_google_sheet <- 'https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=462148650'


perfis_jornalisticos <- googlesheets4::read_sheet(url_google_sheet, "tweets-mais-de-cem-likes") |> 
  dplyr::filter(cod_tweets_magalu == "98") |> 
  dplyr::distinct(autor_username)

# filtrando perfis de jornais
media_tweets <- base_inicial |> 
  dplyr::filter(author_username %in% perfis_jornalisticos$autor_username) |> 
  dplyr::mutate(
    grupos_de_midia = dplyr::case_when(
      author_username %in% c("folha", "UOLNoticias", "UOL") ~ "Grupo Folha",
      author_username %in% c("JornalOGlobo", "jornalextra", "laurojardim", "flaviaol") ~ "Grupo Globo",
      author_username %in% c("Estadao", "BlogdoNoblat") ~ "Grupo Estado",
      author_username %in% c("DCM_online") ~ "Nn&A Produções Jornalísticas",
      author_username %in% c("CNNBrBusiness") ~ "CNN Brasil Novus Mídia",
      author_username %in% c("JornalDaCidadeO") ~ "Jornal da Cidade Online",  
      author_username %in% c("exame") ~ "Editora e Comercio Valongo",  
      author_username %in% c("brasil247") ~ "Brasil 247",  
      author_username %in% c("correio", "em_com") ~ "Diários Associados",
      author_username %in% c("DiarioPE") ~ "Grupo Diario de Pernambuco; Diários Associados",
      author_username %in% c("revistaforum") ~ "Publisher Brasil",
      author_username %in% c("gazetadopovo", "madeleinelacsko") ~ "Gazeta do Povo",
      author_username %in% c("Metropoles") ~ "Grupo Metrópoles",
      author_username %in% c("bbcbrasil") ~ "British Broadcasting Corporation",
      author_username %in% c("Congresso em Foco") ~ "Caracol Web Design",
      author_username %in% c("conexaopolitica") ~ "Conexão Política",
      author_username %in% c("elpais_brasil") ~ "Grupo PRISA",
      author_username %in% c("revistaoeste") ~ "Oeste",
      author_username %in% c("congressoemfoco", "jc_pe") ~ "Sistema Jornal do Commercio de Comunicação",
      author_username %in% c("MidiaNINJA") ~ "Mídia NINJA",
      author_username %in% c("QuebrandoOTabu") ~ "Quebrando o tabu",
      TRUE ~ "CATEGORIZAR"
    ),
    
    proximidade_ideologica_grupos_de_midia = dplyr::case_when(
      grupos_de_midia %in% c("Grupo Folha", "Grupo Globo", "Grupo Estado",
                             "Editora e Comercio Valongo", 
                             "Diários Associados",
                             "Grupo Diario de Pernambuco; Diários Associados",
                             "Sistema Jornal do Commercio de Comunicação") ~ "direita; centro-direita",
      
      grupos_de_midia %in% c("Nn&A Produções Jornalísticas",
                             "Brasil 247",
                             "Publisher Brasil",
                             "Grupo Metrópoles", 
                             "Grupo PRISA",
                             "Mídia NINJA",
                             "Quebrando o tabu"
                             ) ~ "esquerda; centro-esquerda",
      grupos_de_midia %in% c("CNN Brasil Novus Mídia", "Oeste") ~ "direita",
      grupos_de_midia %in% c("British Broadcasting Corporation", "Caracol Web Design") ~ "centro",
      grupos_de_midia %in% c("Jornal da Cidade Online", "Gazeta do Povo", "Conexão Política") ~ "direita; extrema-direita",
      
      TRUE ~ "CATEGORIZAR"
    )
  )
  
writexl::write_xlsx(media_tweets, "data/tweets_jornalisticos.xlsx")

media_tweets_categorizada <- media_tweets |> 
  group_by(grupos_de_midia, proximidade_ideologica_grupos_de_midia) %>% 
  summarise(qtd_tweets = n(),
            soma_like = sum(like_count, na.rm = TRUE),
            soma_retweet = sum(retweet_count, na.rm = TRUE),
            soma_respostas_tweet = sum(reply_count, na.rm = TRUE),
            soma_quotes = sum(quote_count, na.rm = TRUE),
            mean_likes_per_tweet = round(soma_like/qtd_tweets),
            veiculos = paste0(unique(author_name), collapse = ', '),
           veiculos_user = paste0(unique(author_username), collapse = ', ')) %>%
  arrange(desc(mean_likes_per_tweet))


write_xlsx(media_tweets_categorizada, 'data/media_tweets_categorizada.xlsx')



media_tweets_tab_1 <- media_tweets_categorizada |> 
  dplyr::select(grupos_de_midia, veiculos, proximidade_ideologica_grupos_de_midia) |> 
  dplyr::arrange(grupos_de_midia)

write_xlsx(media_tweets_tab_1, 'data/media_tweets_tab_1.xlsx')

  


media_tweets_tab_2 <- media_tweets_categorizada |> 
  dplyr::select(-c(veiculos, veiculos_user, proximidade_ideologica_grupos_de_midia)) |> 
  dplyr::arrange(desc(qtd_tweets))

write_xlsx(media_tweets_tab_2, 'data/media_tweets_tab_2.xlsx')

