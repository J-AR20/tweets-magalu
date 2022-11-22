# código com Jonas :)
# colocar aqui depois.

# carregando pacotes
library(academictwitteR)
library(readr)
library(writexl)
library(purrr)
library(readxl)
library(dplyr)
library(janitor)

# Lista de palavras chave para busca dos tweets criada a partir de contagem de palavras dos resultados
# encontrados no google para o período pesquisa. 

vetor_queries <- c('#MagazineLuizaRacista',
                   '(magalu OR magazineluiza) negro',
                   '(magalu OR magazineluiza) racismo',
                   '(magalu OR magazineluiza) empresa',
                   '(magalu OR magazineluiza) programa',
                   '(magalu OR magazineluiza) ação',
                   '(magalu OR magazineluiza) branco',
                   '(magalu OR magazineluiza) igualdade',
                   '(magalu OR magazineluiza) afirmativo',
                   'racismo reverso')

vetor_queries_dois <- c('(magalu OR magazineluiza) racial',
                        '(magalu OR magazineluiza) afirmativa',
                        '(magalu OR magazineluiza) racista',
                        '(magalu OR magazineluiza) trainee',
                        '(magalu OR magazineluiza) desigualdade',
                        '(magalu OR magazineluiza) discriminação',
                        '(magalu OR magazineluiza) inclusão',
                        '(magalu OR magazineluiza) diversidade',
                        '(magalu OR magazineluiza) lacrar',
                        '(magalu OR magazineluiza) mérito')


# criação de uma função para automatizar a busca das palavras-chave: 

busca_magalu <- function(palavra_chave) { 
  tweets <- get_all_tweets(  
    query = palavra_chave,
    start_tweets = "2020-09-18T00:00:00Z",
    end_tweets = "2020-10-18T00:00:00Z",
    n = 80000
  ) %>% 
    mutate(query = palavra_chave, data_pesquisa = Sys.Date())
  write_xlsx(tweets, paste0('data/dados_magalu/tweets_', janitor::make_clean_names(palavra_chave), '.xlsx'))
  print(paste0('download realizado para query ', palavra_chave))
  tweets
}

# Testando o limite da base de dados com a query possivelmente mais utilizada para valores
# a partir de 50.000 tweets

busca_magalu('#MagazineLuizaRacista')

# Obtive um pouco menos de 20.000 respostas. Vou utilizar esse valor como parâmetro de n= na função.

# APLICANDO FINALMENTE a função busca_magalu() para todas as queries criadas no vetor_query

vetor_queries %>% map(busca_magalu)

# APLICANDO a função busca_magalu() para todas as queries criadas no vetor_query_dois

vetor_queries_dois %>% map(busca_magalu)

# A única query que bateu o teto dos 30.000 tweets foi o "racismo_reverso". Vou refazer a pesquisa 
# para essa palavra-chave e colocar o teto mais alto de 80.000.

busca_magalu('racismo reverso')

# Também chegou no teto dos 50.000 tweets. Vou passar agora para 80.000

busca_magalu('racismo reverso')

# Unir os diferentes resultados de pesquisas feitas com palavras-chave diferentes, numa única base

teste_base_raw <- list.files('data/dados_magalu', full.names = TRUE, pattern = 'xlsx') %>%  # busca os caminhos de vários arquivos
  map(read_xlsx) %>%  # 
  bind_rows()   %>% 
  distinct_at(vars(-query, -data_pesquisa), .keep_all = TRUE)  # tira as linhas duplicadas

# Salvar a base final - tirar repeticoes
library(readr)

base_bruta <- read_rds("data/base_bruta.rds")

base_bruta_distinta <- base_bruta |> 
  dplyr::select(
    -c(referenced_tweets, edit_history_tweet_ids,
       entities, public_metrics, geo, attachments, withheld)
  ) |> 
  dplyr::distinct(id, .keep_all = TRUE) 

write_rds(base_bruta_distinta, "data/base_bruta_final.rds")
