library(academictwitteR)
library(readr)

# academictwitteR::set_bearer()

base_bruta <- read_rds("data/base_bruta_final.rds")

base_metricas <- readr::read_rds("data/base_metricas_final.rds") 

base_users <- readr::read_rds("data/base_user_final.rds") 


base_tweets_completa <- dplyr::left_join(base_bruta, base_metricas,
                                          by = c("id")) |>
  dplyr::left_join(base_users, by = c("author_id" = "author_id")) |>
  dplyr::arrange(desc(like_count))

# TODO: REMOVER COLUNAS DESNECESSÁRIAS


base_tweets_completa |> write_rds("data/base_tweets_completa.rds")  
