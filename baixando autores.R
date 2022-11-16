library(academictwitteR)
library(readr)

# academictwitteR::set_bearer()

base_bruta <- read_rds("data/base_bruta.rds")

base_metricas <- readr::read_rds("data/base_metricas_final.rds") 


# separando tweets com maior interação
# pensar no critério! testando: tweets com mais de 1000 likes:

metricas_tweets_filtrados <- base_metricas |> 
  dplyr::filter(like_count >= 1000) |> 
  dplyr::select(text:id)

base_bruta_filtrada <- base_bruta |> 
  dplyr::select(
    -c(referenced_tweets, edit_history_tweet_ids,
       entities, public_metrics, geo, attachments, withheld)
  ) |> 
  dplyr::filter(id %in% metricas_tweets_filtrados$id) |> 
  dplyr::distinct(id, .keep_all = TRUE)


autores_buscar <- base_bruta_filtrada |> 
  dplyr::distinct(author_id) |> 
  dplyr::pull(author_id)
  

autores_tweets <- academictwitteR::get_user_profile(autores_buscar)

autores_tweets |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  dplyr::arrange(desc(public_metrics$followers_count)) |> 
  readr::write_rds("data/autores_tweets.rds")
  