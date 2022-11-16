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


# buscar autores ------
autores_buscar <- base_bruta_filtrada |> 
  dplyr::distinct(author_id) |> 
  dplyr::pull(author_id)
  

autores_tweets <- academictwitteR::get_user_profile(autores_buscar)

autores_tweets_arrumado <- autores_tweets |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  dplyr::arrange(desc(public_metrics$followers_count)) 

autores_tweets_arrumado |> 
  readr::write_rds("data/autores_tweets.rds")


# unir bases -----
library(readr)

autores <- readr::read_rds("data/autores_tweets.rds") |> 
  tidyr::unnest(cols = c(public_metrics)) |> 
  janitor::clean_names() |> 
  dplyr::select(username:url, location) |> 
  dplyr::mutate(
    dplyr::across(.names = "autor_{.col}")) |> 
  dplyr::select(tidyselect::starts_with("autor_"))


base_tweets_mais_pops <- dplyr::left_join(base_bruta_filtrada, metricas_tweets_filtrados,
                 by = c("id")) |>
  dplyr::left_join(autores, by = c("author_id" = "autor_id")) |>
  dplyr::arrange(desc(like_count))


base_tweets_mais_pops |> write_rds("data/base_tweets_mais_pops.rds")  
