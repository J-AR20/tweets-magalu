library(academictwitteR)
library(readr)


# academictwitteR::set_bearer()

base_bruta <- read_rds("data/base_bruta.rds")

base_metricas <- readr::read_rds("data/base_metricas.rds") |>
  dplyr::filter(value != "Too Many Requests" | is.na(value))

# funcao  para buscar métricas

get_metrics <- function(id_tweet) {
  id_tweets <- id_tweet |> paste0(collapse = ",")
  
  content_response <- httr::GET(
    url = glue::glue(
      "https://api.twitter.com/2/tweets?ids={id_tweets}&tweet.fields=public_metrics&expansions=attachments.media_keys&media.fields=public_metrics"
    ),
    httr::add_headers(
      Authorization = glue::glue('Bearer {academictwitteR::get_bearer()}')
    )
  ) |> httr::content(simplifyDataFrame = TRUE)
  
  if (!is.null(content_response$data)) {
    content_response |>
      purrr::pluck("data") |>
      tibble::as_tibble() |>
      tidyr::unnest(cols = c(public_metrics, edit_history_tweet_ids))
  } else {
    content_response |>
      purrr::pluck(1) |>
      tibble::as_tibble()
    
  }
}

# Iterar nas

possibly_get_metrics <-
  purrr::possibly(get_metrics, otherwise = tibble::tibble(erro = "error"))


iterar_get_metrics <-
  function(base_metricas, base_bruta, amostra = 100) {
    amostra_ids <- base_bruta |>
      dplyr::filter(!id %in% c(base_metricas$id, base_metricas$value)) |>
      dplyr::distinct(id) |>
      dplyr::pull(id) |>
      sample(amostra)
    
    metricas_tweets <- possibly_get_metrics(amostra_ids)
    
    base_metricas_final <-
      dplyr::bind_rows(metricas_tweets, base_metricas)
    
    base_metricas_final
  }




while (!"Too Many Requests" %in% base_metricas$value) {
  base_metricas <- iterar_get_metrics(base_metricas, base_bruta) |>
    dplyr::filter(is.na(erro))
  
  base_metricas |>
    readr::write_rds("data/base_metricas.rds")
  
  print(nrow(base_metricas))
  Sys.sleep(15)
}

beepr::beep()


base_metricas_final <- base_metricas |>
  dplyr::mutate(id_tweet = dplyr::coalesce(id, value)) |>
  dplyr::distinct(id_tweet, .keep_all = TRUE) |>
  dplyr::arrange(desc(like_count))

base_metricas_final |>
  readr::write_rds("data/base_metricas_final.rds")
