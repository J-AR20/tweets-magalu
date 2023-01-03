library(academictwitteR)
library(readr)

# academictwitteR::set_bearer()

base_bruta_distinta <-
  readr::read_rds("dados_brutos/01-baixar-tweets/base_bruta_final.rds")


caminho_base_user <-
  "dados_brutos/03-baixar-autores/base_user.rds"


# buscar autores ------
autores_buscar <- base_bruta_distinta |>
  dplyr::distinct(author_id) |>
  dplyr::pull(author_id) |>
  sort()


possibly_get_user_profile <-
  purrr::possibly(academictwitteR::get_user_profile,
                  otherwise = tibble::tibble(erro = "error"))

# primeira versão da base user
# base_user <- autores_buscar |>
#   sample(100) |>
#   academictwitteR::get_user_profile()

base_user <- readr::read_rds(caminho_base_user)

iterar_get_user <-
  function(base_user,
           autores_buscar,
           amostra = 1000) {
    autores_falta_buscar <-
      autores_buscar[!autores_buscar %in% base_user$id]
    
    amostra_autores <- autores_falta_buscar |>
      sample(amostra)
    
    user_tweets <- possibly_get_user_profile(amostra_autores)
    
    base_user <-
      dplyr::bind_rows(user_tweets, base_user)
    
    base_user
  }


while (!"Too Many Requests" %in% base_user$value) {
  base_user <-
    iterar_get_user(base_user, autores_buscar, amostra = 1000)
  
  base_user |>
    readr::write_rds(caminho_base_user)
  
  print(nrow(base_user))
}


# Arrumando a base
autores_user_arrumado <- base_user |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  tidyr::unnest(cols = c(public_metrics)) |>
  dplyr::select(name:id) |>
  dplyr::mutate(dplyr::across(.names = "author_{.col}")) |>
  dplyr::select(tidyselect::starts_with("author_")) |>
  dplyr::arrange(desc(author_followers_count))

autores_user_arrumado |>
  readr::write_rds("dados_brutos/03-baixar-autores/base_user_final.rds")
