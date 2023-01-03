library(academictwitteR)
library(readr)

# academictwitteR::set_bearer()

base_bruta <-
  read_rds("dados_brutos/01-baixar-tweets/base_bruta_final.rds")

base_metricas <-
  readr::read_rds("dados_brutos/02-baixar-metricas/base_metricas_final.rds")

base_users <-
  readr::read_rds("dados_brutos/03-baixar-autores/base_user_final.rds")


base_tweets_completa <- dplyr::left_join(base_bruta, base_metricas,
                                         by = c("id")) |>
  dplyr::left_join(base_users, by = c("author_id" = "author_id")) |>
  dplyr::arrange(desc(like_count)) |>
  dplyr::select(
    text = text.x,
    author_id:id,
    query,
    data_pesquisa,
    retweet_count:quote_count,
    author_name:author_created_at
  ) |>
  # removi os tweets obviamente partidários da magalu e luizatrajano,
  # e aqueles de perfis de humor
  dplyr::filter(
    !author_username %in% c(
      'magalu',
      'luizatrajano',
      'HaddadDebochado',
      'direitasiqueira',
      'LUIZPATRIOTA39',
      'HistoriaNoPaint',
      'nerdclassico'
    ),
    !author_name %in% c("Haddad Debochado")
  )

base_tweets_completa |> write_rds("dados_brutos/04-unir-bases/base_tweets_completa.rds")
