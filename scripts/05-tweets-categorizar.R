base_completa <-
  readr::read_rds("dados_brutos/04-unir-bases/base_tweets_completa.rds")

# separando tweets com maior interação
# o critério usado foi: tweets com mais de 100 likes:

tweets_filtrados <- base_completa |>
  dplyr::filter(!author_username %in% c('magalu', 'luizatrajano')) |>  # remover tweets magalu e luizatrajano
  dplyr::filter(like_count >= 100)

# Criar a tabela para colar no google sheets

tweets_filtrados |>
  dplyr::select(id, author_username, author_name, text) |>
  writexl::write_xlsx("dados_output/05-tweets-categorizar/tweets-para-categorizar.xlsx")


# A CATEGORIZAÇÃO FOI FEITA NO GOOGLE SHEETS PELO JONAS