base_completa <- readr::read_rds("data/base_tweets_completa.rds")

# separando tweets com maior interação
# pensar no critério! testando: tweets com mais de 1000 likes:

tweets_filtrados <- base_completa |> 
  dplyr::filter(like_count >= 1000)



# Criar a tabela para colar no google sheets

tweets_filtrados |> 
  dplyr::select(id, author_username, author_name, text) |> 
  writexl::write_xlsx("data/tweets-pops-para-categorizar.xlsx")
