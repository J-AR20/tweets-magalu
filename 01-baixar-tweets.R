# código com Jonas :)
# colocar aqui depois.

# teste

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
