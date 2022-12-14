# library(academictwitteR)
library(readr)
# library(writexl)
# library(purrr)
# library(readxl)
library(dplyr)
# library(janitor)
library(ggplot2)
# install.packages('esquisse')
library(esquisse)

# lendo a base completa
base_completa <- read_rds('data/base_tweets_completa.rds') %>% 
  filter(!author_username %in% c('magalu', 'luizatrajano'))
# removi acima os tweets obviamente partidários da magalu e luizatrajano

linha_do_tempo <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1z3oNAxrIYHZkjwVirRxLKiGMQAyxGE8cAemfXvusUUY/edit#gid=1890916712", "linha_do_tempo")


linha_do_tempo_alterada <- linha_do_tempo |> 
  tidyr::separate(evento, sep = ": ", into = c("evento", "desc_evento")) |> 
  dplyr::mutate(data = as.Date(data),
              #  evento = stringr::str_remove(evento, "Evento ")
              )




# Gráfico de tweets ao longo do tempo -------------------------------------


# preparação da base
tweets_por_data <- base_completa %>% 
  mutate(data = as.Date(created_at)) %>%  # ajustando a data
  dplyr::left_join(linha_do_tempo_alterada) |> 
  count(data, evento, desc_evento) # contando quantas linhas existe para cada data


tweets_por_data_com_evento <- tidyr::drop_na(tweets_por_data, evento)

# fazendo o gráfico de data através do pacote esquisse
grafico_linha_do_tempo <- ggplot(tweets_por_data) +
  aes(x = data, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  geom_point(data = tweets_por_data_com_evento, show.legend = FALSE) + 
 ggrepel::geom_label_repel(data = tweets_por_data_com_evento, aes(label = evento), size = 2, show.legend = FALSE, nudge_y = 6000, nudge_x = 2, segment.color = "gray") + 
  labs(
    caption = "\n Fonte: Dados extraídos da API do Twitter usando o pacote academictwitteR",
    y = "Quantidade de Tweets",
    x = "Mês/Ano"
  ) + 
  scale_x_date(date_labels = "%d/%m/%Y") +
  theme_light()

grafico_linha_do_tempo

ggsave(filename = "grafico_linha_do_tempo.png", plot = grafico_linha_do_tempo)
