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
  filter(!author_username %in% c('magalu', 'luizatrajano', 'HaddadDebochado', 'direitasiqueira', 'LUIZPATRIOTA39'))
# removi acima os tweets obviamente partidários da magalu e luizatrajano, e aqueles de perfis de humor 

# Gráfico de tweets ao longo do tempo -------------------------------------


# preparação da base
tweets_por_data <- base_completa %>% 
  mutate(data = as.Date(created_at)) %>%  # ajustando a data
  count(data) # contando quantas linhas existe para cada data

# fazendo o gráfico de data através do pacote esquisse
ggplot(tweets_por_data) +
  aes(x = data, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    caption = "\n Fonte: Dados extraídos da API do Twitter usando o pacote academictwitteR",
    y = "Quantidade de Tweets",
    x = "Mês/Ano"
  ) + 
  theme_light()
