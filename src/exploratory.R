library(tidyverse)
library(here)
library(readxl)
library(ggthemes)

# Cargo df
df_deflactado <- read_excel(here::here("data/df_deflactado.xlsx")) 

# Df con divisiones filtradas
df_filtrado <- df_deflactado %>% 
  filter(!division %in% c('10', '19', '20', '17', '26', '11 y 12')) 

# Df con inputs en logs
df_log <- df_deflactado %>% 
  rowwise() %>% 
  mutate(y  = log(y),
         x  = log(sum(ci, k, l)),
         k  = log(k),
         l  = log(l),
         ci = log(ci)) %>% 
  filter(!division %in% c('10', '19', '20', '17', '26', '11 y 12')) 

# Grafico
df_log %>% 
  ggplot(aes(x=x, y=y, color=division)) +
  geom_point() +
  labs(y="Log del VBP", x='Log de los Inputs') + 
  ggtitle('Scatterplot de Output e Inputs según división') +
  theme_bw()



