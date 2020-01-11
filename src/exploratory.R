library(tidyverse)

# Cargo df
df_deflactado <- read_excel(here::here("data/df_deflactado.xlsx")) 

# DF CON INPUTS
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
  ggplot(aes(x=ci, y=y, color=division)) +
  geom_point()
