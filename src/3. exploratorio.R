library(dplyr)
library(ggplot2)
library(ggthemes)
library(here)

#### DF EXPLORATORIO DE LA BASE DE DATOS ####
# Cargo df
df_deflactado <- readRDS(here::here("data/df_deflactado.rds")) 

# Df con divisiones filtradas
df_filtrado <- df_deflactado %>% 
  filter(!division %in% c('10', '19', '20', '17', '26', '11 y 12')) 

# saveRDS(df_filtrado, "data/df_filtrado.rds")

# Df con inputs en logs
df_log <- df_deflactado %>%
  rowwise() %>%
  mutate(y  = log(y),
         x  = log(sum(ci, k, l)),
         k  = log(k),
         l  = log(l),
         ci = log(ci)) %>%
  filter(!division %in% c('10', '19', '20', '17', '26', '11 y 12'))

# Df agrupado para toda la industria
df_agrupado <- df_deflactado %>%
  group_by(anio) %>%
  summarise(y  = sum(y),
            k  = sum(k),
            l  = sum(l),
            ci = sum(ci))

# saveRDS(df_agrupado, "data/df_agrupado.rds")

# Grafico
df_log %>% 
  ggplot(aes(x=x, y=y, color=division)) +
  geom_point() +
  labs(y="Log del VBP", x='Log de los Inputs') + 
  ggtitle('Scatterplot de Output e Inputs según división, en logaritmos') +
  theme_bw()
# Remueve archivos auxiliares
rm(df_log)

