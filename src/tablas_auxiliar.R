library(tidyverse)
# library(here)
# library(readxl)
# library(ggthemes)

# Cargo df
df_eaae <- readRDS("data/df_eaae.rds")
df_deflactado <- read_excel(here::here("data/df_deflactado.xlsx")) 

# Tabla divisiones
divisiones <- df_eaae %>% 
  select(division, descripcion) %>% 
  distinct() %>% 
  transmute(`Division` = division,
            `Descripcion` = str_sub(descripcion, 1, 83))

# Tabla divisiones quitar
divisiones_quitar <- divisiones %>% 
filter(`Division` %in% c('10', '19', '17', '20', '11 y 12', '26', '32', '28')) 

# Tabla correlaciones
matriz_correlaciones <- df_eaae %>% 
  select("vbp", "ci", "ckf", "rem") %>% 
  cor() %>% 
  as_data_frame() 

row.names(matriz_correlaciones) <- c("vbp", "ci", "ckf", "rem")

# write_xlsx(matriz_correlaciones, "data/tablas/matriz_correlaciones.xlsx")

# Scatterplot variables en niveles
grafico_scatter_niveles <- df_deflactado %>% 
  ggplot(aes(x=x, y=y, color=division)) +
  geom_point() +
  labs(y="VBP", x='Suma de los Inputs') + 
  ggtitle('Scatterplot de Output e Inputs según división, en niveles') +
  theme_bw()


# Df con inputs en logs
df_log <- df_deflactado %>% 
  rowwise() %>% 
  mutate(y  = log(y),
         x  = log(sum(ci, k, l)),
         k  = log(k),
         l  = log(l),
         ci = log(ci)) %>% 
  filter(!division %in% c('10', '19', '17', '20', '11 y 12', '26', '32', '28')) 

# Grafico
grafico_scatter_logs <- df_log %>% 
  ggplot(aes(x=x, y=y, color=division)) +
  geom_point() +
  labs(y="Log del VBP", x='Log de los Inputs') + 
  ggtitle('Scatterplot de Output e Inputs según división') +
  theme_bw()
