library(tidyverse)
library(here)
library(readxl)
library(ggthemes)

# Cargo df
df_eaae <- readRDS("data/df_eaae.rds")
df_deflactado <- read_excel(here::here("data/df_deflactado.xlsx")) 

# Tabla divisiones
divisiones <- df_eaae %>% 
  select(division, descripcion) %>% 
  distinct() %>% 
  transmute(`División` = division,
            `Descripción` = str_sub(descripcion, 1, 83))

# Tabla divisiones quitar
divisiones_quitar <- divisiones %>% 
  filter(`División` %in% c('10', '19', '20', '17', '26', '11 y 12')) 

# Tabla correlaciones
matriz_correlaciones <- df_eaae %>% 
  select("vbp", "ci", "ckf", "rem") %>% 
  cor() %>% 
  as_data_frame() 

row.names(matriz_correlaciones) <- c("vbp", "ci", "ckf", "rem")

# write_xlsx(matriz_correlaciones, "data/tablas/matriz_correlaciones.xlsx")

# Scatterplot variables en niveles
# grafico_scatter_niveles <- df_deflactado %>% 
#   ggplot(aes(x=x, y=y, color=division)) +
#   geom_point() +
#   labs(y="VBP", x='Suma de los Inputs') + 
#   ggtitle('Scatterplot de Output e Inputs según división, en niveles') +
#   theme_bw()


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
# grafico_scatter_logs <- df_log %>% 
#   ggplot(aes(x=x, y=y, color=division)) +
#   geom_point() +
#   labs(y="Log del VBP", x='Log de los Inputs') + 
#   ggtitle('Scatterplot de Output e Inputs según división') +
#   theme_bw()

df_log %>% 
  select(y, k , l, ci , x) %>% 
  summary()

modelo <- lm(y ~ k + l + ci, data=df_log)

# Tabla TFPC
mpi_tfpc <- df_filtrado %>% 
  select(division) %>% 
  distinct() %>% 
  rbind('Agregado') %>% bind_cols(bind_rows(tfpc_div, tfpc_agr))

