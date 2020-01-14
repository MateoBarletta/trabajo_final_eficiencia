library(tidyverse)
library(here)
library(readxl)
library(ggthemes)
library(skimr)
library(car)

# Cargo df
df_eaae       <- readRDS("data/df_eaae.rds")
df_deflactado <- readRDS("data/df_deflactado.rds")
df_agrupado   <- readRDS("data/df_agrupado.rds")
df_filtrado   <- readRDS("data/df_filtrado.rds")

# Divisiones
divisiones <- df_eaae %>% 
  select(division, descripcion) %>% 
  distinct() %>% 
  transmute(`Division` = division,
            `Descripcion` = str_sub(descripcion, 1, 83))

# Tabla divisiones quitar
divisiones_quitar <- divisiones %>% 
  filter(`Division` %in% c('10', '19', '20', '17', '26', '11 y 12')) 

# Tabla divisiones anexo
tabla_divisiones <- df_eaae %>% 
  select(division, descripcion) %>% 
  distinct() %>% 
  transmute(`Division` = division,
            `Descripcion` = str_sub(descripcion, 1, 83))

# Tabla correlaciones
matriz_correlaciones <- df_eaae %>% 
  select("vbp", "ci", "ckf", "rem") %>% 
  cor() %>% 
  as_tibble() 

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

# # Grafico
# df_log %>%
#   ggplot(aes(x=x, y=y, color=division)) +
#   geom_point() +
#   labs(y="Log del VBP", x='Log de los Inputs') +
#   ggtitle('Scatterplot de Output e Inputs según división') +
#   theme_bw()

# Estadísticos descriptivos
tabla_estadisticos <- df_filtrado %>% 
  select(y, k , l, ci) %>% 
  skim_without_charts() %>% 
  transmute(variable = skim_variable,
            n = '40',
            mean = numeric.mean,
            sd = numeric.sd,
            p0 = numeric.p0,
            p25 = numeric.p25,
            p50 = numeric.p50,
            p75 = numeric.p75,
            p100 = numeric.p100)

# Corre script estimaciones
source('src/estimacion.R', encoding = 'UTF-8')

# Test de significacion conjunta
modelo <- lm(log(y) ~ log(k) + log(l) + log(ci), data = df_filtrado)
linearHypothesis(modelo, c("1*log(k) + 1*log(l) + 1*log(ci) = 1"))

# Tabla TFPC
mpi_tfpc <- df_filtrado %>% 
  select(division) %>% 
  distinct() %>% 
  rbind('Agregado') %>% bind_cols(bind_rows(tfpc_div, tfpc_agr))

