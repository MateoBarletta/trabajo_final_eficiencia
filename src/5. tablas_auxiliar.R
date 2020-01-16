library(dplyr)
library(tidyr)
library(ggplot2)
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
source('src/4. estimacion.R', encoding = 'UTF-8')

# Test de significacion conjunta
modelo <- lm(log(y) ~ log(k) + log(l) + log(ci), data = df_filtrado)
linearHypothesis(modelo, c("1*log(k) + 1*log(l) + 1*log(ci) = 1"))

# Tabla MPI
mpi_tfpc <- df_filtrado %>% 
  select(division) %>% 
  distinct() %>% 
  rbind('Promedio') %>% 
  bind_cols(tfpc_div) %>% 
  transmute (Division = division,
             `2013`   = tfpc_2013,
             `2014`   = tfpc_2014,
             `2015`   = tfpc_2015,
             `2016`   = tfpc_2016,
             Punta    = tfpc_punta)

# Tabla TFPC
tabla_tc <- df_filtrado %>% 
  select(division) %>% 
  distinct() %>% 
  rbind('Promedio') %>% 
  bind_cols(matriz_tc) %>% 
  transmute(Division = division,
            TFPC = `V1`,
            TC = `V2`,
            TEC = `V3`,
            PTEC = `V4`,
            SEC = `V5`)