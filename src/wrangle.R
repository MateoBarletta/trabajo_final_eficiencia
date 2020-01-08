#### DEFLACTACION ####

# CARGO LIBRERIAS
library(dplyr)
library(here)
library(readxl)

# CARGO DF
df_eaae <- read_excel(here::here("data/eaae.xlsx")) %>% 
  mutate(anio = as.character(anio))

# Correlaciones
df_eaae %>% 
  select("vbp", "ci", "imp", "ckf", "vab","rem", "een") %>% 
  cor()

# # Deflactacion
# Las variables se encuentran en valores corrientes por lo que es necesario deflactarlas.
# Para eso usaremos distintos índices de precios:
# 
# Para el Producto (VBP) y Consumo Intermedio (CI) usaremos el índice de precios implícito del producto del BCU.
# Para el Capital (CKF) el índice de precios implícitos de la formación bruta de capital fijo del BCU.
# Para el Trabajo (REM) el índice medio de salarios del INE.
# 
# En principio se utilizan todos los índices globales, 
# para mejorar la estimación se puede considerar utilizar índices abiertos por industria.

# CARGO DEFLACTORES
deflactores <- read_excel("data/deflactores.xlsx") %>% 
  mutate(anio = as.character(anio))

# DEFLACTACION
df_deflactado <- df_eaae %>% 
  left_join(deflactores, by = 'anio') %>% 
  transmute(anio,
            division,
            seccion,
            descripcion,
            y  = 100 * vbp / ipi_vbp, 
            ci = 100 * ci / ipi_vbp,
            k  = 100 * ckf / ipi_fbkf,
            rem_d = 100 * rem / ims)

# SALVA EXCEL
# write_xlsx(df_deflactado, "data/df_deflactado.xlsx")1
