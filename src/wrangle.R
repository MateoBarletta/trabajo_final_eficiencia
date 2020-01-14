# CARGO LIBRERIAS
library(dplyr)
library(here)
library(readxl)
library(writexl)

# CARGO DF
eaae <- read_excel(here::here("data/eaae.xlsx")) %>% 
  mutate(anio = as.character(anio))

# Agrupo division 29
division_quitar <- eaae %>% 
  filter(division %in% c('29', '30'))

division_agregar <- division_quitar %>% 
  group_by() %>%
  summarise(anio        = '2012',
            seccion     = 'C',
            division    = '29 y 30',
            descripcion = 'Fabricación de vehículos automotores, remolques y semirremolques. Fabricación de otros tipos de equipo de transporte.',
            vbp         = sum(vbp),
            vab         = sum(vab),
            ci          = sum(ci),
            rem         = sum(rem),
            ckf         = sum(ckf),
            imp         = sum(imp),
            een         = sum(een))

df_eaae <- eaae %>% 
  anti_join(division_quitar, by= c('anio','division')) %>% 
  bind_rows(division_agregar) %>% 
  arrange(anio, division)

rm(eaae, division_agregar, division_quitar)

# saveRDS(df_eaae, "data/df_eaae.rds")



#### DEFLACTACION ####
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
            k  = 100 * ckf / ipi_fbkf,
            l  = 100 * rem / ims, 
            ci = 100 * ci / ipi_vbp,
            x  = k + l + ci)

# Salva RDS
# saveRDS(df_deflactado, "data/df_deflactado.rds") 

