#### DEFLACTACION ####

# CARGO LIBRERIAS
library(dplyr)
library(here)
library(readxl)
library(writexl)

# CARGO DF
df_eaae <- read_excel(here::here("data/eaae.xlsx")) %>% 
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

# Tabla divisiones
divisiones <- df_eaae %>% 
  select(division, descripcion) %>% 
  distinct()

# write_xlsx(divisiones, "data/tablas/divisiones.xlsx")


# Tabla correlaciones
matriz_correlaciones <- df_eaae %>% 
  select("vbp", "ci", "ckf", "rem") %>% 
  cor() %>% 
  as_data_frame() 

row.names(matriz_correlaciones) <- c("vbp", "ci", "ckf", "rem")

# write_xlsx(matriz_correlaciones, "data/tablas/matriz_correlaciones.xlsx")




## Deflactacion
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
# write_xlsx(df_deflactado, "data/df_deflactado.xlsx")
