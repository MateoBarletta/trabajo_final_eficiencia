# CARGO LIBRERIAS
library(dplyr)
library(here)
library(readxl)

# CARGO DF
eaae <- readRDS("data/eaae.rds") %>% 
  mutate(anio = as.character(anio))

# Agrupo divisiones 29 y 30
division_quitar <- eaae %>% 
  filter(division %in% c('29', '30'))

# Creo df auxiliar para agregar
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

# Quito divisiones separadas y agrego divisiones agregadas
df_eaae <- eaae %>% 
  anti_join(division_quitar, by= c('anio','division')) %>% 
  bind_rows(division_agregar) %>% 
  arrange(anio, division)

# Salva RDS
# saveRDS(df_eaae, "data/df_eaae.rds")


#### DEFLACTACION ####
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

# Elimina archivos auxiliares
rm(deflactores, eaae, division_agregar, division_quitar)
