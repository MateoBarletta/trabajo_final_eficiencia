#### IMPORTACION DE ARCHIVOS DE EAAE ####

# CARGA LIBRERIAS
library(dplyr)
library(here)
library(readxl)
library(janitor)


# PATH IMPORTACION ARCHIVOS
path <- here::here("data", "eaae")

# NOMBRES COLUMNAS
col1 <- c("seccion",	"division", "descripcion", "VBP", "CI", "VAB", "REM", "IMP", "CKF", "EEN")
col2 <- c("seccion",	"division", "descripcion", "VBP", "CI", "IMP", "CKF", "VAB", "REM", "EEN")

# IMPORTA DFs por año
df_2016 <- read_excel(file.path(path, "EAE_C2.5_2016.xls"),
                      skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2016')

df_2015 <- read_excel(file.path(path, "EAE_C2.6_2015.xls"),
                                 skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2015')

df_2014 <- read_excel(file.path(path, "EAE_C2.6_2014.xls"),
                      skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2014')

df_2013 <- read_excel(file.path(path, "EAE_C2.6_2013.xls"),
                      skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2013')

df_2012 <- read_excel(file.path(path, "EAE_C2.6_2012.xls"),
                      skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2012')

df_2011 <- read_excel(file.path(path, "EAE_C2_2011.xls"),
                      skip = 11, col_names = col1) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2011')

df_2010 <- read_excel(file.path(path, "EAE_C2_2010.xlsx"),
                      skip = 11, col_names = col2) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2010')

df_2009 <- read_excel(file.path(path, "EAE_C2_2009.xlsx"),
                      skip = 11, col_names = col2) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2009')

df_2008 <- read_excel(file.path(path, "EAE_C2_2008.xls"),
                      skip = 11, col_names = col2) %>% 
  filter(!is.na(VBP)) %>% 
  mutate(anio = '2008')

# CREA DF UNICO
  df_eaae <- bind_rows(df_2012, df_2013, df_2014, df_2015, df_2016) %>% 
  clean_names() %>% 
  filter(seccion == "C") %>% 
  select(anio, seccion, division, descripcion, vbp, vab, ci, rem, ckf, everything())


# SALVA EXCEL
# saveRDS(df_eaae, "data/eaae.rds")

# LIMPIEZA DE ARCHIVOS INTERMEDIOS
rm(df_2008, df_2009, df_2010, df_2011, df_2012, df_2013, df_2014, df_2015, df_2016)
rm(path, col1, col2)
  