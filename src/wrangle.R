library(readxl)
library(dplyr)

# CARGO DF
df_eaae <- read_excel("data/eaae.xlsx")

# Correlaciones
df_eaae %>% 
  select("vbp", "ci", "imp", "ckf", "vab","rem", "een") %>% 
  cor()
