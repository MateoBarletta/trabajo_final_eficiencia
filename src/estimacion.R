library(tidyverse)
library(Benchmarking)

# Cargo df
df <- df_log

#### ESTIMACION ####

# Output base
y0 <- df %>% 
  filter(anio == '2012') %>% 
  select(y) %>% 
  data.frame()

# Inputs base
x0 <- df %>% 
  filter(anio == '2012') %>% 
  select(ci, k, l)   %>% 
  data.frame()

# Output año 1
y1 <- df %>% 
  filter(anio == '2013') %>% 
  select(y) %>% 
  data.frame()

# Inputs año1
x1 <- df %>% 
  filter(anio == '2013') %>% 
  select(ci, k, l)  %>% 
  data.frame()

# DEA PLOT
dea.plot(x0, y0, RTS="crs", txt=TRUE)
dea.plot(x1, y1, RTS="crs", add=TRUE, col="red")
points(x1, y1, col="red", pch=16)
points(x0, y0,  pch=16)
text(x1, y1, 1:dim(x1)[1], col="red", adj=-1)# Import data
dea.plot(x0, y0, RTS="vrs", add=TRUE, lty="dashed")
dea.plot(x1, y1, RTS="vrs", add=TRUE, col="red", lty="dashed")


