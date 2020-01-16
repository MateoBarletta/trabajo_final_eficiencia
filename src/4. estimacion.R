library(dplyr)
library(Benchmarking)

# Cargo df
df_filtrado <- readRDS(here::here("data/df_filtrado.rds")) 

df <- df_filtrado

# Defino funcion MPI DEA (Output oriented)
calcula_mpi <- function(x_t0, x_t1, y_t0, y_t1){
  
  d00 <- dea(x_t0, y_t0, RTS = "crs", ORIENTATION = "out")
  d01 <- dea(x_t1, y_t1, RTS = "crs", XREF = x_t0, YREF = y_t0, ORIENTATION="out") 
  d10 <- dea(x_t0, y_t0, RTS = "crs", XREF = x_t1, YREF = y_t1, ORIENTATION="out") 
  d11 <- dea(x_t1, y_t1, RTS = "crs", ORIENTATION = "out")
  
  eff00 <- cbind(1/d00$eff)
  eff10 <- cbind(1/d10$eff)
  eff01 <- cbind(1/d01$eff)
  eff11 <- cbind(1/d11$eff)
  
  tfpc <- cbind((eff01/eff00 * eff11/eff10) ^ 0.5)
  
  return(tfpc)
  
}

#### ESTIMACION POR DIVISION ####
## Creo vectores para estimacion
# Output
y  <- cbind(df$y)

# Inputs
x  <- cbind(df$k, df$l, df$ci)

# Año 2012 (base) 
y0  <- matrix(c(y[1:16,1]), ncol=1)
x0  <- matrix(c(x[1:16,1:3]), ncol=3)

# Año 2013
y1  <- matrix(c(y[17:32,1]),ncol=1)
x1  <- matrix(c(x[17:32,1:3]),ncol=3)

# Año 2014
y2  <- matrix(c(y[33:48,1]),ncol=1)
x2  <- matrix(c(x[33:48,1:3]),ncol=3)

# Año 2015
y3  <- matrix(c(y[49:64,1]),ncol=1)
x3  <- matrix(c(x[49:64,1:3]),ncol=3)

# Año 2016
y4  <- matrix(c(y[65:80,1]),ncol=1)
x4  <- matrix(c(x[65:80,1:3]),ncol=3)

# Calculo MPI para todos los intervalos
tfpc_2013 <- calcula_mpi(x_t0 = x0, x_t1 = x1, y_t0 = y0, y_t1 = y1)
tfpc_2014 <- calcula_mpi(x_t0 = x1, x_t1 = x2, y_t0 = y1, y_t1 = y2)
tfpc_2015 <- calcula_mpi(x_t0 = x2, x_t1 = x3, y_t0 = y2, y_t1 = y3)
tfpc_2016 <- calcula_mpi(x_t0 = x3, x_t1 = x4, y_t0 = y3, y_t1 = y4)

# Calculo MPI punta a punta (2012 a 2016)
tfpc_punta <- calcula_mpi(x_t0 = x0, x_t1 = x4, y_t0 = y0, y_t1 = y4) 

# Armo tabla tfpc divisiones para todos los años
tfpc_div <- tibble(tfpc_2013, tfpc_2014, tfpc_2015, tfpc_2016, tfpc_punta) %>% 
  rbind(colMeans(tibble(tfpc_2013, tfpc_2014, tfpc_2015, tfpc_2016, tfpc_punta)))
  

##### DESCOMPOSICIÓN DEL MPI #####
# Estimo las funciones de distancia
d00 <- dea(x0, y0, RTS = "crs", ORIENTATION = "out")
d40 <- dea(x0, y0, RTS = "crs", XREF = x4, YREF = y4, ORIENTATION = "out") 
d04 <- dea(x4, y4, RTS = "crs", XREF = x0, YREF = y0, ORIENTATION="out") 
d44 <- dea(x4, y4, RTS = "crs", ORIENTATION = "out")

# Funciones de Shepard (inverso)
eff00 <- cbind(1/d00$eff)
eff40 <- cbind(1/d40$eff)
eff04 <- cbind(1/d04$eff)
eff44 <- cbind(1/d44$eff)

# Cambio en la eficiencia
# TEC  = PECH x SEC
# TFPC = TEC x TC
tec <- cbind(eff44/eff00)

# Cambio tecnico
tc <- cbind((eff04/eff44 * eff00/eff40)^0.5)

# Pure and scale efficiency decomposition
d00v   <-  dea(x0 ,y0, RTS="vrs", ORIENTATION="out");
d44v   <-  dea(x4, y4, RTS="vrs", ORIENTATION="out") 
eff00v <- cbind(1/d00v$eff)
eff44v <- cbind(1/d44v$eff)
pech <- cbind(eff44v/eff00v)
sech <- cbind((eff00v/eff00)/(eff44v/eff44))

m <- cbind(tfpc_punta, tc, tec, pech, sech) 
matriz_tc <- m %>% 
  rbind(colMeans(m)) %>% 
  as_tibble()

# Limpieza de archivos auxiliares
rm(tfpc_2013, tfpc_2014, tfpc_2015, tfpc_2016, tfpc_punta, m)
rm(x, y, x0, x1, x2, x3, x4, y0, y1, y2, y3, y4)
rm(d00v, d44v, eff00v, eff44v, pech, sech, tec, tc, d00, d40, d04, d44, eff00, eff40, eff04, eff44)
