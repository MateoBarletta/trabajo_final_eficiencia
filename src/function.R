library(Benchmarking)

# Funcion MPI DEA (Output oriented)
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

# #### ESTIMACION AGREGADA ####
# # Agrego df para toda la industria
# df_agrupado <- df %>% 
#   group_by(anio) %>% 
#   summarise(y  = sum(y),
#             k  = sum(k),
#             l  = sum(l),
#             ci = sum(ci))
# 
# # Output
# yy <- cbind(df_agrupado$y)
# 
# # Inputs
# xx <- cbind(df_agrupado$k, df_agrupado$l, df_agrupado$ci)
# 
# # Año 2012 (base) 
# y_0 <- matrix(c(yy[1,1]), ncol=1)
# x_0 <- matrix(c(xx[1,1:3]), ncol=3) 
# 
# # Año 2013
# y_1 <- matrix(c(yy[2,1]), ncol=1)
# x_1 <- matrix(c(xx[2,1:3]), ncol=3) 
# 
# # Año 2014
# y_2 <- matrix(c(yy[3,1]), ncol=1)
# x_2 <- matrix(c(xx[3,1:3]), ncol=3) 
# 
# # Año 2015
# y_3 <- matrix(c(yy[4,1]), ncol=1)
# x_3 <- matrix(c(xx[4,1:3]), ncol=3) 
# 
# # Año 2016
# y_4 <- matrix(c(yy[5,1]), ncol=1)
# x_4 <- matrix(c(xx[5,1:3]), ncol=3) 
# 
# # Calculo MPI para todos los intervalos
# tfpc_2013 <- calcula_mpi(x_t0 = x_0, x_t1 = x_1, y_t0 = y_0, y_t1 = y_1)
# tfpc_2014 <- calcula_mpi(x_t0 = x_1, x_t1 = x_2, y_t0 = y_1, y_t1 = y_2)
# tfpc_2015 <- calcula_mpi(x_t0 = x_2, x_t1 = x_3, y_t0 = y_2, y_t1 = y_3)
# tfpc_2016 <- calcula_mpi(x_t0 = x_3, x_t1 = x_4, y_t0 = y_3, y_t1 = y_4)
# 
# # Calculo MPI punta a punta (2012 a 2016)
# tfpc_punta <- calcula_mpi(x_t0 = x_0, x_t1 = x_4, y_t0 = y_0, y_t1 = y_4) 
# 
# # Armo tabla tfpc divisiones
# tfpc_agr <- tibble(tfpc_2013, tfpc_2014, tfpc_2015, tfpc_2016, tfpc_punta)
# 
# # Limpieza de archivos auxiliares
# rm(tfpc_2013, tfpc_2014, tfpc_2015, tfpc_2016, tfpc_punta)
# rm(xx, yy, x_0, x_1, x_2, x_3, x_4, y_0, y_1, y_2, y_3, y_4)




# #Estimacion 2012/2013
# # Estimate 4 distance functions
# d00 <- dea(x0, y0, RTS="crs", ORIENTATION="out")
# d01 <- dea(x1, y1, RTS="crs", XREF=x0, YREF=y0, ORIENTATION="out") 
# d10 <- dea(x0, y0, RTS="crs", XREF=x1, YREF=y1, ORIENTATION="out") 
# d11 <- dea(x1, y1, RTS="crs", ORIENTATION="out")
# 
# # teff <- cbind(d00$eff, d01$eff, d10$eff, d11$eff) 
# # colnames(teff) <- c("d00", "d10", "d01","d11")
# 
# # shepard's distance functions
# eff00 <- cbind(1/d00$eff)
# eff10 <- cbind(1/d10$eff)
# eff01 <- cbind(1/d01$eff)
# eff11 <- cbind(1/d11$eff)
# 
# eff <- cbind(eff00,eff10,eff01,eff11)
# colnames(eff) <- c("eff00", "eff10", "eff01","eff11")
# 
# # Malquist productivity index (tfpc)
# # El indice de malmquist se aplica con funciones de distancia de shepard (invertir las de farrell)
# tfpc <- cbind((eff01/eff00 * eff11/eff10) ^ 0.5)


# # Estimo SFA contemporaneos
# msfa0 <- sfa(x0,y0)
# msfa1 <- sfa(x1,y1)
# msfa2 <- sfa(x2,y2)
# msfa3 <- sfa(x3,y3)
# msfa4 <- sfa(x4,y4)
# 
# # Estimo SFA para x0 y x1
# x01 <- cbind(x0, x1)
# y01 <- cbind(y0, y1)
# 
# msfa01 <- sfa(x0,y1)
# msfa01
# 
# # Productividad y descomposicion
# tc01 <- exp(msfa01$coef[1])
# tec01 <- teBC.sfa(msfa1)/teBC.sfa(msfa0)
# 
# tfpc01 <- tc01*tec01
# # Output
# y  <- cbind(df$y)
# 
# # Inputs
# x  <- cbind(df$k, df$l, df$ci)
# 
# # Año 2012 (base) 
# y0  <- matrix(c(y[1:16,1]), ncol=1)
# x0  <- matrix(c(x[1:16,1:3]), ncol=3) 
# 
# # Año 2016
# y4  <- matrix(c(y[65:80,1]),ncol=1)
# x4  <- matrix(c(x[65:80,1:3]),ncol=3)
# 
# # Estimo las funciones de distancia
# d00 <- dea(x0, y0, RTS = "crs", ORIENTATION = "out")
# d40 <- dea(x0, y0, RTS = "crs", XREF = x4, YREF = y4, ORIENTATION = "out") 
# d04 <- dea(x4, y4, RTS = "crs", XREF = x0, YREF = y0, ORIENTATION="out") 
# d44 <- dea(x4, y4, RTS = "crs", ORIENTATION = "out")
# 
# # Funciones de Shepard (inverso)
# eff00 <- cbind(1/d00$eff)
# eff40 <- cbind(1/d40$eff)
# eff04 <- cbind(1/d04$eff)
# eff44 <- cbind(1/d44$eff)
# 
# # Cambio en la eficiencia
# # TEC  = PECH x SEC
# # TFPC = TEC x TC
# tec <- cbind(eff44/eff00)
# 
# # Cambio tecnico
# tc <- cbind((eff04/eff44 * eff00/eff40)^0.5)
# 
# # Pure and scale efficiency decomposition
# d00v   <-  dea(x0 ,y0, RTS="vrs", ORIENTATION="out");
# d44v   <-  dea(x4, y4, RTS="vrs", ORIENTATION="out") 
# eff00v <- cbind(1/d00v$eff)
# eff44v <- cbind(1/d44v$eff)
# pech <- cbind(eff44v/eff00v)
# sech <- cbind((eff00v/eff00)/(eff44v/eff44))
# 
# matriz_tc <- cbind(tfpc_punta, tc, tec, pech, sech)
# colnames(m) <- c("TFPC", "TC", "TEC","PECH", "SEC")
# colMeans(m)



