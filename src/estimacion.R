library(tidyverse)
library(Benchmarking)

# Cargo df
df <- df_log
df <- df2

#### ESTIMACION ####
# Output
y <- cbind(df$y)

# Inputs
x <- cbind(df$k, df$l, df$ci)
xx <- cbind(df$x)


# Año 2012 (base) 
y0  <- matrix(c(y[1:16,1]),ncol=1)
x0  <- matrix(c(x[1:16,1:3]),ncol=3)
x_0 <- matrix(c(xx[1:14,1]),ncol=1) 

# Año 2013
y1  <- matrix(c(y[17:32,1]),ncol=1)
x1  <- matrix(c(x[17:32,1:3]),ncol=3)
x_1 <- matrix(c(xx[15:28,1]),ncol=1) 

# Año 2014
y2  <- matrix(c(y[33:48,1]),ncol=1)
x2  <- matrix(c(x[33:48,1:3]),ncol=3)
x_2 <- matrix(c(xx[29:42,1]),ncol=1)

# Año 2015
y3  <- matrix(c(y[49:64,1]),ncol=1)
x3  <- matrix(c(x[49:64,1:3]),ncol=3)
x_3 <- matrix(c(xx[43:56,1]),ncol=1)

# Año 2016
y4  <- matrix(c(y[65:80,1]),ncol=1)
x4  <- matrix(c(x[65:80,1:3]),ncol=3)
x_4 <- matrix(c(xx[57:70,1]),ncol=1)


# Estimate 4 distance functions
d00 <- dea(x0, y0, RTS="crs", ORIENTATION="out")
d01 <- dea(x1, y1, RTS="crs", XREF=x0, YREF=y0, ORIENTATION="out") 
d10 <- dea(x0, y0, RTS="crs", XREF=x1, YREF=y1, ORIENTATION="out") 
d11 <- dea(x1, y1, RTS="crs", ORIENTATION="out")

teff <- cbind(d00$eff, d01$eff, d10$eff, d11$eff) 
colnames(teff) <- c("d00", "d10", "d01","d11")

# shepard's distance functions
eff00 <- cbind(1/d00$eff)
eff10 <- cbind(1/d10$eff)
eff01 <- cbind(1/d01$eff)
eff11 <- cbind(1/d11$eff)

eff <- cbind(eff00,eff10,eff01,eff11)
colnames(eff) <- c("eff00", "eff10", "eff01","eff11")

# Malquist productivity index (tfpc)
# El indice de malmquist se aplica con funciones de distancia de shepard (invertir las de farrell)
tfpc <- cbind((eff01/eff00 * eff11/eff10) ^ 0.5)

 


# # Estimo SFA contemporaneos
# msfa0 <- sfa(x0,y0)
# msfa1 <- sfa(x1,y1)
# msfa2 <- sfa(x2,y2)
# msfa3 <- sfa(x3,y3)
# msfa4 <- sfa(x4,y4)


# Estimo SFA para x0 y x1
x01 <- cbind(x0, x1)
y01 <- cbind(y0, y1)

msfa01 <- sfa(x0,y1)
msfa01

#Productividad y descomposicion
tc01 <- exp(msfa01$coef[1])
tec01 <- teBC.sfa(msfa1)/teBC.sfa(msfa0)

tfpc01 <- tc01*tec01






# DEA PLOT
dea.plot(x_0, y0, RTS="crs", txt=TRUE, xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_1, y1, RTS="crs", add=TRUE, col="red", xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_2, y2, RTS="crs", add=TRUE, col="green", xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_3, y3, RTS="crs", add=TRUE, col="yellow", xlim = c(20, 25), ylim = c(20,25))
dea.plot(x_4, y4, RTS="crs", add=TRUE, col="blue", xlim = c(20, 25), ylim = c(20,25))
# points(x_1, y1, col="red", pch=16)
# points(x_2, y2, col="green", pch=16)
# points(x_3, y3, col="yellow", pch=16)
points(x_4, y4, col="blue", pch=16)
# text(x_1, y1, 1:dim(x1)[1], col="red", adj=-1)
# text(x_2, y2, 1:dim(x1)[1], col="green", adj=-1)
# text(x_3, y3, 1:dim(x1)[1], col="yellow", adj=-1)
text(x_4, y4, 1:dim(x1)[1], col="blue", adj=-1)










