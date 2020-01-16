# Output
y  <- cbind(df$y)

# Inputs
x  <- cbind(df$k, df$l, df$ci)

# Año 2012 (base) 
y0  <- matrix(c(y[1:16,1]), ncol=1)
x0  <- matrix(c(x[1:16,1:3]), ncol=3) 

# Año 2016
y4  <- matrix(c(y[65:80,1]),ncol=1)
x4  <- matrix(c(x[65:80,1:3]),ncol=3)

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

matriz_tc <- cbind(tfpc_punta, tc, tec, pech, sech)
colnames(m) <- c("TFPC", "TC", "TEC","PECH", "SEC")
colMeans(m)
