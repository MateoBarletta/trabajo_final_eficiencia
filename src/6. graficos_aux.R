library(Benchmarking)

# Cargo df
df <- readRDS("data/df_filtrado.rds") 

# Creo vectores para estimacion
# Output
y  <- cbind(df$y)

# Inputs
x  <- cbind(df$k, df$l, df$ci)
xx <- cbind(df$x)

# Año 2012 (base) 
y0  <- matrix(c(y[1:16,1]), ncol=1)
x0  <- matrix(c(x[1:16,1:3]), ncol=3)
x00 <- matrix(c(xx[1:16,1]), ncol=1)

# Año 2016
y4  <- matrix(c(y[65:80,1]),ncol=1)
x4  <- matrix(c(x[65:80,1:3]),ncol=3)
x44 <- matrix(c(x[65:80,1]),ncol=1)

# Plot
dea.plot(x0[,3], y0[,1], RTS="crs", lty="dashed", col='royalblue', main="Gráfico 3: Frontera de eficiencia DEA")
points(x0[,3], y0[,1], pch=16, col="royalblue")
points(x4[,3], y4[,1], pch=16, col="red")
dea.plot(x4[,3], y4[,1], RTS="crs", add=TRUE, lty = "dashed", col='red')