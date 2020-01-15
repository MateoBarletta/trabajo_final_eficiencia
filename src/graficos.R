library(tidyr)
library(dplyr)
library(Benchmarking)

# Cargo df
df <- readRDS(here::here("data/df_filtrado.rds")) 

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
dea.plot(x0[,3], y0[,1], RTS="crs", lty="dashed", col='royalblue', main="Frontera de eficiencia DEA")
points(x0[,3], y0[,1], pch=16, col="royalblue")
points(x4[,3], y4[,1], pch=16, col="red")
dea.plot(x4[,3], y4[,1], RTS="crs", add=TRUE, lty="dashed", col='red')






# mpi_tfpc %>% 
#   select(-tfpc_punta) %>% 
#   filter(division %in% c('14', '33', '31', '28')) %>% 
#   pivot_longer(values_to = 'MPI', names_to = 'Año', names_prefix="tfpc_", -division) %>% 
#   ggplot(aes(x = Año, y = MPI, color = division)) +
#   geom_point() +
#   geom_line(aes(group = division)) +
#   ggtitle("Evolución de la divisiones con mejor y peor desempeño") +
#   labs(color = 'División') +
#   theme_bw()


# DEA PLOT
# dea.plot(x_0, y0, RTS="crs", txt=TRUE, xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_1, y1, RTS="crs", add=TRUE, col="red", xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_2, y2, RTS="crs", add=TRUE, col="green", xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_3, y3, RTS="crs", add=TRUE, col="yellow", xlim = c(20, 25), ylim = c(20,25))
# dea.plot(x_4, y4, RTS="crs", add=TRUE, col="blue", xlim = c(20, 25), ylim = c(20,25))
# points(x_1, y1, col="red", pch=16)
# points(x_2, y2, col="green", pch=16)
# points(x_3, y3, col="yellow", pch=16)
# points(x_4, y4, col="blue", pch=16)
# text(x_1, y1, 1:dim(x1)[1], col="red", adj=-1)
# text(x_2, y2, 1:dim(x1)[1], col="green", adj=-1)
# text(x_3, y3, 1:dim(x1)[1], col="yellow", adj=-1)
# text(x_4, y4, 1:dim(x1)[1], col="blue", adj=-1)

# # Plot 
# dea.plot(x_0[,3], y_0[,1], RTS="crs", lty="dashed", col='royalblue')
# points(x_0[,3], y_0[,1], pch=16, col="blue")
# points(x_4[,3], y_4[,1], pch=16, col="red")
# dea.plot(x_4[,3], y_4[,1], RTS="crs", add=TRUE, lty="dashed", col='red')


