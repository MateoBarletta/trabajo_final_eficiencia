library(tidyverse)
library(Benchmarking)

# Cargo df
df <- df_log
df <- df_deflactado %>% 
  filter(!division %in% c('10', '19', '20', '17', '26', '11 y 12')) 



#### ESTIMACION ####
# Output
y <- cbind(df$y)

# Inputs
x <- cbind(df$k, df$l, df$ci)

# Año 2012 (base) 
y0 <- matrix(c(y[1:16,1]),ncol=1)
x0 <- matrix(c(x[1:16,1:3]),ncol=3)

# Año 2013
y1 <- matrix(c(y[17:32,1]),ncol=1)
x1 <- matrix(c(x[17:32,1:3]),ncol=3)

# Año 2014
y2 <- matrix(c(y[33:48,1]),ncol=1)
x2 <- matrix(c(x[33:48,1:3]),ncol=3)

# Año 2015
y3 <- matrix(c(y[49:64,1]),ncol=1)
x3 <- matrix(c(x[49:64,1:3]),ncol=3)

# Año 2016
y4 <- matrix(c(y[65:80,1]),ncol=1)
x4 <- matrix(c(x[65:80,1:3]),ncol=3)



# DEA PLOT
dea.plot(x0, y0, RTS="crs", txt=TRUE)
dea.plot(x1, y1, RTS="crs", add=TRUE, col="red")
points(x1, y1, col="red", pch=16)
points(x0, y0,  pch=16)
text(x1, y1, 1:dim(x1)[1], col="red", adj=-1)# Import data
dea.plot(x0, y0, RTS="vrs", add=TRUE, lty="dashed")
dea.plot(x1, y1, RTS="vrs", add=TRUE, col="red", lty="dashed")

# SFA with all periods
msfa <- sfa(x,y)
msfa

#technical change 
tc <- exp(1.3285)

# estimate contemporary function
msfa0 <- sfa(x0,y0)
msfa1 <- sfa(x1,y1)
msfa2 <- sfa(x2,y2)
msfa3 <- sfa(x3,y3)
msfa4 <- sfa(x4,y4)

tec <- teBC.sfa(msfa1)/teBC.sfa(msfa0)
tfpc <- tc*tec

