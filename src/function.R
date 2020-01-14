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


