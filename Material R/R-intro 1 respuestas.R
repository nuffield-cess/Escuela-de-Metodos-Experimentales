
# -----------------------------------------------------------------------------
# -- Soluciones a ejercicios clase opcional 1  --
# Por supuesto que hay más de una forma de resolver estos ejercicios
# acá van algunas alternativas
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------
# -- Ejrecicio 1 
# --- Crear un vector llamado e1 que sea una secuencia de 1 a 10, repetida 5 veces 
# ------------------------------------------------------------------------------------------	

e1 <- rep(1:10, times = 5)

v <- c(1:10)
e1 <- rep(v, times = 5)



# --------------------------------------------------------------------------------------------------
# -- Ejrecicio 2 
# --- Crear una base de datos llamada e2, en la cual se le agrega la variable (idnum, abajo) a df1
# --------------------------------------------------------------------------------------------------
idnum <- seq(from = 1, to = 10)
e2 <- data.frame(idnum, df1)

# Otra alternativa es
e2<-df1
e2$idnum<-idnum





