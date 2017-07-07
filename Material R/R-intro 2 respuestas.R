

# -----------------------------------------------------------------------------
# -- Soluciones a ejercicios clase opcional 2 --
# Por supuesto que hay más de una forma de resolver estos ejercicios
# acá van algunas alternativas
# ------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------
# -- Ejercicio 3 
# --- a. En e2 (de los ejercios de ayer), cambiar el nombre del vec1 a 'rate' y el vec2 a 'state'
# --- b. Crear una nueva varible = "raíz" cuadrada de rate dividida por "idnum" 
# --- nota: sólo las variables numéricas se puede usar en funciones matemáticas
# --------------------------------------------------------------------------------------------------

vec1 <- c(4.6, 4.8, 5.2, 6.3, 6.8, 7.1, 7.2, 7.4, 7.5, 8.6)    
vec2 <- c('UT', 'IA', 'MN', 'AL', 'WI', 'MN', 'OH', 'IA', 'NY', 'IA')

idnum <- seq(from = 1, to = 10)

df1<-cbind(vec1, vec2)
e2 <- data.frame(idnum, df1)

names(e2)[names(e2) %in% c("vec1","vec2")]<-c('rate', 'state')
#names(e2)[2:3] <- c('rate', 'state') 

class(e2$rate)<-"numeric"

e2$e3 <- sqrt(e2$rate) / e2$idnum

 

# ----------------------------------------------------------------------------------------------------------
# -- Ejercicio 4
# --- Usando e2, crear un subconjunto llamado e4 que sólamente contiene las observaciones
# --- con "rate" entre 5 y 7 
# ----------------------------------------------------------------------------------------------------------


e4 <- subset(e2, rate > 5 & rate < 7)

# ----------------------------------------------------------------------------------------
# -- Ejercicio 5 
# --- Usando e2, crear un set apropiado de estadísticas descripticas de la variable "rate" 
# ----------------------------------------------------------------------------------------

summary(e2$rate)
mean(e2$rate)
max(e2$rate)
quantile(e2$rate)


# ----------------------------------------------------------------------------------------
# -- Ejercicio 6 
# --- Usando e2, crear un ifelse statement de al menos cuatro categorías 
# ----------------------------------------------------------------------------------------


e2$ifvar<-ifelse(e2$state=="IA", 1 , 
                 ifelse(e2$rate<5 & !e2$state=="IA",2 , 
                        ifelse(e2$rat>7 & !e2$state=="IA", 3, 
                               ifelse(e2$idnum==6, 4, 5
                                                  ))))









