




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

# ------------------------------------------------------------------------------
# -- Exercise 6 
# --- Using e2, create a histogram of rate with appropriate customizations
# ------------------------------------------------------------------------------

hist(e2$rate, xlab = "Rate", main = "Histogram of Rate", col = "green")






# ----------------------------

#  Vectors 
v1 <- rnorm(100, 75, 15)
v2 <- as.factor(rep(c("A", "B", "C", "D"), times = 25))
v3 <- rnorm(100, 1, .5) 

# Data Frames 
mydata <- data.frame(v1, v2, v3)


# -----------------------------------------------------------------------------
# -- Exercise 1 
# --- Create a subset of mydata, which contains the 25 highest v1 scores
# -----------------------------------------------------------------------------  
sorted <- mydata[order(-mydata$v1), ][1:25, ]

fullsort<-mydata %>% arrange(desc(v1))
fullsort[1:25, ]
# -----------------------------------------------------------------------------
# -- Exercise 2 
# --- Determine if the dataset below (exercise_2) is long or wide, and reshape 
# --- the dataset using one of the methods above
# -----------------------------------------------------------------------------


spread(exercise_2, key = Treatment, value = Result)

wide<-reshape(exercise_2, v.names = "Result", idvar = "Participant",
        timevar = "Treatment", direction = "wide")

# -----------------------------------------------------------------------------
# -- Exercise 3 
# --- Merge the reshaped dataset from Ex.2, and the exercise_3 dataset below
# -----------------------------------------------------------------------------
exercise_3 <- read.csv("Exercise 3.csv")
#wide <- dcast(exercise_2, Participant ~ Treatment, value.var = "Result")
merged <- merge(exercise_3, wide, by = "Participant")


# -----------------------------------------------------------------------------
# -- Exercise 5 
# --- Using one of the methods above, find the average Before & After Score 
# --- for each Gender and then each State
# ----------------------------------------------------------------------------- 
by(merged$Before, merged$Sex, mean)
by(merged$After, merged$Sex, mean)
# OR
aggregate(merged[, c("Before", "After")], by = list(merged$State), FUN = mean)
# OR
apply(merged[which(merged$Sex == "Male"), c("Before", "After")], 2, mean)


# -----------------------------------------------------------------------------
# -- Exercise 4 
# --- Create a function which takes the difference (After-Before) of the
# --- merged data set (ex. 4) and reports the mean difference
# -----------------------------------------------------------------------------
fn_mean_dif <- function(low_bound, up_bound) {
  diff <- up_bound - low_bound
  return(mean(diff))
}    

fn_mean_dif(merged$Result.Before, merged$Result.After)


# -----------------------------------------------------------------------------
# -- Exercise 6 
# --- Create a for loop which prints out the participants who improved their 
# --- score by at least 5 points
# -----------------------------------------------------------------------------
for(i in 1:nrow(merged)) {
  if ((merged$After[i] - merged$Before[i]) >= 5) {
    print(merged[i, ])
  }
}



