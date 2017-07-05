
# -------------------------------------------
# Introduction to R
#
# Essex Nuffield Summer School 
# -------------------------------------------

setwd("~/GitHub/Winter-School")
rm(list=ls())

# ---------------------------------------------
# -------- Intro opcional 2: Manejo de datos --
# ---------------------------------------------


## 
# -------------------------------------------
# --  Dataset Manipulation I --
# -------------------------------------------  


health <- read.table("Dataset.csv", sep = ";", header = TRUE)  
View(health)


# Nombres de columnas (variables)
#--------------------------------

names(health)
names(health)[5:10] <- c("food", "smoke", 
                         "exercise", "happy",
                         "alcohol", "doctor")
names(health)
# tb se puede cambiar el numberde una variable específica
#names(health)[names(health)=='health1'] <- "food"



# Trabajando con datos que contienen Missing Data
#--------------------------

health$age

which(health$age == -1) #Identfica el/los objectos relevantes
health$age[which(health$age == -1 )] <- NA
is.na(health$age)
table(is.na(health$age))

mean(health$age)
mean(health$age, na.rm = TRUE)

# Creando Variables 
#--------------------------

# Nueva variable = suma de las variables de salud
health$health_sum <- rowSums(health[,c("food", "smoke", 
                                      "exercise", "happy",
                                      "alcohol", "doctor")])

# método alternativo
apply(health[, 5:10], 1, sum) # o varianza por columna: apply(health[, 5:10], 2, var)

# Crear media de salud
health$health_avg <- health$health_sum / 6

# liminar una variable
health$health_avg <- NULL

# --------------------------------------------------------------------------------------------------
# -- Ejercicio 3 
# --- a. En e2 (de los ejercios de ayer), cambiar el nombre del vec1 a 'rate' y el vec2 a 'state'
# --- b. Crear una nueva varible = "raíz" cuadrada de rate dividida por "idnum" 
# --- nota: sólo las variables numéricas se puede usar en funciones matemáticas
# --------------------------------------------------------------------------------------------------


# Recodificar una variable continua en una categórica
#---------------------------------
summary(health$age) 			
health$age_cat[health$age <= 32.5] <-"Grupo 1"
health$age_cat[health$age > 32.5 & health$age <= 50] <- "Grupo 2"
health$age_cat[health$age > 50] <-"Grupo 3"


# Función de recodificación con el "car" package - de contínuo a contínuo
#---------------------------------

# install.packages("car")
# library(car)
# health$health22 <- recode(health$smoke, "1=5;2=4;3=3;4=2;5=1")
# health$health55 <- recode(health$alcohol, "1=5;2=4;3=3;4=2;5=1")

# F. Subsets of a Data Frame 

# Subconjuntos de datos utilizando el nombre de la columna:
health[1:3, c("id", "gender", "smoke")]

# otra alternativa
subset(health, age_cat=="Grupo 1")

# otra más especificando por valores de las observaciones (filas)
health$age
which(health$age > 40)  # Informa las filas (NO los valores) donde el valor lógico es TRUE 
which(health$age > 40 & health$age < 50) # & = y
which(health$age < 25 | health$age > 50) # | = ó

sub1 <- health[which(health$age > 40), c("age","smoke")]
sub2 <- subset(health, age > 40, select = c("age","smoke")) #alternativa

sub1 - sub2
# ----------------------------------------------------------------------------------------------------------
# -- Ejercicio 4
# --- Usando e2, crear un subconjunto llamado e4 que sólamente contiene las observaciones
# --- con "rate" entre 5 y 7 
# ----------------------------------------------------------------------------------------------------------


# -------------------------------
# -- Estadísticas Descriptivas --
# ------------------------------- 

summary(health) 
summary(health$age)

# Variable contínua: age 
mean(health$age, na.rm = TRUE)
median(health$age, na.rm = TRUE)
sd(health$age, na.rm = TRUE)
quantile(health$age, na.rm = TRUE)


# Variable categórica: gender 
table(health$gender) # Frecuencias
prop.table(table(health$gender)) # proporciones

table(health$gender, health$age_cat)  # Frecuencias 2x2

# Margin.table as well 
margin.table(table(health$gender, health$age_cat), 1) # ¿por qué sale ese valor?
margin.table(table(health$gender, health$age_cat), 2)

#Proporciones por fila y columna 

prop.table(margin.table(table(health$gender, health$age_cat), 1))
prop.table(margin.table(table(health$gender, health$age_cat), 2))
prop.table(table(health$gender, health$age_cat), 2)

# Correlation 
cor(health[c("food", "smoke", "exercise", "happy", "alcohol")])
cor.test(health$food, health$smoke)


#graficar correlación
plot(health$food, health$smoke)
plot(health[5:9])

library(car)
scatterplot(health$food, health$smoke)



# ----------------------------------------------------------------------------------------
# -- Ejercicio 5 
# --- Usando e2, crear un set apropiado de estadísticas descripticas de la variable "rate" 
# ----------------------------------------------------------------------------------------


# --------------------------
# -- Condiciones If/Else  --
# --------------------------

# If Statement  - típicamente usado dentro de funciones o loops 
x <- 9
if (x > 10) {
  print ("Mayor que 10")
} else if (x == 10) {
  print ("Igual a 10")
} else if (x < 10 & x >= 0) {
  print ("Entre 0 y 10")
} else {
  print ("Menos que 0")
}

# If-Else Statement 
# ifelse(test, acción if sí, acción if no)
x <- seq(1:4)
ifelse(x < 4, "Menos que 4", "mayor o igua a 4")

# Uso en bases de datos
health$condición<- ifelse(health$health_sum == median(health$health_sum)  , "Mediana",
                          ifelse(health$health_sum < mean(health$health_sum), "Peor", "Mejor"))



# ----------------------------------------------------------------------------------------
# -- Ejercicio 6 
# --- Usando e2, crear un ifelse statement de al menos cuatro categorías 
# ----------------------------------------------------------------------------------------


# ----------------------
#  -- Merge Datasets  --
# ----------------------
# Crear dos bases de datos con variables en común
data1 <- data.frame(id = rep(1:5, 3), year = rep(2000:2002, each = 5), 
                    group = sample(c("A", "B", "C"), 15, replace = TRUE))



data2 <- data.frame(id = rep(1:5, each = 4), year = rep(2000:2003, 5),
                    score = rnorm(20, 50, 15)) 

View(data1)
View(data2)


# Merge mediante id & year, 1:1 merge
data_merge <- merge(data1, data2, by = c("id", "year")) ## sólo se mantienen las variables que se combinan
View(data_merge)

# Manteniendo todos los datos.
data_merge <- merge(data1, data2, by = c("id", "year"), all = TRUE) 
View(data_merge)

# left-merge: x.all=TRUE. right-merge:y.all=TRUE


# dplyr package
library(dplyr)
inner_merge <- inner_join(data1, data2, by = c("id", "year"))
outer_merge <- full_join(data1, data2, by = c("id", "year"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -----------------------------------
# -- Dataset Manipulation II  --
# -----------------------------------

# UScereal  es una base de datos dentro del MASS package
library(MASS)
data(UScereal)
head(UScereal)

# C. Selecting Cases 
which(UScereal$mfr == 'K')  # reports indicies       
which(UScereal$calories > 250)

# Use indices to select rows w/ selected columns                                              
UScereal[which(UScereal$mfr == 'K'), c('mfr', 'calories')]

# subset function 
subset(UScereal, calories > 250, c('mfr', 'calories'))

# with dplyr, using the %>% operator
# select rows
install.packages('dplyr')
library(dplyr)
UScereal %>% filter(calories > 250)

# select columns 
UScereal %>% select(mfr, calories)

# select rows and columns 
UScereal %>% filter(calories > 250) %>% select(mfr, calories)

# D. Sorting Data 
sort(UScereal$calories)  
sort(UScereal)  # sort() only works for vectors

order(UScereal$calories)  
UScereal[order(UScereal$calories), c('mfr', 'calories')] 

UScereal[order(UScereal$mfr, -UScereal$calories), ]

# with dplyr
library(dplyr)
UScereal %>% arrange(mfr, desc(calories)) 


# -----------------------------------------------------------------------------
# -- Exercise 1 
# --- Create a subset of mydata, which contains the 25 highest v1 scores
# -----------------------------------------------------------------------------  

# Reshape 
# Make a Panel Dataset
health <- data.frame(id = rep(1:10, each = 4, len = 40),
                     trial = rep(c(1:4), 10), 
                     score = rnorm(40, 3, .75))
health[1:5, ]

# Reshape : Long --> Wide
health_wide <- reshape(health, v.names = "score", idvar = "id", 
                       timevar = "trial", direction = "wide") 
health_wide[1:5, ]
head(reshape(health_wide)) # to go back to long format

# Reshape : Wide --> Long
health_long <- reshape(health_wide, idvar = "id", direction = "long")         
health_long[1:5, ] 

# Using the tidyr package
# install.packages('tidyr')
library(tidyr)
spread(health, key = trial, value = score) # key is the identifier
gather(health_wide, key = trial, value = score, score.1:score.4) # can also reference by column number(2:5)


# -----------------------------------------------------------------------------
# -- Exercise 2 
# --- Determine if the dataset below (exercise2) is long or wide, and reshape 
# --- the dataset using one of the methods above
# -----------------------------------------------------------------------------
# setwd to the Data Management folder
setwd("C:/Users/Sonke.Ehret/Dropbox/CESS_Shared/Summer Schools 2017/Oxford/R_intro") 
exercise_2 <- read.csv("Exercise 2.csv")


# -----------------------------------------------------------------------------
# -- Exercise 3 
# --- Merge the reshaped dataset from Ex.2, and the exercise_3 dataset below
# -----------------------------------------------------------------------------
exercise_3 <- read.csv("Exercise 3.csv")



# G. Apply function and Aggregate Statistics 
# Apply mean() across numeric nutrition variables in the UScereal dataset     
apply(UScereal[, c(2:8, 9)], MARGIN = 1, FUN = mean)  
apply(UScereal[, c(2:8, 9)], MARGIN = 2, FUN = mean)

# Apply sd() across numeric nutrition variables in the UScereal dataset      
lapply(UScereal[, c(2:8, 9)],sd)
sapply(UScereal[, c(2:8, 9)], sd)

# tapply() instead of by factors : return an array.
tapply(UScereal$calories, UScereal$mfr, summary)

# Summary statistics of 'cty' grouped by 'trans'            
by(UScereal$calories, UScereal$mfr, summary)
by(UScereal$calories, list(UScereal$mfr, UScereal$shelf), summary) 

# dplyr 
library(dplyr)
UScereal %>% group_by(mfr) %>% summarize(avg.cal = mean(calories))

UScereal %>% group_by(mfr) %>% summarize(avg.cal = mean(calories), count = n())

UScereal %>% group_by(mfr) %>% mutate(avg.cal = mean(calories), count = n())

#mutate(UScereal,avg.cal = mean(UScereal$calories))








# -------------------------------------------
# -- Funcciones  --
# -------------------------------------------

#  <function.name> <- function(arg1, arg2, ...) {
#                     function_body
#                     return(any_value_to_return)
#                   }

# Create your own functions
addTwoNums <- function(a, b) {
  tmp <- a + b
  return(tmp)
  # Alternatively either of the below would substitue for the above
  # return(a + b)
  # a + b
}

addTwoNums(2, 1)
addTwoNums(5)  # Does it work? 

addTwoNums <- function(a, b = 2) {
  return(a + b)
}

addTwoNums(5)
addTwoNums(a = c(4, 10, 0))  # Multiple arguments at the same time
addTwoNums(3, 4) # How does it work?

# Multiple results to report 
myOperations <- function(a, b) {
  add <- a + b
  subtract <- a - b
  multiply <- a * b
  divide <- a / b
  mylist <- list(add, subtract, multiply, divide)
  return(mylist)
}

myOperations(5, 10)


# -----------------------------------------------------------------------------
# -- Exercise 4 
# --- Create a function which takes the difference (After-Before) of the
# --- merged data set (ex. 4) and reports the mean difference
# -----------------------------------------------------------------------------



# A more complicated example
# Create your own t-test function
my_ttest <- function(x, mu = 0, test = "two-sided", alpha = 0.05) {
  n <- length(x)
  df <- n - 1
  std <- sqrt(var(x))
  t <- sqrt(n) * (mean(x) - mu) / std  
  tail_area <- switch(test, "two-sided" = 2 * (1 - pt(abs(t), n - 1)),
                      lower = pt(abs(t), df), upper = 1 - pt(abs(t), df))
  list(t.statistics = t, degree.freedom = df, p.value = tail_area)
}

my_ttest(v1)

switch(1, c("one", "two"),c("three", "four"))
switch(2, c("one", "two"),c("three", "four"))




