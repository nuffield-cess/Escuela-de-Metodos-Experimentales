# -------------------------------------------
# Introducción a R
#
# CESS Santiago Winter School 
# -------------------------------------------


# Intro opcional 1:   Intro a R
# Intro opcional 2:   Manejo de datos

# Módulo 1: Análisis de Datos 
# Módulo 2: Cálculos de Poder
# Módulo 3: Gráficos


# ------------------------------------------------------
# -------- Intro opcional 1:   Intro a R ---------------
# ------------------------------------------------------



# -----------------------------------------
# -- I. RStudio y su interfaz de usuario --
# -----------------------------------------


# Elementos de pantalla de RStudio:
# 1) Script
# 2) Global Environment
# 3) Impresión de resultados
# 4) Gráficos, packages, ayuda



# ----------------------
# -- II. R Basics --
# ----------------------


# Lenguaje: Comentarios con "#"
#--------------------------

1  # reconoce 1 como un vector de un sólo elemento
1 + 1
"a"
a  

 
# Objetos : <numbre objecto> <- <información contenida en el Objeto>
#--------------------------


valor1 <- 1 + 2
valor1	# Al imprimir el objeto se puede ser su valor			

valor2 <- "blue" # Pueden ser numéricos, characteres o factores
valor2				



# Ejemplo básico de Funciones 
#--------------------------

sqrt(2 ^ 3)
sqrt(valor1)
sqrt(valor2)
help(sqrt)			
?help
		
# Tipos de Datos
#--------------------------

# 1. Vectores
vec1 <- c(4.6, 4.8, 5.2, 6.3, 6.8, 7.1, 7.2, 7.4, 7.5, 8.6)    
vec2 <- c('UT', 'IA', 'MN', 'AL', 'WI', 'MN', 'OH', 'IA', 'NY', 'IA')
?c  # combina valores y los transforma en un vector
vec3<-c(1, "a", 2, 33, "z") # combinar número y valores hacer que el vector sea considerado un conjunto de characteres 
vec3


# Elementos de los vectores: <vector>[<índices>]
vec1[7]      
vec2[1:3]
vec3[c(1, 4)]

#subconjunto
vec4<-vec2[1:3]
vec3<-vec4 # Se pueden reemplazar valores en forma simple ¡¡¡Tengan cuidado!!!
vec3

# Largo de un Vector       	
length(vec1)
length(vec3)          


# Clase de vectores         
class(vec1)
class(vec3)

class(vec1)<-"character"  ### Se puede transformar de numérico a character
class(vec1)<-"numeric"  ### y de vuelta 

class(vec3)<-"numeric" ### Los characteres no-numericos no se pueden transformar a numéricos
vec3

vec3<-c(1, "a", 2, 33, "z") # combinar número y valores hacer que el vector sea considerado un conjunto de characteres 
class(vec3)

vec3<-as.numeric(vec3) ## transformar a numérico
vec3


vec5<-as.numeric(vec2) ## Produce un error

vec5<-as.numeric(as.factor(vec2)) ## Pero los factores se pueden transformar en categorías numéricas
vec5


# Funciones de un vector numérico
#--------------------------

mean(vec1)
var(vec1)
sd(vec1)
sum(vec1)
max(vec1)
		
# Resumen de vectores
#--------------------------

summary(vec1)
summary(vec2)
summary(as.factor(vec2)) 	# Si un vector es un factor se pueden obtener frecuencias
table(vec2) # equivalente


?as.factor()


# Operaciones con vectores
#--------------------------

vec1
vec1+1
vec1*5
rep(1, times = 10)   
rep(vec1, times = 5) 
round(vec1/3, 2)

# Combinar dos vectores en uno solo de largo = 20 
#--------------------------

vec3 <- c(vec1, vec2)   
vec3
length(vec3)

	
# ------------------------------------------------------------------------------------------
# -- Ejrecicio 1 
# --- Crear un vector llamado e1 que sea una secuencia de 1 a 10, repetida 5 veces 
# ------------------------------------------------------------------------------------------	


# Matrices 
#--------------------------

m1 <- cbind(vec1, vec2)  # prueben cbind(vec1, vec3) 
m1  # ¿en qué se diferencia del vec3?
class(m1)  # Toda las columnas de una matriz deben tener la misma clase(numeric/character...), 
dim(m1)

# Seleccionando valores dentro de una matriz usando paréntesis : <matrix>[<índice fila>,<índice columna>]
m1[, 1]
m1[2, ]

m1[2, 1]			

# 3. Bases de datos, alias "Data Frame" 
df1 <- data.frame(vec1, vec2)
class(df1) 			
str(df1) # estructura de la base de datos


# Seleccionando variables usando paréntesis : <dataframe>[<índice fila>,<índice columna>]
df1[, 1] 
df1[, 2]

df1[, 'vec1']

# Seleccionando variables/columnas usando sus nombres 
names(df1) #numbre de las variables

df1$vec1
df1$vec2
	
df1$vec2[1] # Objeto 1 de la variable vec2
df1$vec2[1:5]

# --------------------------------------------------------------------------------------------------
# -- Ejrecicio 2 
# --- Crear una base de datos llamada e2, en la cual se le agrega la variable (idnum, abajo) a df1
# --------------------------------------------------------------------------------------------------
idnum <- 1:10


# Listas
#--------------------------

l1 <- list(idnum, df1, 2) 
class(l1)

# Seleccionando Components
l1[[1]] 
l1[[2]] 

class(l1[[1]])
class(l1[1])

	
# Missing Data
#--------------------------

vec4 <- c(4.6, 4.8, 5.2, 6.3, 6.8, 7.1, 7.2, 7.4, 7.5, 8.6, NA)
sum(vec4)  #  ¿Por qué no se puede calcular la suma?
is.na(vec4) 
sum(vec4, na.rm = TRUE)
is.na(vec4)  # NA is still in the vector & only removed from the calculation


				
# --------------------------------
# -- III. Trabajando con datos  --
# --------------------------------  

getwd()  # Nos dice el directorio actual
setwd("C:/Users/Denise Laroze P/Documents/GitHub/Winter-School")  # seleccióna el directorio que se quiere utilizar 
dir()

# Leer bases de datos con formatos .csv (comma separated values) 
#--------------------------

health <- read.csv("Dataset.csv") ### ¿Error?
View(health)

health <- read.table("Dataset.csv", sep = ";", header = TRUE)  
class(health)
# También se puede hacer referencia a otra carpeta--  
# read.csv("/Users/.../.../datos.csv")   


# packages de R  - No todo se puede hacer con Base R, usuarios han escrito otras funciones accequibles mediate packages
#--------------------------

library() 			# Revisar los packages instalados en el computador
search() 			  # packages actualmente en uso 

# Para instalar un Package nuevo
# - Paso1: install.packages("nombre del package")
# - Paso2: library(nombre del package) 

# Para introducir datos de Stata se puede usar el package "foreign" o "readstata13" 
library(foreign,readstata13)

dfs12<-read.dta("data_stata12.dta")
dfs14<-read.dta13("data_stata14.dta")

read.table('Dataset.txt') # cuando los datos estan separados por espacios y en formato txt
#read.delim('Dataset.txt', delim = '\t')



# También se pueden leer bases de datos en formatos de SAS, SPSS, y Stata: package "haven"
# install.packages("haven")
# library(haven)
# read.spss("Dataset.sav")
# read.sas("Dataset.sas7bdat)


# Datos en formato Excel 
# install.packages("readxl")
# package lee archivos tant en xls como xlsx 
# library(readxl)
# read_excel("Dataset.xlsx")


# Guardar/Exportar bases de datos 
#-----------------------------------

write.csv(health, "IntrotoR.final.csv", row.names = FALSE) 
# o con el readr package
# write_csv(health, 'healthExam.csv')save.image("rdata.RData")


rm(list=ls()) ### Elimina todo

load("rdata.Rdata") ##abre todala información de R guardada 


# Use the haven package to export SPSS or Stata files
# write_spss(health, "my_spss.sav")
# write_dta(health, "my_stata.dta")



# -----------------------------------------------------------------------------------------
# Referencias
# 1.Modern Applied Statistics with S-PLUS, 2nd Edition, W.N. Venables & B.D. Repley 
# 2.Linear Models with R, Julian J. Faraway 
# 3.Manuals : browseURL("https://cran.r-project.org/manuals.html")
# 4.Quick-R : browseURL("http://www.statmethods.net")
# 5.UCLA : browseURL("https://stats.idre.ucla.edu/r/")
# 6.Cheat Sheet browseURL("https://cran.r-project.org/doc/contrib/Short-refcard.pdf")
# -----------------------------------------------------------------------------------------










