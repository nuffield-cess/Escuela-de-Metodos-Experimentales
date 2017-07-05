
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
# -- IV. Dataset Manipulation--
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

# Computing Variables 
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
# --- b. Create a new variable in e2 called e3, which is the sqrt of rate, divided by the idnum 
# --------------------------------------------------------------------------------------------------


# REcodificar una variable continua en una categórica
#---------------------------------
summary(health$age) 			
health$age_cat[health$age <= 32.5] <-"Group 1"
health$age_cat[health$age > 32.5 & health$age <= 50] <- "Group 2"
health$age_cat[health$age > 50] <-"Group 3"


# E. Recode function within the "car" package - From continuous to continuous

# install.packages("car")
# library(car)
# health$health22 <- recode(health$smoke, "1=5;2=4;3=3;4=2;5=1")
# health$health55 <- recode(health$alcohol, "1=5;2=4;3=3;4=2;5=1")

# F. Subsets of a Data Frame 

# Subsets by specifying the column name:
health[1:3, c("id", "gender", "smoke")]

# Subsets by specifying the row ###
health$age
which(health$age > 40)  # Returns indices of rows where logical statement is TRUE
which(health$age > 40 & health$age < 50)
which(health$age < 25 | health$age > 50)

sub1 <- health[which(health$age > 40), c("age","smoke")]
sub2 <- subset(health, age > 40, select = c("age","smoke"))

sub1 - sub2
# ----------------------------------------------------------------------------------------------------------
# -- Exercise 4 
# --- Using e2, create a subset called e4 which contains only the observations with a rate between 5 and 7 
# ----------------------------------------------------------------------------------------------------------


# -------------------------------------------
# -- V. Descriptive Statistics --
# ------------------------------------------- 

summary(health) 
summary(health$age)

# A. Continuous variable : age 
mean(health$age, na.rm = TRUE)
median(health$age, na.rm = TRUE)
sd(health$age, na.rm = TRUE)
quantile(health$age, na.rm = TRUE)


# B.  Categorical variable : gender 
table(health$gender)
prop.table(table(health$gender))

table(health$gender, health$age_cat)  

# Try it with margin.table as well 
margin.table(table(health$gender, health$age_cat), 1)
margin.table(table(health$gender, health$age_cat), 2)

# Question: how to find the row and column frequencies?

prop.table(margin.table(table(health$gender, health$age_cat), 1))
prop.table(margin.table(table(health$gender, health$age_cat), 2))

# C. Correlation 
cor(health[5:9])
plot(health[5:9])

# -----------------------------------------------------------------------------------
# -- Exercise 5 
# --- Using e2, create an appropriate set of statistics for the rate variable 
# -----------------------------------------------------------------------------------


# -------------------------------------------
# -- VI. Hypothesis Testing --
# ------------------------------------------- 

# Chi-square Test
chisq.test(health$gender, health$age_cat)
summary(table(health$gender, health$age_cat))

# t-test 
# One sample t test
t.test(health$health_sum, mu=3)
# Independent 2 group t test where y is numeric and x is a binary factor
?t.test(health_sum ~ gender, data = health)
# Paired t test
t.test(health$food, health$smoke, paired = TRUE) 
tResults <- t.test(health$food, health$health, paired = TRUE) 
summary(tResults)
tResults$statistic
tResults['statistic']

# Linear Regression Model
lm_health <- lm(health_sum ~ age + gender, data = health)
summary(lm_health)
par(mfrow = c(2, 2))
plot(lm_health)
confint(lm_health)

# ANOVA 
aov_health <- aov(health_sum ~ state + gender, data = health)
summary(aov_health)

# Generate a random sample from specific distribution 
# n=100 from N(0,1) distribution
rnorm(100)
?rnorm
# n=100 from U(0,1) distribution
runif(100)
?runif


# Density for specific distribution
par(mfrow = c(1, 1))  
x <- seq(-4, 4, length = 100)
y1 <- dnorm(x)
plot(x, y1, type = "l", lwd = 2, col = "blue")
y2 <- dnorm(x, m = 0, sd = 2)
lines(x, y2, type = "l", lwd = 2, col = "red")


# Cumulative distribution function : To get p-value, pnorm() function.
pnorm(1.96)



# Quantile function : To get quantiles or "critical values", qnorm( ) function. 
qnorm(0.95) # p = .05, one-tailed (upper)
qnorm(c(0.025, 0.975)) # p = .05, two-tailed


# ------------------------------------------------------------------------------
# -- Exercise 6 
# --- Using e2, create a histogram of rate with appropriate customizations
# ------------------------------------------------------------------------------





# -------------------------------------------
# -- I. Data Management  --
# -------------------------------------------

# Remove every objects in your working environment 
rm(list = ls())
# remove.packages()



# UScereal is the dataset inside the MASS package
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



# E. Reshape 
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



# F. Merge Datasets 
# Create two data with common variables
data1 <- data.frame(id = rep(1:5, 3), year = rep(2000:2002, each = 5), 
                    group = sample(c("A", "B", "C"), 15, replace = TRUE))



data2 <- data.frame(id = rep(1:5, each = 4), year = rep(2000:2003, 5),
                    score = rnorm(20, 50, 15)) 

data1
data2


# Merge them by id & year, 1:1 merge
data_merge <- merge(data1, data2, by = c("id", "year")) 
data_merge

# Extra rows from both datasets are added.
data_merge <- merge(data1, data2, by = c("id", "year"), all = TRUE) 
data_merge

# left-merge: x.all=TRUE. right-merge:y.all=TRUE


# dplyr package
library(dplyr)
inner_merge <- data1 %>% inner_join(data2, by = c("id", "year"))
outer_merge <- data1 %>% full_join(data2, by = c("id", "year"))

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
# -- II. Functions  --
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

# -------------------------------------------
# -- III. If/Else Statements  --
# -------------------------------------------

# If Statement  - Typically used inside Functions & Loops
x <- 9
if (x > 10) {
  print ("Greater than 10")
} else if (x == 10) {
  print ("Equal to 10")
} else if (x < 10 & x >= 0) {
  print ("Between 0 and 10")
} else {
  print ("Less than 0")
}

# If-Else Statement 
# ifelse(test, action if yes, action if no)
x <- seq(1:4)
ifelse(x < 4, "less than 4", "more than equal to 4")


