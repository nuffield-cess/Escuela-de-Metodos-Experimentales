

# -----------------------------------------------
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS Santiago
# -----------------------------------------------





# -------------------------------------------
# -- I. Hypothesis Testing --
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
# -- Exercise 1 
# --- Using e2, create a histogram of rate with appropriate customizations
# ------------------------------------------------------------------------------








