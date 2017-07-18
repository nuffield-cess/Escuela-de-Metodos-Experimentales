
#################################################
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS-Santiago
#################################################-




# ---------------------------------------------------
# -- I. Computing Treatment effects
# ---------------------------------------------------

## simulation parameters
n <- 100 # sample size
mu0 <- 0 # mean of Y_i(0)
sd0 <- 1 # standard deviation of Y_i(0)
mu1 <- 1 # mean of Y_i(1)
sd1 <- 1.5 # standard deviation of Y_i(1)

## generate a sample

#Control
Y0 <- rnorm(n, mean = mu0, sd = sd0)
#Treatment
Y1 <- rnorm(n, mean = mu1, sd = sd1)
#compliance
C <- rep(c(0,1),n/2)

tau <- Y1 - Y0 # individual treatment effect

## true value of the sample average treatment effect
ATE <- mean(tau)
ATE

Y<-data.frame(Y1, Y0, C)

#intent-to-treat effect: compare outcomes when subjects are assigned to treatment (rather than control)

Y$Y1_C<-Y$Y1*Y$C
Y$Y0_C<-Y$Y1*Y$C

ITT<-mean(Y$Y1_C-Y$Y0_C)

# complier average causal effect. treatment effect for compliers
CACE<-mean(Y1[which(Y$C==1)]-Y0[which(Y$C==1)])

# Alternative way of producing CACE
# ITT_D proportion of compliers - aka 'sigma' in lecture notes.

ITT_D=length(C[C==1])/length(C)
ITT/ITT_D


# ---------------------------------------------------
# -- II. Randomization Inference using Monte Carlo --
# ---------------------------------------------------


## repeatedly conduct randomized controlled trials
sims <- 5000 # repeat 5,000 times, we could do more
diff.means <- rep(NA, sims)  # container

for (i in 1:sims) {
  ## randomize the treatment by sampling of a vector of 0's and 1's
  treat <- sample(c(rep(1, n / 2), rep(0, n / 2)), size = n, replace = FALSE)
  ## difference-in-means
  diff.means[i] <- mean(Y1[treat == 1]) - mean(Y0[treat == 0])
}

hist(diff.means, breaks = 50)
abline(v = ATE, size = 2, col = 'red')


prop.table(table(diff.means > ATE))


#proportion of indidividual treatment effects greatwer than zero
length( diff.means[ diff.means>0])/length( diff.means)


t.test(Y1, Y0, alternative = c("greater"))




# ----------------------------------------------------------
# -- III. Randomization Inference Using the ri package  ----
# ----------------------------------------------------------  



# simulation in which 2 of 7 villages (example from lecture)  are assigned to treatment

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible

# input full schedule of potential outcomes
# using Table 2.1

#Control group
Y0 <- c(10,15,20,20,10,15,15)
#Treatment group
Y1 <- c(15,18,30,18,20,15,33)



# create a vector of possible treatment assignments 
Z  <- c(rep(1,2),rep(0,8))

# create a potential outcomes object called a data frame

Ys <- data.frame(Y0,Y1)
# check column means
colMeans(Ys)


# We can also estiamte the ate when we have only the observed outcomes y, treatment assignment Z, and probs
# ate <- estate(y,Z,prob=probs)


probs <- genprobexact(Z,blockvar=NULL)  # inputs imply equal-probability assignment
# verify that probability of treatment is constant across the sample
table(probs)


# Randomization Inference

# generate all permutations of Z under _complete_ random assignment
# note that default is to do every possible permutation if less than 10,000 permutations

perms <- genperms(Z)

# show number of permutations
ncol(perms)


# calculate the sampling distribution of estimated difference-in-means
truedist <- gendist(Ys,perms,Ypre=NULL,prob=probs,HT=FALSE)

# display the frequency distribution of the sampling distribution
table(truedist)

# graphically display the sampling distribution
dispdist(truedist,0)

# show the ATE estimate for each random assignment
truedist

mean(truedist)


sum((truedist-mean(truedist))^2)

se<-sqrt(sum((truedist-mean(truedist))^2)/length(truedist))


# calculate the proportion of estimates that are above zero

length(truedist[truedist > 0])
length(truedist[truedist > 0])/length(truedist)







# -------------------------------------------
# --              CACE                     --
# ------------------------------------------- 


#Clear any previous work
rm(list=ls(all=TRUE))

#Load Relevant packages
library(AER)
library(sandwich)

# Canvassing (mailing) experiments data

data1 <- read.csv(file="http://hdl.handle.net/10079/70rxwqn",head=TRUE,sep=",")

# You can check that your data was read in correctly using these two commands:
# colnames(data1)
# dim(data1)


# select one-person households that were either pure controls or canvass only
sel <-  data1$onetreat==1 & data1$mailings==0 & data1$phongotv==0 & data1$persons==1

# verify the number of observations
table(sel)
data2 <- data1[sel,]

# define variables
# V98: voted in '98
v98      <- data2$v98
#PERSNGRP: Personal contact attempted;
persngrp <- data2$persngrp

#cntany: any contact(compliance)
cntany   <- data2$cntany

############  NOTE USE OF ROBUST STANDARD ERRORS

#  ITT
coef(summary(lm(v98 ~ persngrp)))
# robust SEs
itt_fit <- lm(v98 ~ persngrp)
coeftest(itt_fit,vcovHC(itt_fit))


#  ITT_D
# Note that results from this will vary based on the current version that you have but this variation should not be a concern. 
coef(summary(lm(cntany ~ persngrp)))
# robust SEs
itt_d_fit <- lm(cntany ~ persngrp)
coeftest(itt_d_fit,vcovHC(itt_d_fit))


#  CACE
coef(summary(ivreg(v98 ~ cntany,~persngrp)))
# robust SEs
cace_fit <- ivreg(v98 ~ cntany,~persngrp)
coeftest(cace_fit,vcovHC(cace_fit))

#compute CACE manually. CACE=ITT/ITT_D

CACE<-itt_fit$coefficients[2]/itt_d_fit$coefficients[2]