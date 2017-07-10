
#################################################
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS-Santiago
#################################################-




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