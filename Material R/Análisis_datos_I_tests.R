

##########################################
## Paper: Cheating is a national Pastime?
## Author code: Denise Laroze
## Year: 2017
##########################################



library(foreign)
library(ggplot2)
library(readstata13)
library(RColorBrewer)
library(rms)
theme_set(theme_bw())
library(plyr)
library(stargazer)
library(gridExtra)
library(clusterSEs)
library(car)
library(sandwich)


rm(list=ls())

setwd("~/GitHub/Winter-School/Material R")
fig.path <- "Figures"
v<-"07Jul2017"

#dat<-read.csv("Masterfile_2016_Dec.csv", sep=";")
dat <- read.csv("cheating_escuela_metodos.csv")


####################
### Subsets of data
####################

dat.2<-dat[dat$auditrate==0, ]
dat.2<-dat.2[dat.2$treatment_lab!="Redistribution",]

####  Baseline Russia
dat.2$country <- factor(dat.2$country, levels = c("Russia", "UK", "Chile"))

uk<-subset(dat.2,  country=="UK")
cl<-subset(dat.2,  country=="Chile")
ru<-subset(dat.2,  country=="Russia")


### Subsets of data by treatment
baseline.uk<-subset(dat.2, treatment_lab=="Baseline" & country=="UK")
baseline.cl<-subset(dat.2, treatment_lab=="Baseline" & country=="Chile")
baseline.ru<-subset(dat.2, treatment_lab=="Baseline"  & country=="Russia")

status.uk<-subset(dat.2, treatment_lab=="Status" & country=="UK")
status.cl<-subset(dat.2, treatment_lab=="Status" & country=="Chile")
status.ru<-subset(dat.2, treatment_lab=="Status" & country=="Russia")

shock.uk<-subset(dat.2, treatment_lab=="Shock" & country=="UK")
shock.cl<-subset(dat.2, treatment_lab=="Shock" & country=="Chile")
shock.ru<-subset(dat.2, treatment_lab=="Shock" & country=="Russia")


nof.uk<-subset(dat.2, treatment_lab=="Non-fixed" & country=="UK")
nof.cl<-subset(dat.2, treatment_lab=="Non-fixed" & country=="Chile")
nof.ru<-subset(dat.2, treatment_lab=="Non-fixed" & country=="Russia")




################################# 
### EStadísticas de resúmen
#################################
library(xtable)


###########################
### Sesiones Experimentales
###########################
# Total
N.Subjects <- length(unique(dat.2$subj_id))
N.Subjects
N.Session <-length(unique(dat.2$session))
N.Session

# By country
cdata <- ddply(dat.2, c("country"), summarise,
               N.Subjects    = length(unique(subj_id)),
               N.Session = length(unique(session))
)

xtable(cdata)


# Treatments and sessions
cdata <- ddply(dat.2, c("treatment_lab", "country"), summarise,
               N.Gender    =  length(unique(subj_id)),
               N.Session = length(unique(session))
               #Mean.cheat = mean(cheat)
)
xtable(cdata)               

################################
### Mediana respuestas correctas
################################

median(uk$ncorrectret)
median(ru$ncorrectret)
median(cl$ncorrectret)



##########################
## Tests de Hipothesis  ##
##########################

# Chi-square Test
chisq.test(dat.2$cheat, dat.2$perform_high) # variables categóricas

table(dat.2$cheat, dat.2$perform_high)
summary(table(dat.2$cheat, dat.2$perform_high)) #Chisq

# t-test # hyp: La mitad de la población miente 
# One sample t test
t.test(dat.2$cheat, mu=0.5) 


# T-est de dos grupos, donde y es numérico y x es binario
t.test(cheat ~ gender, data = dat.2)

# Paired t test
t.test(health$food, health$smoke, paired = TRUE) 
tResults <- t.test(health$food, health$health, paired = TRUE) 
summary(tResults)
tResults$statistic
tResults['statistic']


# ANOVA 
aov_health <- aov(health_sum ~ state + gender, data = health)
summary(aov_health)


####################################
#### Country level comparisons
#####################################

####### 0% Percent Audit
m.all<- lm(formula = percevaded ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2)
summary(m.all)

par(mfrow = c(2, 2))
plot(m.all)
confint(m.all)




m.all.r<-coeftest(m.all,vcovHC(m.all))
m.all.r





m.all.int<- lrm(formula = percevaded ~ ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2, x=T, y=T)
m.all.int.cl <- robcov(m.all.int, dat.2$subj_id)


m.uk<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=uk, x=T, y=T)
m.uk.cl <- robcov(m.uk, uk$subj_id)


m.cl<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
           data=cl, x=T, y=T)
m.cl.cl <- robcov(m.cl, cl$subj_id)


m.ru<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
           data=ru, x=T, y=T)
m.ru.cl <- robcov(m.ru, ru$subj_id)

stargazer(m.all.cl, m.all.int, m.uk.cl, m.cl.cl, m.ru.cl,
          order=c(1,5,6,2,3, 4),
          type = "html", out="interaction.html")

stargazer(m.all.cl, m.all.int, m.uk.cl, m.cl.cl, m.ru.cl,
          dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", 
                             "\\# of Additions*UK", "\\# of Additions*Chile","UK", "Chile", 
                             "Gains from Cheating",
                            "Constant"),
          order=c(1,5,6,2,3, 4),
          keep.stat = c("n"),
          #add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          add.lines = list(c("Countries", "All", "All", "UK", "Chile", "Russia")),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          omit.table.layout = "n",
          out=paste0("Tables/table0_", v, ".tex"))


#############################
### Analysis by treatment
#############################

m.b<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2[dat.2$treatment_lab=="Baseline",], x=T, y=T)
m.b.cl <- robcov(m.b, dat.2$subj_id[dat.2$treatment_lab=="Baseline"])

m.b.c<- lrm(formula = cheat ~ ncorrectret*country + cost_comply ,# + ideology + gender + age_subject, 
          data=dat.2[dat.2$treatment_lab=="Baseline",], x=T, y=T)
m.b.c.cl <- robcov(m.b.c, dat.2$subj_id[dat.2$treatment_lab=="Baseline"])

#joint significance test
m.b.c2<- glm(formula = cheat ~ ncorrectret + ncorrectret:country + cost_comply ,# + ideology + gender + age_subject, 
             data=dat.2[dat.2$treatment_lab=="Baseline",], family=binomial(link = "logit"))
linearHypothesis(m.b.c2, "ncorrectret + ncorrectret:countryUK")
linearHypothesis(m.b.c2, "ncorrectret + ncorrectret:countryChile")
#


m.st<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
          data=dat.2[dat.2$treatment_lab=="Status",], x=T, y=T)
m.st.cl <- robcov(m.st, dat.2$subj_id[dat.2$treatment_lab=="Status"])

m.st.c<- lrm(formula = cheat ~  ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2[dat.2$treatment_lab=="Status",], x=T, y=T)
m.st.c.cl <- robcov(m.st.c, dat.2$subj_id[dat.2$treatment_lab=="Status"])


m.sh<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
          data=dat.2[dat.2$treatment_lab=="Shock",], x=T, y=T)
m.sh.cl <- robcov(m.sh, dat.2$subj_id[dat.2$treatment_lab=="Shock"])

m.sh.c<- lrm(formula = cheat ~  ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2[dat.2$treatment_lab=="Shock",], x=T, y=T)
m.sh.c.cl <- robcov(m.sh.c, dat.2$subj_id[dat.2$treatment_lab=="Shock"])


m.nf<- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
           data=dat.2[dat.2$treatment_lab=="Non-fixed",], x=T, y=T)
m.nf.cl <- robcov(m.nf, dat.2$subj_id[dat.2$treatment_lab=="Non-fixed"])

m.nf.c<- lrm(formula = cheat ~  ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
             data=dat.2[dat.2$treatment_lab=="Non-fixed",], x=T, y=T)
m.nf.c.cl <- robcov(m.nf.c, dat.2$subj_id[dat.2$treatment_lab=="Non-fixed"])




stargazer(m.b.cl, m.b.c.cl, m.st.cl, m.st.c.cl, m.sh.cl, m.sh.c.cl, m.nf.cl, m.nf.c.cl)


stargazer(m.b.cl, m.b.c.cl, m.st.cl, m.st.c.cl, m.sh.cl, m.sh.c.cl, m.nf.cl, m.nf.c.cl,
          covariate.labels=c("\\# of Additions", 
                             "\\# of Additions*UK", "\\# of Additions*Chile","UK", "Chile", 
                             "Gains from Cheating",
                             "Constant"),
          order=c(1,5,6,2,3, 4),
          dep.var.labels.include = F,
          keep.stat = c("n"),
          #add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          add.lines = list(c("Countries", "All", "All", "All", "All",  "All", "All", "All", "All"),
                           c("Treatments","Baseline","Baseline", "Status", "Status", "Shock","Shock", "Non-Fixed", "Non-Fixed")),
          dep.var.caption = "",
          #star.char = c("", "", ""),
          omit.table.layout = "n",
          out=paste0("Tables/table01_1_", v, ".tex"))



####################################################################################
### Repetir los modelos anteriores utilizando una base de datos propia.
### De no contar con una, utilizar la siguiente base
### data1 <- read.csv(file="http://hdl.handle.net/10079/70rxwqn",head=TRUE,sep=",")
### Más información en http://isps.yale.edu/research/publications/isps00-001
####################################################################################



