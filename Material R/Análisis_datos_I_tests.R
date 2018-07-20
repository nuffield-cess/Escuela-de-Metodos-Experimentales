

#################################################
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS-Santiago
#################################################



library(foreign)
library(ggplot2)
library(readstata13)
library(rms)
theme_set(theme_bw())
library(plyr)
library(stargazer)
library(car)
library(sandwich)
#library(plm)
library(texreg)
library(AER)

rm(list=ls())

setwd("~/GitHub/Winter-School/Material R")
fig.path <- "Figures"
v<-"26Jul2018"

#dat<-read.csv("Masterfile_2016_Dec.csv", sep=";")
dat <- read.csv("cheating_escuela_metodos.csv")


##########################
### Subconjuntos de datos 
##########################

dat.2<-dat[dat$auditrate==0, ]
dat.2<-dat.2[dat.2$treatment_lab!="Redistribution",] # Eliminar un tratamiento por pocas observaciones

####  Por país
uk<-subset(dat.2,  country=="UK")
cl<-subset(dat.2,  country=="Chile")
ru<-subset(dat.2,  country=="Russia")


### Por tratamiento y país
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

# por país
cdata <- ddply(dat.2, c("country"), summarise,
               N.Subjects    = length(unique(subj_id)),
               N.Session = length(unique(session))
)

xtable(cdata)


# tratamientos y número de sesiones
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
mean(dat$realdie, na.rm=T) # Con dado real
mean(dat$digitaldie, na.rm=T) # Con dado digital
t.test(dat$realdie, dat$digitaldie, paired = TRUE) 

tResults <- t.test(dat$realdie, dat$digitaldie, paired = TRUE) 
summary(tResults)
tResults$statistic
tResults['statistic']


# ANOVA 
aov <- aov(percevaded ~ ncorrectret + cost_comply, data = dat.2)
summary(aov)



############################
#### Regresiones lineales
############################

####### Comparación entre países con 0% audit

# Todos los países juntos
m.all<- lm(formula = percevaded ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
            data=dat.2)
summary(m.all)

par(mfrow = c(2, 2))
plot(m.all)
confint(m.all)

#Corrección de hetersckedasticidad
m.all.r<-coeftest(m.all,vcovHC(m.all))
m.all.r

# UK
m.uk<- lm(formula = percevaded ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=uk)
m.uk.r <- coeftest(m.uk,vcovHC(m.uk))


# Chile
m.cl<- lm(formula = percevaded ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
           data=cl)
m.cl.r <- coeftest(m.cl,vcovHC(m.cl))


# Rusia
m.ru<- lm(formula = percevaded ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
           data=ru)
m.ru.r <- coeftest(m.ru,vcovHC(m.ru))


# Interacciones
m.all.int<- lm(formula = percevaded ~ ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
               data=dat.2)
m.all.int.r <- coeftest(m.all.int,vcovHC(m.all.int))

#### Exportar datos
# solo ver la tabla
stargazer(m.all.r,  m.uk.r, m.cl.r, m.ru.r, m.all.int.r,
          order=c(1,5,6,2,3, 4))

# html para copiar a word
stargazer(m.all.r,  m.uk.r, m.cl.r, m.ru.r, m.all.int.r,
          order=c(1,5,6,2,3, 4), 
          type = "html", out="Tables/interaction.html")

# exportar a latex
stargazer(m.all.r,  m.uk.r, m.cl.r, m.ru.r, m.all.int.r,
          dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", 
                             "\\# of Additions*UK", "\\# of Additions*Chile","UK", "Chile", 
                             "Gains from Cheating",
                            "Constant"),
          order=c(1,5,6,2,3, 4),
          keep.stat = c("n"),
          #add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          add.lines = list(c("Countries", "All",  "UK", "Chile", "Russia", "All")),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          omit.table.layout = "n",
          out=paste0("Tables/table0_", v, ".tex"))


#############################
### Interacciones
#############################

#joint significance test
int<- lm(percevaded ~ ncorrectret*country + cost_comply,# + ideology + gender + age_subject, 
             data=dat.2)
# Otra alternativa para visualizar resultados
plotreg(int)

# Joint hypothesis test
linearHypothesis(int, "ncorrectret + ncorrectret:countryUK")
linearHypothesis(int, "ncorrectret + ncorrectret:countryRussia")

# Visualización de interacciones
ggplot(dat.2, aes(x = ncorrectret , y=percevaded , group = country, color = country)) + 
  geom_point() +
  geom_smooth(method = lm, se=T) +
  xlab("N correct RET ") +
  ylab(" % evaded")

## Otra alternativa para visualizar interacciones
library(effects)
eff_cf <- effect("ncorrectret*country", int)
print(plot(eff_cf, multiline=F))
print(plot(eff_cf, multiline=T))

########################
### Análisis de panel
########################
library(plm)


summary(lm(percevaded ~ ncorrectret + cost_comply, dat.2)
)

# equivalente a una regression lineal simple
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
            effect = c("individual"),
            model = c("pooling"),
            index = c("period","subj_id"))
)

## Individual fixed effects
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
    effect = c("individual"),
    model = c("within"),
    index = c("period","subj_id"))
)


## Time fixed effects
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
            effect = c("time"),
            model = c("within"),
            index = c("period","subj_id"))
)


# twoway fixed effect
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
            effect = c("twoways"),
            model = c("within"),
            index = c("period","subj_id"))
)

# Random effect
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
            model = c("random"), 
            index = c("period","subj_id"))
)


# First Diference
summary(plm(percevaded ~ ncorrectret + cost_comply, dat.2, 
            effect = c("individual"),
            model = c("fd"), 
            index = c("period","subj_id"))
)


############################
### Modelos no lineales
############################


?glm()

# Logit
logit<- glm(formula = cheat ~ ncorrectret + ncorrectret:country + cost_comply ,# + ideology + gender + age_subject, 
             data=dat.2, family=binomial(link = "logit"))

# Probit

probit<- glm(formula = cheat ~ ncorrectret + ncorrectret:country + cost_comply ,# + ideology + gender + age_subject, 
            data=dat.2, family=binomial(link = "probit"))
?glm()



######################
### Intent to Treat
######################


iv<-ivreg(percevaded ~ cost_comply + ncorrectret | ncorrectret + treatment_lab + auditrate, data= dat.2)

summary(iv)

####################################################################################
### Repetir los modelos anteriores utilizando una base de datos propia.
### De no contar con una, utilizar la siguiente base
### data1 <- read.csv(file="http://hdl.handle.net/10079/70rxwqn",head=TRUE,sep=",")
### Más información en http://isps.yale.edu/research/publications/isps00-001
####################################################################################



