# -----------------------------------------------
# Análisis de datos usando R: Gráficos
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS Santiago
# -----------------------------------------------

rm(list=ls())

setwd("~/GitHub/Winter-School/Material R")
fig.path <- "Figures"
v<-"07Jul2017"

#dat<-read.csv("Masterfile_2016_Dec.csv", sep=";")
dat <- read.csv("cheating_escuela_metodos.csv")


#########################
### subconjuntos de datos
#######################

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






##########################
### Figures
############################
###########

####################################
#### Percent Evaded by Performance
####################################
library(Rmisc)
tgc <- summarySE(dat.2, measurevar="percevaded", groupvars=c("treatment2_lab", "perform_high", "country"), na.rm=T)
tgc<-tgc[!is.na(tgc$treatment2_lab),]

tgc$perform_high[tgc$perform_high==1]<-"High Performance"
tgc$perform_high[tgc$perform_high==0]<-"Low Performance"
tgc<-tgc[tgc$treatment2_lab!="Redistribution", ] ### Eliminating Redistribution, not used in the paper



ggplot(tgc, aes(x = perform_high, y = percevaded*100, colour= country)) + 
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("Percent Evaded") + xlab("") +
  geom_point(position = position_dodge(width=0.3))  + facet_wrap( ~ treatment2_lab ) +
  scale_color_brewer(palette="Set1", guide = guide_legend(title = "")) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1))

ggsave(paste0("perevaded_performance_country_", v, ".pdf"), path= fig.path, width = 7, height = 7)




###############
#### Die Toss
###############

# Die Toss UK

plot.data <- NULL
for(i in 0:1){
  tmp <- data.frame(prop.table(table(nof.uk$realdie[which(nof.uk$perform_high == i)])))
  tmp$perfomance <- i
  plot.data <- rbind(plot.data, tmp)
}
plot.data$perfomance[plot.data$perfomance==1]<-"High Performance"
plot.data$perfomance[plot.data$perfomance==0]<-"Low Performance"

d.uk<-ggplot(plot.data, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill="#1AA515") + 
  facet_wrap(~perfomance) + ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.80)) +
  xlab("Reported Die Value - UK") + geom_hline(yintercept = 1/6, lty="dashed")
d.uk
#ggsave(paste0("die_result_UK_", v, ".pdf"), path=fig.path,  width = 7, height = 4)


# Die Toss Chile

plot.data <- NULL
for(i in 0:1){
  tmp <- data.frame(prop.table(table(nof.cl$realdie[which(nof.cl$perform_high == i)])))
  tmp$perfomance <- i
  plot.data <- rbind(plot.data, tmp)
}
plot.data$perfomance[plot.data$perfomance==1]<-"High Performance"
plot.data$perfomance[plot.data$perfomance==0]<-"Low Performance"

d.cl<-ggplot(plot.data, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill="#F23E08") + 
  facet_wrap(~perfomance) + ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.80)) +
  xlab("Reported Die Value - Chile") +scale_x_discrete(limits=c(1,2, 3, 4, 5, 6)) + geom_hline(yintercept = 1/6, lty="dashed")

d.cl

#ggsave(paste0("die_result_UK_", v, ".pdf"), path=fig.path,  width = 7, height = 4)


# Die Toss Russia

plot.data <- NULL
for(i in 0:1){
  tmp <- data.frame(prop.table(table(ru$realdie[which(ru$perform_high == i)])))
  tmp$perfomance <- i
  plot.data <- rbind(plot.data, tmp)
}
plot.data$perfomance[plot.data$perfomance==1]<-"High Performance"
plot.data$perfomance[plot.data$perfomance==0]<-"Low Performance"

d.ru<-ggplot(plot.data, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill= "#0833F2") + 
  facet_wrap(~perfomance) + ylab("Percent") + scale_y_continuous(labels = scales::percent, limits = c(0,0.80)) +
  xlab("Reported Die Value - Russia") +scale_x_discrete(limits=c(1,2, 3, 4, 5, 6)) + geom_hline(yintercept = 1/6, lty="dashed")


d.ru


### Combine plots


d.all<-grid.arrange(d.uk, d.cl, d.ru, nrow=3)
d.all
rm(d.uk, d.cl, d.ru)
ggsave(paste0("comparative_die",v, ".pdf"), d.all, path=fig.path, width = 5, height = 6)



#######################################
#### Descriptive Comparative Statistics
#######################################

##############################
#### Percent evaded by country
##############################


library(Rmisc)
plot.data<-dat.2[!is.na(dat.2$treatment2_lab),]
tgc <- summarySE(plot.data, measurevar="percevaded", groupvars=c("country"), na.rm=T)
tgc$treatment_lab<-"All Treatments"

tgc2 <- summarySE(plot.data, measurevar="percevaded", groupvars=c("treatment_lab", "country"), na.rm=T)

tgc<-rbind(tgc, tgc2)
rm(tgc2)
tgc<-tgc[tgc$treatment_lab!="Redistribution", ] ### Eliminating Redistribution, not used in the paper



ggplot(tgc, aes(x = country, y = percevaded*100, fill= country)) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.3)+
  scale_fill_manual("",values=c( "#F23E08" , "#0833F2" ,"#1AA515"), guide = FALSE) + 
  ylim(0,100) + ylab("Percent Evaded") + xlab("") +
  #geom_point(position = position_dodge(width=0.3))  
  facet_wrap( ~ treatment_lab ) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1))

ggsave(paste0("perevaded_country_", v, ".pdf"), path= fig.path, width = 7, height = 7)


#################
#### Gender plot
#################

table(dat.2$gender)
dat.2$gender[dat.2$gender==1]<-"Male"
dat.2$gender[dat.2$gender==0]<-"Female"

plot.df<-dat.2[,c("gender", "country")]
tbl<-prop.table(table(plot.df),2)
plot.df<-as.data.frame(tbl)


ggplot(plot.df, aes(x = gender, y = Freq*100, fill=country))+
  geom_bar(position="dodge",stat="identity") + xlab("Gender") + 
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  ylim(0, 100) + ylab("Percent")

ggsave(paste0("comparative_gender", v, ".pdf"), path=fig.path,  width = 7, height = 4)


#######################
#### Gender comparison
#######################

plot.dat<-dat.2[complete.cases(dat.2$gender), ]


cdata <- ddply(plot.dat, c("gender", "country"), summarise,
               N.Gender    =  length(unique(subj_id)),
               Mean.cheat = mean(cheat),
               mean.percevaded = mean(percevaded, na.rm=T)
               
)

cdata$treatment_lab<-"All treatments"

cdata2 <- ddply(plot.dat, c("treatment_lab", "gender", "country"), summarise,
                N.Gender    =  length(unique(subj_id)),
                Mean.cheat = mean(cheat),
                mean.percevaded = mean(percevaded, na.rm=T)
                
)

cdata<-rbind(cdata, cdata2)
#cdata<-cdata[cdata$treatment_lab!="Redistribution",]


ggplot(cdata, aes(x = treatment_lab, y = mean.percevaded*100, fill=gender)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  scale_fill_manual("", values = c("red", "blue")) +
  ylab("Percent Evadad") + xlab("") + ylim(0, 100)


ggsave(paste0("cheat_gender", v, ".pdf"), path=fig.path,  width = 7, height = 5)




###########################
### Mean amount given in DG
###########################

plot.df<-subset(dat.2, proposerdg==1)
plot.df <- summarySE(plot.df, measurevar="offerdg", groupvars=c("country"), na.rm=T)


ggplot(plot.df, aes(x = country, y = offerdg, fill=country)) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  geom_errorbar(aes(ymin=(offerdg-ci), ymax=(offerdg+ci)), width=.3)+
  scale_fill_manual("",values=c( "#F23E08" , "#0833F2" ,"#1AA515"), guide = FALSE) + 
  ylab("Offer in Dictator Game") + ylim(0,1000) + xlab("")
ggsave(paste0("comparative_offer_DG", v, ".pdf"), path=fig.path,  width = 7, height = 4)

##########################
### Hist amount given DG
##########################

plot.df<-dat.2[dat.2$proposerdg==1, c("offerdg", "country")]
country.dat<-table(plot.df$country)
country.dat

plot.df<-as.data.frame(table(plot.df))
plot.df$pc[plot.df$country=="Chile"]<-plot.df$Freq[plot.df$country=="Chile"]/country.dat[1]*100
plot.df$pc[plot.df$country=="Russia"]<-plot.df$Freq[plot.df$country=="Russia"]/country.dat[2]*100
plot.df$pc[plot.df$country=="UK"]<-plot.df$Freq[plot.df$country=="UK"]/country.dat[3]*100


ggplot(plot.df, aes(x = offerdg, y = pc, fill=country))+
  geom_bar(position="dodge",stat="identity") + xlab("Offers in Dictator Game") + 
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  ylim(0, 100) + ylab("Percent")
ggsave(paste0("comparative_hist_offers_DG", v, ".pdf"), path=fig.path,  width = 7, height = 4)


######################
### Density age
######################

ggplot(dat.2, aes(x=age_subject)) + geom_density(aes(group=country, fill=country) , alpha=0.5)+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  xlab("Age") + ylab("Density")
ggsave(paste0("comparative_density_age", v, ".pdf"), path=fig.path,  width = 7, height = 4)


###################
#### safe choices
###################


plot.df<-dat.2[,c("safechoices", "country")]
country.dat<-table(plot.df$country)
country.dat

plot.df<-as.data.frame(table(plot.df))
plot.df$pc[plot.df$country=="Chile"]<-plot.df$Freq[plot.df$country=="Chile"]/country.dat[1]*100
plot.df$pc[plot.df$country=="Russia"]<-plot.df$Freq[plot.df$country=="Russia"]/country.dat[2]*100
plot.df$pc[plot.df$country=="UK"]<-plot.df$Freq[plot.df$country=="UK"]/country.dat[3]*100


ggplot(plot.df, aes(x = safechoices, y = pc, fill=country))+
  geom_bar(position="dodge",stat="identity") + xlab("Number of Safe Choices") + 
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  ylim(0, 100) + ylab("Percent")
ggsave(paste0("comparative_safechoices", v, ".pdf"), path=fig.path,  width = 7, height = 4)


###########################
### N correct Sums
###########################

plot.df<-dat.2
plot.df <- summarySE(plot.df, measurevar="ncorrectret", groupvars=c("country"), na.rm=T)


ggplot(plot.df, aes(x = country, y = ncorrectret, fill=country)) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  geom_errorbar(aes(ymin=(ncorrectret-ci), ymax=(ncorrectret+ci)), width=.3)+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515"), guide = FALSE) + 
  ylab("Mean Correct Sums") + xlab("")
ggsave(paste0("comparative_performance", v, ".pdf"), path=fig.path,  width = 7, height = 4)

###########################
### Denisty N correct Sums
###########################

ggplot(dat.2, aes(x=ncorrectret)) + geom_density(aes(group=country, fill=country) , alpha=0.5)+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  xlab("Number Correct Sums") + ylab("Density") 
ggsave(paste0("comparative_performance_density_overlap", v, ".pdf"), path=fig.path,  width = 7, height = 4)

ggplot(dat.2, aes(x=ncorrectret)) + geom_density(aes(group=country, fill=country))+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  xlab("Number Correct Sums") + ylab("Density") + 
  facet_grid( ~ country)
ggsave(paste0("comparative_performance_density", v, ".pdf"), path=fig.path,  width = 7, height = 4)



################################################################################################################################
#### Alternative Types of Cheaters - Always declare 100%,  Always declare 0% sometimes, Always Cheat, Sometimes Cheat
#################################################################################################################################

##### UK

cdata <- ddply(uk, c("subj_id", "treatment_lab" ), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$treatment_lab, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "treatment_lab"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"


plot.data<-plot.data[plot.data$treatment_lab!="Redistribution",]

cdata <- ddply(uk, c("subj_id"), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$cheat_pattern))
plot.data2<-as.data.frame(plot.data2)
plot.data2$treatment_lab<-"Total"
names(plot.data2)[names(plot.data2) == "Var1"] <- "cheat_type"

plot.data<-rbind(plot.data, plot.data2)

#levels(plot.data$cheat_type) <- c("Always cheat 100%", "Always Cheat", "Sometimes Cheat"  , "Never Cheat" )

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))


uk.p<-ggplot(plot.data, aes(x = treatment_lab, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c("Red","Blue", "Yellow", "Green3"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10)) + 
  ggtitle("UK")



##### Russia

cdata <- ddply(ru, c("subj_id", "treatment_lab" ), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$treatment_lab, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "treatment_lab"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"


plot.data<-plot.data[plot.data$treatment_lab!="Redistribution",]

cdata <- ddply(ru, c("subj_id"), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$cheat_pattern))
plot.data2<-as.data.frame(plot.data2)
plot.data2$treatment_lab<-"Total"
names(plot.data2)[names(plot.data2) == "Var1"] <- "cheat_type"

plot.data<-rbind(plot.data, plot.data2)

#levels(plot.data$cheat_type) <- c("Always cheat 100%", "Always Cheat", "Sometimes Cheat"  , "Never Cheat" )

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))


ru.p<-ggplot(plot.data, aes(x = treatment_lab, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c("Red","Blue", "Yellow", "Green3"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10)) + 
  ggtitle("Russia")





##### Chile

cdata <- ddply(cl, c("subj_id", "treatment_lab" ), summarise,
               mean.percevaded = mean(percevaded, na.rm=T), ## two observations with NA percevaded because of zero on ncorrectret: View(subset(cl, is.na(percevaded)))
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$treatment_lab, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "treatment_lab"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"


plot.data<-plot.data[plot.data$treatment_lab!="Redistribution",]

cdata <- ddply(cl, c("subj_id"), summarise,
               mean.percevaded = mean(percevaded, na.rm=T),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$cheat_pattern))
plot.data2<-as.data.frame(plot.data2)
plot.data2$treatment_lab<-"Total"
names(plot.data2)[names(plot.data2) == "Var1"] <- "cheat_type"

plot.data<-rbind(plot.data, plot.data2)

#levels(plot.data$cheat_type) <- c("Always cheat 100%", "Always Cheat", "Sometimes Cheat"  , "Never Cheat" )

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))


cl.p<-ggplot(plot.data, aes(x = treatment_lab, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c("Red","Blue", "Yellow", "Green3"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10)) + 
  ggtitle("Chile")

cl.p


d.all<-grid.arrange(uk.p,  ru.p, cl.p, nrow=3)
d.all

rm(uk.p,  ru.p, cl.p)

ggsave(paste0("comparative_4types_cheaters", v, ".pdf"), d.all,  path=fig.path,  width = 8, height = 10)







############################
#### Safe choices comparisons
############################

tgc <- summarySE(dat.2, measurevar="percevaded", groupvars=c("safechoices", "country"), na.rm=T)

ggplot(tgc, aes(x = country, y = percevaded*100, fill= country)) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.3)+
  scale_fill_manual("",values=c( "#F23E08" , "#0833F2" ,"#1AA515"), guide = FALSE) + 
  ylim(0,100) + ylab("Percent Evaded") + xlab("") +
  #geom_point(position = position_dodge(width=0.3))  
  facet_wrap( ~ safechoices ) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1)) +
  ggtitle("Percent evaded by number of safe choices")

ggsave(paste0("perevaded_country_safechoices", v, ".pdf"), path= fig.path, width = 7, height = 7)





#######

####################???
### Otros gráficos
####################

#---------------
#-- Densidad --
#---------------

# Density for specific distribution
par(mfrow = c(1, 1))  
x <- seq(-4, 4, length = 100)
y1 <- dnorm(x)
plot(x, y1, type = "l", lwd = 2, col = "blue")
y2 <- dnorm(x, m = 0, sd = 2)
lines(x, y2, type = "l", lwd = 2, col = "red")
