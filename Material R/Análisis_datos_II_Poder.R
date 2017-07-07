# -----------------------------------------------
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS Santiago
# -----------------------------------------------

# -------------------------------------------
# -------- Módulo Análisis de Poder ---------
# -------------------------------------------


### playing with power function
# power.prop.test: for binary outcome
# power.t.test: for continuous outcome
library(ggplot2)
library(scales)


# power analysis needs to specify 4 elements
# 1: significance level
# 2: statistical power
# 3: effect size
# 4: sd of data (for t-test) or mean of the control group (for prop test)

##--------#---------#---------#---------#---------#---------#---------#---------
## Binary outcome example
##--------#---------#---------#---------#---------#---------#---------#---------
base.turnout <- 0.45
effect.size <- 0.02
power.prop.test(p1=base.turnout,p2=base.turnout+effect.size,
                sig.level=0.05,power=0.8)



##--------#---------#---------#---------#---------#---------#---------#---------
## similar analysis using power.t.test
##--------#---------#---------#---------#---------#---------#---------#---------
sd.sample <- sqrt(base.turnout*(1-base.turnout)) # Variance of Bernoulli distribuiton: p*(1-p)
power.t.test(delta=effect.size, sd = sd.sample, sig.level = 0.05, power = 0.8)


##--------#---------#---------#---------#---------#---------#---------#---------
## Draw power curve
##--------#---------#---------#---------#---------#---------#---------#---------
# the idea is to calculate necessary sample sizes for different effect sizes
# we don't know the true level of effect sizes, so we need to know
# what is the necessary to get sample sizes for both 
# an optimistic senario (with large effect size) and 
# a pessimistic senario (with small effect size).

effects <- seq(0.005, 0.05, by = 0.001)
m <- length(effects)
n <- rep(NA, m)
for (i in 1:m) {
	n[i] <- power.prop.test(p1 = base.turnout, 
	p2 = base.turnout + effects[i],
	power = 0.8, sig.level=0.05, alternative="one.sided")$n
}

plot(effects, n, log = "y", xlab="Hypothesized Effect Size", ylab="Sample size",  yaxt = "n", type="l", lwd=4)
axis(2, labels=FALSE,las=1)
namelist <- c("1000","2000","5000","10000","20000","50000","100000")
mtext(namelist, side=2, at=as.numeric(namelist),las=1,adj=1,line=1)


## Making fancier graphics
## ggplot example
plot.data <- data.frame(effects, n)
ggplot(plot.data, aes(x = effects, y = n)) + geom_line() + 
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000), 
                limit = c(1000, 150000),
                labels = comma) + 
  xlab("Hypothesized Effect Size") + ylab("Sample Size")
ggsave("power_plot/power_output.pdf", width = 5, height = 4)

##--------#---------#---------#---------#---------#---------#---------#---------
## Try different powers
##--------#---------#---------#---------#---------#---------#---------#---------
effects <- seq(0.005, 0.05, by = 0.001)
sig <- 0.05
powers <- seq(from = 0.5, to = 0.9, by = 0.1)

par(mfrow=c(2,3))
for(p in powers){
  m <- length(effects)
  n <- rep(NA, m)
  for (i in 1:m) n[i] <- power.prop.test(p1 = base.turnout, p2 = base.turnout + effects[i], 
                                         power = p, sig.level=sig, alternative="one.sided")$n
  plot(effects, n, log = "y", xlab="Hypothesized Effect Size", ylab="Sample size",
       main=paste("Power: ", p),  yaxt = "n", type="l", lwd=4, ylim=c(500,300000))
  axis(2, labels=FALSE,las=1)
  namelist <- c("1000","2000","5000","10000","20000","50000","100000","200000")
  mtext(namelist, side=2, at=as.numeric(namelist),las=1,adj=1,line=1,cex=.5)
}


## ggplot
dev.off()
plot.data <- NULL
for(p in powers){
  m <- length(effects)
  n <- rep(NA, m)
  for (i in 1:m) n[i] <- power.prop.test(p1 = base.turnout, p2 = base.turnout + effects[i], 
                                         power = p, sig.level=sig, alternative="one.sided")$n
  temp.data <- data.frame(effects, n, power = sprintf("Power = %s", p))
  plot.data <- rbind(plot.data, temp.data)
}
ggplot(plot.data, aes(x = effects, y = n)) + geom_line() + facet_wrap(~power) +
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000), 
                limit = c(500, 300000),
                labels = comma) + 
  xlab("Hypothesized Effect Size") + ylab("Sample Size")
ggsave("power_plot/multiple_siglevels.pdf", width = 7, height = 6)
