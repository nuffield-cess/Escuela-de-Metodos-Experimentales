# -----------------------------------------------
# Análisis de datos usando R
# Profesora: Denise Laroze
# Escuela de Métodos Experimentales CESS Santiago
# -----------------------------------------------

# -------------------------------------------
# -------- Módulo Análisis de Poder ---------
# -------------------------------------------


### Jugando con la Función de Poder
# power.prop.test: para resultados binarios
# power.t.test: para resultados continuos 
library(ggplot2)
library(scales)


# Para realizar análisis de poder se requiere especificar 4 elementos
# 1: nivel de significancia
# 2: poder estadístico
# 3: tamaño del efecto
# 4: desviación estándar (para t-test) o media del grupo de control (para prop test)

##--------#---------#---------#---------#---------#---------#---------#---------
## Ejemplo resultado binario
##--------#---------#---------#---------#---------#---------#---------#---------
base.turnout <- 0.45
effect.size <- 0.02
power.prop.test(p1=base.turnout,p2=base.turnout+effect.size,
                sig.level=0.05,power=0.8)



##--------#---------#---------#---------#---------#---------#---------#---------
## similar usando power.t.test
##--------#---------#---------#---------#---------#---------#---------#---------
sd.sample <- sqrt(base.turnout*(1-base.turnout)) # Varianza de la distribución de Bernoulli: p*(1-p)
power.t.test(delta=effect.size, sd = sd.sample, sig.level = 0.05, power = 0.8)


##--------#---------#---------#---------#---------#---------#---------#---------
## Dibujar la curva de poder
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

plot(effects, n, log = "y", xlab="Tamaño hipotético del efecto", ylab="Tamaño de la muestra",  yaxt = "n", type="l", lwd=4)
axis(2, labels=FALSE,las=1)
namelist <- c("1000","2000","5000","10000","20000","50000","100000")
mtext(namelist, side=2, at=as.numeric(namelist),las=1,adj=1,line=1)


## Mejores gráficos
## ggplot
plot.data <- data.frame(effects, n)
ggplot(plot.data, aes(x = effects, y = n)) + geom_line() + 
  scale_y_log10(breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000), 
                limit = c(1000, 150000),
                labels = comma) + 
  xlab("Tamaño hipotético del efecto") + ylab("Tamaño de la muestra")
#ggsave("power_output.pdf", width = 5, height = 4)

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
  plot(effects, n, log = "y", xlab="Tamaño hipotético del efecto", ylab="Tamaño de la muestra",
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
  xlab("Tamaño hipotético del efecto") + ylab("Tamaño de la muestra")
ggsave("power_plot/multiple_siglevels.pdf", width = 7, height = 6)




#---------------------------------------
# Calculos de poder mediante simulación
#---------------------------------------



# Simular datos, para ello se deben seleccionar el tamaño del efecto (delta) y una distribución de residuos

set.seed(5963)

#Función de poder utilizando un error distribuido uniformemente [-4,4]
power<-function (rep, esize, N) {
  pv <- rep(NA, rep)
  for (i in 1:rep){
    mydata <- data.frame(samegroup=rep(c(T,F), each=N/2))
    mydata$given <- 8 + mydata$samegroup * esize + sample(-4:4, N, replace=T)  
    p<- wilcox.test(given ~ samegroup, data=mydata, conf.int=T)
    pv[i]<-p$p.value
    power<-sum(pv < 0.05)/length(pv)
  }
  return(power)
  
}

# Resultado con un N fijo
power(rep=100, esize=1.5, N=30)

#Loop sobre la función variando N
N<-seq(10, 200, 2)
M <- length(N)
N.power<- rep(NA, M)

for (i in 1:M){
  N.power[i] <- power(rep=100, esize=1.5, N=N[i])
}


#graficar datos
plot(N.power,N/2, main="Power calculations", ylab="Number of obs in each group", xlab="Power")


#Función de poder utilizando un error distribuido normalmente
power.norm<-function (rep, esize, N, var) {
  pv <- rep(NA, rep)
  for (i in 1:rep){
    mydata <- data.frame(samegroup=rep(c(T,F), each=N/2))
    mydata$given <- 5.5 + mydata$samegroup * esize +rnorm(N, 0, var) 
    p<- wilcox.test(given ~ samegroup, data=mydata, conf.int=T)
    pv[i]<-p$p.value
    power<-sum(pv < 0.05)/length(pv)
  }
  return(power)
}


# Variando sobre N y el tamaño del effecto, con sigma fijo 

M <- length(N)
N.power.norm<- rep(NA, M)

N<-seq(10, 100, 2)
effects <- seq(-0.7, -1.7, by = -0.2)

m <- c(length(N), length(effects))
ne.power <- matrix(NA, m[1], m[2])
for (i in 1:(m[1])) {
  for (j in 1:(m[2])) {
    ne.power[i, j] <- power.norm(rep=500, esize=effects[j], N=N[i], var=2)
    
  }
}





# Plots of Power calculations

##  Open a new default device.

get( getOption( "device" ) )()

##  Graficar todas las simunaciones en un mismo gráfico (2 filas 3 columnas) 

par( mfrow = c( 3, 2 ) )
plot( ne.power[,1], N/2, col = "red", main = "Effect=-0.7, sigma=2", xlim=c(0,1), ylab="Núm obs", xlab="")
plot( ne.power[,2], N/2, col = "blue", main = "Effect=-0.9", xlim=c(0,1), ylab="", xlab="")
plot( ne.power[,3], N/2, col = "green", main = "Effect=-1.1", xlim=c(0,1), ylab="Núm obs", xlab="")
plot( ne.power[,4], N/2, col = "purple", main = "Effect=-1.3", xlim=c(0,1), ylab="", xlab="")
plot( ne.power[,5], N/2, col = "springgreen4", main = "Effect=-1.5", xlim=c(0,1), ylab="Núm obs", xlab="power")
plot( ne.power[,6], N/2, col = "grey30", main = "Effect=-1.7", xlim=c(0,1), ylab="", xlab="power")








