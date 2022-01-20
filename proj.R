#Importando as bibliotecas
library(WDI)
library(tsDyn)
library(MSwM)
library(urca)
library(forecast)
library(seasonal)

#Obtendo os dados 
devtools::install_github('wilsonfreitas/rbcb')

test <- rbcb::get_series(c(PIB = 4380))
test1 <- rbcb::get_series(c(CAMBIO = 11752))

test1 <- test1[-c(1:24),]

#Transformando os dados para série temporal
PIB <- test$PIB
PIB <- ts(PIB,start=c(1990,1),end=c(2021,10),frequency=12)
CAMBIO <- test1$CAMBIO
CAMBIO <- ts(CAMBIO,start=c(1990,1),end=c(2021,10),frequency=12)

#Retirando a sazonalidade do pib
Q <- ordered(cycle(PIB))
pib.reg <- lm(PIB~Q)
summary(pib.reg)
pib.des <- ts(resid(pib.reg),
              start = c(1990,1), freq=12)

par(mfrow=c(1,1))
plot(PIB,main='',xlab='Ano', ylab='',col='blue',bty='l')
par(new=TRUE)
plot(pib.des,col='red',lty=2)
legend('topleft',axes=F, ann=F,c('PIB', 'PIB dessazonalizado'),col=c('blue', 'red'), lty=1:2,bty='n')
grid(col='darkgrey')

#Retirando a tendência do pib
filtrohp <- function(y, lambda){
  id <- diag(length(y))
  d <- diff(id, d=2)
  tendhp <- solve(id+lambda*crossprod(d), y)
  tendhp
}

lambda <- 14400
tendencia <- filtrohp(pib.des, lambda)
hppib <- pib.des - tendencia

#testando a estacionaridade do pib
ur.kpss(hppib)

#Retirando a sazonalidade do Cambio
Q2 <- ordered(cycle(CAMBIO))
cambio.reg <- lm(CAMBIO~Q2)
summary(cambio.reg)
cambio.des <- ts(resid(cambio.reg),
              start = c(1990,1), freq=12)

par(mfrow=c(1,1))
plot(CAMBIO,main='',xlab='Ano', ylab='',col='blue',bty='l')
par(new=TRUE)
plot(cambio.des,col='red',lty=2)
legend('topleft',axes=F, ann=F,c('CAMBIO', 'CAMBIO dessazonalizado'),col=c('blue', 'red'), lty=1:2,bty='n')
grid(col='darkgrey')

#Retirando a tendência do cambio
tendencia2 <- filtrohp(cambio.des,lambda)
hpcambio <- cambio.des - tendencia2

#Testando a estacionaridade do cambio
ur.kpss(hpcambio)

#Modelo linear do pib com o cambio
md <- lm(hppib ~ hpcambio)

summary(md)

#Modelo Markow Switching
markov = msmFit(md, k = 2, sw = rep(TRUE, 3))

summary(markov)

par(mar=c(3,3,3,3))
plotProb(markov, which=1)

plotProb(markov, which=2)

#Diagnóstico para Markov switching
par(mar=c(3,3,3,3))
plotDiag(markov, regime=1, which=1)

plotDiag(markov, regime=1, which=2)

plotDiag(markov, regime=1, which=3)

