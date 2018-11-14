######################################################
### O que fazer e o que n√£o fazer com a regress√£o? ###
###             Pol√≠tica Hoje (2011)							 ###
######################################################
###         Figueiredo, Nunes, Baptista,           ###
###        Alexandre Junior, Rocha e Santos        ###
######################################################

library(fields)
library(car)

banco <- read.csv("C:/Users/test/Documents/GitHub/analisedados/lista6/simula.correto.csv", header= T )
banco2 <- read.csv("C:/Users/test/Documents/GitHub/analisedados/lista6/simula.errado.csv", header=T)
attach(banco)
?attach

# As duas linhas abaixo produzem uma tabela descritiva com as principais medidas 
# de tendencia central e de dispers„o das vari·veis produzidas.
variav.descrit <- data.frame(y, x1, x2, x3)
t(stats(variav.descrit))

# Box-plot
?par

par(mfrow = c(1,1))
boxplot(y, x1, x2, names = c('Y','X','X2'), ylim = c(-20,20))

# Histogramas e Box-plots das distribuiÁıes de y, x1, x2

par(mfrow = c(1,3))
hist(y, main = "Histograma de Y")
hist(x1, main = "Histograma de X1")
hist(x2, main ="Histograma de X2")

# modelo linear usando o mÈtodo de mÌnimos quadrados ordin·rios. 
 
modelo.1 <- lm(y ~ x1)
modelo.2 <- lm(y ~ x1 + x2)
modelo.3 <- lm(y ~ x2 + x3)
modelo.4 <- lm(y ~ x1 + x3)
modelo.f <- lm(y ~ x1 + x2 + x3)

# Tabela da ANOVA
tab.ANOVA <- function(modelo, gln, gld) {
	
	# modelo - modelo de regressao testado
	# gln - graus de liberdade do numerador
	# gld - graus de liberdade do denominador
	
	SSreg <- sum((y-mean(y))^2)
 	print(paste('SSreg:'))
  print(SSreg)
	RSS <- sum(modelo$res^2)
 	print(paste('RSS:'))
  print(RSS)
	F.teste <- ((SSreg-RSS)/(gln)/(RSS/gld))
 	print(paste('F:'))
  print(F.teste)
	print(paste('P-valor:'))
  1-pf(F.teste, 3, 196)

}

tab.ANOVA(modelo.f, 3, 196)

# Tabela com os resultados da regress„o
summary(modelo.1)
summary(modelo.2)
summary(modelo.3)
summary(modelo.4)
summary(modelo.f)

# Gr·fico para a representaÁao dos intervalos de confianÁa dos coeficientes estimados
par(mfrow = c(1,1))
betas <- coefficients(modelo.f) 
IC <-  confint(modelo.f, level = 0.95) 

y.axis <- seq(from = 1, to = length(betas))
plot(betas, y.axis, type = "p", pch = 19, xlab = "Magnitude dos Coeficientes", 
     ylab ="", axes = F, xlim = c(min(IC - .4), max(IC + .4)), 
     ylim = c(min(y.axis-.2), max(y.axis+.2)), cex = 1,yaxs = "i",xaxs = "i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at = seq(round(min(IC - .9)), round(max(IC + .9)), by = 0.1), 
     labels = seq(round(min(IC - .9)), round(max(IC + .9)), by = 0.1), tick=T, cex.axis = 1, mgp = c(2,.7,0))
axis(2, at = y.axis, label = names(betas), las = 1, tick = T, line = -.5, cex.axis = 1, mgp = c(2,.7,0))
abline(v=0, lty=2, col="red")


# Gr·fico para interpretar os coeficientes 
novo.x <- data.frame(x1 = seq(min(x1), max(x1), length.out = dim(banco)[1]), x2 = mean(x2), x3 = mean(x3))
novo.x2 <- data.frame(x1 = mean(x1), x2 = seq(min(x2), max(x2), length.out = dim(banco)[1]),  x3 = mean(x3))
novo.x3 <- data.frame(x1 = mean(x1), x2 = mean(x2), x3 = seq(min(x3), max(x3), length.out = dim(banco)[1]))

par(mfrow = c(1,3))

y.predito <- predict(modelo.f, novo.x, interval = "confidence")
matplot(novo.x$x1, y.predito, lty = c(1,2,2), col = c('black', 'red', 'red'), 
        type = "l", ylab = "Valor predito de Y", xlab = "X")
abline(h=0, col='gray', lty=3)
  
y.predito <- predict(modelo.f, novo.x2, interval = "confidence")
matplot(novo.x2$x2, y.predito, lty = c(1,2,2), col = c('black', 'red', 'red'), type = "l", ylab = "Valor predito de Y", xlab = "X2")
abline(h=0, col = 'gray', lty = 3)
	
y.predito <- predict(modelo.f, novo.x3, interval = "confidence")
matplot(novo.x3$x3, y.predito, lty = c(1,2,2), col=c('black', 'red', 'red'), type = "l", ylab = "Valor predito de Y", xlab = "X3")
abline(h = 0, col = 'gray', lty = 3)

detach(banco)

########################################################
###                   DiagnÛstico                    ###
########################################################

modelo1 <- lm(y ~ x1 + x2 + x3, data = banco)
modelo2 <- lm(y ~ x1 + x2 + x3, data = banco2)


par(mfrow=c(1,2))

### Avaliando o peso de outliers ###
# Bonferonni p-valor para casos extremos
outlierTest(modelo1) 
outlierTest(modelo2)

# qq plot para resÌduos padronizados em t
qqPlot(modelo1, main = "QQ Plot 1") 
qqPlot(modelo2, main = "QQ Plot 2")
 
### Avaliando ObservaÁıes Influentes ###
# Gr·ficos Parciais (Partil Plots)
avPlots(modelo1)
avPlots(modelo2)

# Cook's D plot
# IdentificaÁ„o de valores D > 4/(n-k-1) 
limite <- 4/((length(banco$x) - length(modelo1$coefficients) - 1)) 
plot(modelo1, which = 4, cook.levels = limite)
limite <- 4/((length(banco2$x) - length(modelo2$coefficients) - 1)) 
plot(modelo2, which = 4, cook.levels = limite)

# Gr·fico de InfluÍncia 
influencePlot(modelo1, id.method = "identify", main = "Gr·fico de InfluÍncia 1", ylim = c(-6, 6), xlim = c(0, 0.25))
influencePlot(modelo2, id.method = "identify", main="Gr·fico de InfluÍncia 2", ylim = c(-6, 6 ), xlim = c(0, 0.25))

### Checando a Normalidade dos ResÌduos ###
# distribution of studentized residuals
sresid <- studres(modelo1) 
hist(sresid, freq = FALSE, ylim = c(0, 0.4), main = "DistribuiÁ„o dos ResÌduos Padronizados 1")
xfit <- seq(min(sresid), max(sresid), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

?studres

sresid <- studres(modelo2) 
hist(sresid, freq = FALSE, main = "DistribuiÁ„o dos ResÌduos Padronizados 2")
xfit <- seq(min(sresid),max(sresid),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

### Avaliando homoscedasticidade ###
# Teste para vari‚ncia do erro n„o constante
ncvTest(modelo1)
ncvTest(modelo2)

# plot resÌduos padronizados vs. valores preditos
spreadLevelPlot(modelo1)
spreadLevelPlot(modelo2)

### Avaliando N„o-linearidade ##
# component + residual plot 
crPlots(modelo1)
crPlots(modelo2)

### Teste para Colinearidade ###
vif(modelo1) # variance inflation factors 
sqrt(vif(modelo1)) > 2 # problem?

par(mfrow = c(1,2))
plot(banco$x1, banco$x2, xlab = 'x1', ylab = 'x2') # plot de correlaÁ„o
plot(banco2$x2, banco2$x5, xlab = 'x1', ylab = 'x5')


### Teste para autocorrelaÁ„o dos erros ###
durbinWatsonTest(modelo1)
 