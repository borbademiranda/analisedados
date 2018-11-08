######################################################
### O que fazer e o que não fazer com a regressão? ###
###             Política Hoje (2011)							 ###
######################################################
###         Figueiredo, Nunes, Baptista,           ###
###        Alexandre Junior, Rocha e Santos        ###
######################################################
###                   Simulações                   ###
######################################################

# Há grande chance de que os resultados encontrados no texto sejam diferentes dos 
# resultados produzidos quando estas linhas de comando forem replicadas. 
# O motivo pelo qual isso se dá está inteiramente associado a forma como os 
# dados foram produzidos. Como explicado no texto, cada variável é construída 
# a partir de uma amostra aleatória com distribuição normal padronizada. 
# Portanto, cada vez que um novo banco de dados é produzido, uma amostra diferente é construída.
# Para replicar os resultados, consulte o arquivo 'cod.replica' e faça o download 
# dos bancos 'simula.correto' e 'simula.errado'

########################################################
###               Pacotes Necessários                ###
########################################################
install.packages("fields")
install.packages("car")

library(fields)
library(car)

########################################################
###            Criando os Dados Corretos             ###
########################################################

dados.c <- function(alfa=1, beta1=3, beta2=2, beta3=1, rho=0, rho2=0, n=30, sigma=10) {
  
  # alfa - valor verdadeiro para alfa
  # beta1 - valor verdadeiro para o beta
  # beta2 - valor verdadeiro para o beta2
	# beta3 - valor verdadeiro para o beta2
  # rho - correlacao entre x e x2 e x3
  # n - tamanho da amostra
  # sigma - desvio padrao da distribuicao do erro
  
  x1 <<- rnorm(n,0,1) # variável contínua
  x2 <<- x1*rho + rnorm(n,0,1)*sqrt(1-rho^2) # variável contínua associada a X
	x3 <<- x1*rho2 + rnorm(n,0,1)*sqrt(1-rho2^2)
	x3 <<- ifelse(x3>mean(x3),1,0) # variável dicotomica
  e <<- rnorm(n,0,1)
  y <<- alfa + beta1*x1 + beta2*x2 - beta3*x3 + sigma*e

}


########################################################
###        Criando os Dados com Limitações           ###
########################################################

dados.e <- function(alfa=1, beta1=3, beta2=2, beta3=1, rho=0, rho2=0, n=30, sigma=10) {
  
  # alfa - valor verdadeiro para alfa
  # beta1 - valor verdadeiro para o beta
  # beta2 - valor verdadeiro para o beta2
	# beta3 - valor verdadeiro para o beta2
  # rho - correlacao entre x e x2 e x3
  # n - tamanho da amostra
  # sigma - desvio padrao da distribuicao do erro
  
  x1 <<- rbinom(n,size=3,prob=0.3)
  x2 <<- x1*rho + rnorm(n,42,10)*sqrt(1-rho^2)
	x3 <- x1*rho2 + rnorm(n,50,32)*sqrt(1-rho^2)
	x3 <<- ifelse(x3>mean(x3),1,0) # variável dicotomica
  e <<- rgamma(n,shape=1)
  y <<- alfa + beta1*x1^2 + beta2*x2 + beta3*x3 + sigma*e

}


########################################################
#  Função para estudar o modelo MQO nos dados corretos #
########################################################
	
analise.func1 <- function(alfa=1, beta1=2, beta2=3, beta3=3, rho=0, rho2=0, n=300, sigma=3) {

	dados.c(alfa, beta1, beta2, beta3, rho, rho2, n, sigma)

 	variav.descrit <- data.frame(y, x1, x2, x3)
	print(t(stats(variav.descrit)))
	
	modelo1 <<- lm(y ~ x1 + x2 + x3)
	print(summary(modelo1))

	novo.x <- data.frame(x1=seq(min(x1), max(x1), 0.1), x2=mean(x2), x3=mean(x3))
	novo.x2 <- data.frame(x1=mean(x1), x2=seq(min(x2), max(x2), 0.1),  x3=mean(x3))
 	novo.x3 <- data.frame(x1=mean(x1), x2=mean(x2), x3=seq(min(x3), max(x3), 0.1))
	
 	par(mfrow=c(2,2))

	betas <- coefficients(modelo1) # model coefficients
	IC <-  confint(modelo1, level=0.95) # CIs for model parameters 

	y.axis <- seq(from=1, to=length(betas))
	plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), main="Magnitude dos Coeficientes e Intervalos de Confiança \n para Modelo simulado em MQO",cex=1,yaxs="i",xaxs="i")
	segments(IC[,1], y.axis, IC[,2], y.axis)
	axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, mgp=c(2,.7,0))
	axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, mgp=c(2,.7,0))
	abline(v=0, lty=2, col="red")
	
	y.predito.IC <- predict(modelo1, novo.x, interval="confidence")
 	matplot(novo.x$x1, y.predito.IC, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X")
 	abline(h=0, col='gray', lty=3)
  
	y.predito.IC2 <- predict(modelo1, novo.x2, interval="confidence")
 	matplot(novo.x2$x2, y.predito.IC2, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X2")
 	abline(h=0, col='gray', lty=3)
	
	y.predito.IC3 <- predict(modelo1, novo.x3, interval="confidence")
 	matplot(novo.x3$x3, y.predito.IC3, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X3")
 	abline(h=0, col='gray', lty=3)

}



########################################################
#  Função para estudar o modelo MQO nos dados errados  #
########################################################
	
analise.func2 <- function(alfa=1, beta1=2, beta2=3, beta3=3, rho=0, rho2=0, n=300, sigma=3) {

	dados.e(alfa, beta1, beta2, beta3, rho, rho2, n, sigma)

 	variav.descrit <- data.frame(y, x1, x2, x3)
	print(t(stats(variav.descrit)))
	
	modelo1 <<- lm(y ~ x1 + x2 + x3)
	print(summary(modelo1))

	novo.x <- data.frame(x1=seq(min(x1), max(x1), 0.1), x2=mean(x2), x3=mean(x3))
	novo.x2 <- data.frame(x1=mean(x1), x2=seq(min(x2), max(x2), 0.1),  x3=mean(x3))
 	novo.x3 <- data.frame(x1=mean(x1), x2=mean(x2), x3=seq(min(x3), max(x3), 0.1))
	
 	par(mfrow=c(2,2))

	betas <- coefficients(modelo1) # model coefficients
	IC <-  confint(modelo1, level=0.95) # CIs for model parameters 

	y.axis <- seq(from=1, to=length(betas))
	plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), main="Magnitude dos Coeficientes e Intervalos de Confiança \n para Modelo simulado em MQO",cex=1,yaxs="i",xaxs="i")
	segments(IC[,1], y.axis, IC[,2], y.axis)
	axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, mgp=c(2,.7,0))
	axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, mgp=c(2,.7,0))
	abline(v=0, lty=2, col="red")
	
	y.predito.IC <- predict(modelo1, novo.x, interval="confidence")
 	matplot(novo.x$x1, y.predito.IC, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X")
 	abline(h=0, col='gray', lty=3)
  
	y.predito.IC2 <- predict(modelo1, novo.x2, interval="confidence")
 	matplot(novo.x2$x2, y.predito.IC2, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X2")
 	abline(h=0, col='gray', lty=3)
	
	y.predito.IC3 <- predict(modelo1, novo.x3, interval="confidence")
 	matplot(novo.x3$x3, y.predito.IC3, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y", xlab="X3")
 	abline(h=0, col='gray', lty=3)

}

########################################################
###              Resultados e Análise                ###
########################################################

# Como esse código é possível produzir dados semelhentes aos que foram usados no texto. O primeiro produz dados apropriados para a regressão linear de MQO, o segundo não.
analise.func1(alfa=1, beta1=4, beta2=0.5, beta3=2, rho=0.3, rho2=0.25, n=200, sigma=5)

banco <- data.frame(cbind(y,x1,x2,x3))
write.csv(banco, file="banco.csv")

analise.func2(alfa=1, beta1=3, beta2=2, beta3=1, rho=0.95, n=50, sigma=75)



########################################################
###     Explicação da construção do código acima     ###
########################################################
# Começamos criando os dados para nossa análise
dados.c(alfa=1, beta1=2, beta2=3, beta3=1, rho=0, n=300, sigma=3)

# Vamos calcular uma regressão linear para mensurar a correlação entre x, x2 e x3 com y. Usando o pacote 'stats' do R, estimamos tais associações por meio do método de Mínimos Quadrados Ordinários. Observe que os coeficientes alfa, beta1, beta2 e beta3 são muito próximos dos valores verdadeiros estabelecidos na criação do banco de dados. A diferença para o valor real se dá pelo erro incluído na criação dos dados.
modelo1 <- lm(y ~ x1 + x2 + factor(x3))
summary(modelo1)

# Agora vamos começar a análise dos resultados. Primeiramente, é preciso conhecer a distribuição e as variáveis com as quais estamos trabalhando. A tabela gerada abaixo apresenta as principais características de cada variável como mínimo, máximo, média, e desvio-padrão.
variav.descrit <- data.frame(y, x1, x2, x3)
t(stats(variav.descrit))

# Uma parte fundamental ligada à interpretação dos coeficientes obtidos na regressão linear é a representação dos resultados através de gráficos que mostrem a importância relativa de cada resultado. Abaixo nós construímos um gráfico de pontos com a representação da associação entre x e y usando os valores preditos de y e os valores de x observados na amostra em análise.
novo.x <- data.frame(x1=seq(min(x1), max(x1), 0.1), x2=mean(x2), x3=mean(x3))
novo.x2 <- data.frame(x1=mean(x1), x2=seq(min(x2), max(x2), 0.1),  x3=mean(x3)) # sabendo que x possui valores que variam de -3 a 3, nós criamos uma variável ficticia para ser usada na montagem do gráfico. Fazemos o mesmo para x2 e x3.
novo.x3 <- data.frame(x1=mean(x1), x2=mean(x2), x3=seq(min(x3), max(x3), 0.1))


par(mfrow=c(1,3))

# Gráfico para x
y.predito.IC <- predict(modelo1, novo.x, interval="confidence") # cálculo dos valores preditos com o intervalo de confiança para x1
matplot(novo.x$x, y.predito.IC, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y")
abline(h=0, col='gray', lty=3)
  
# Gráfico para x2
y.predito.IC2 <- predict(modelo1, novo.x2, interval="confidence") # cálculo do intervalo de confiança para x2
matplot(novo.x2$x2, y.predito.IC2, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y")
abline(h=0, col='gray', lty=3)
	
# Gráfico para x3
y.predito.IC3 <- predict(modelo1, novo.x3, interval="confidence") # cálculo do intervalo de confiança para x3
matplot(novo.x3$x3, y.predito.IC3, lty=c(1,2,2), col=c('black', 'red', 'red'), type="l", ylab="Valor predito de Y")
abline(h=0, col='gray', lty=3)


# Uma outra forma interessante de apresentar os resultados é mostrando a magnitude dos coeficientes juntamente com os respectivos intervalos de confiança.
par(mfrow=c(1,1))
betas <- coefficients(modelo1) # model coefficients
IC <-  confint(modelo1, level=0.95) # CIs for model parameters 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), main="Magnitude dos Coeficientes e Intervalos de Confiança \n para Modelo simulado em MQO",cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
#abline(h=y.axis, lty = "dotted")

axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")



### Use as funções abaixo para testar diferentes modelos usando MQO e as variáveis Y, x, x2, e x3. Os parametros alfa, beta, beta1, beta2, beta3, sigma, rho e tamanho da amostra (n) podem ser alterados livremente. Abaixo seguem alguns exemplos.
analise.func(alfa=1, beta1=3, beta2=2, beta3=1, rho=0, n=100, sigma=3)

analise.func(alfa=1, beta1=0.3, beta2=2, beta3=2, rho=0, n=100, sigma=3)

analise.func(alfa=1, beta1=2, beta2=2, beta3=2, rho=0, n=10, sigma=3)
	
analise.func(alfa=1, beta1=0, beta2=8, beta3=0, rho=0, n=1000, sigma=3)
	
analise.func(alfa=1, beta1=5, beta2=5, beta3=5, rho=0, n=3000, sigma=5)




########################################################
###                     Parte II                     ###
########################################################
###        Criando os Dados para Simulação           ###
########################################################

# Nessa segunda parte disponibilizaremos algumas funções que podem ser usadas para verificar os pressupostos e as afirmações que são explicadas na seção "Os dez mandamentos do modelo de MQO" do nosso texto. O objetivo aqui é usar simulações de Monte Carlo para provar os pressupostos do modelo linear de MQO.

dados <- function(alfa=1, beta=2, beta2=0, rho=.001, n=30, p=1, sigma=10, siglevel=.05) {
    
  # alfa - valor verdadeiro para alfa
  # beta - valor verdadeiro para o beta
  # beta2 - valor verdadeiro para o beta2
  # rho - correlacao entre x e x2
  # n - tamanho da amostra
  # p - numero de simulacoes produzidas
  # sigma - desvio padrao da distribuicao do erro
  # siglevel - probabilidade para teste de hipótese expresso em decimais

  # Objetos criados para armazenar valores calculados
  lista.alfa.estim <<- numeric(p)
	lista.beta.estim <<- numeric(p)
  lista.beta2.estim <<- numeric(p)
	lista.test.t <<- numeric(p)
  lista.test.t2 <<- numeric(p)
	interv.conf.inferior <<- numeric(p)
  interv.conf.inferior2 <<- numeric(p)
	interv.conf.super <<- numeric(p)
  interv.conf.super2 <<- numeric(p)
  
  # Criar variavel aleatoria x e x2 usando distribuicao normal padronizada 
	x <<- rnorm(n, 0, 1)
  x2 <<- x*rho + rnorm(n, 0, 1)*sqrt(1-rho^2)
	ssx <<- sum( (x-mean(x))^2 )
  ssx2 <<- sum( (x2-mean(x2))^2 )
  
  # Simular p regressoes usando MQO e os dados produzidos acima
	for(i in 1:p) {
    
		e <<- sigma*rnorm(n, 0, 1)
		#e <<- sigma*rbinom(n,1,0.5)
		#e <<- sigma*rgamma(n,2)
  
		y <<- alfa + beta*x + beta2*x2 + e
		r <<- lm(y~x+x2)
		
    ssres <<- sum(resid(r)^2)
		seresid <<- sqrt( ssres / (n-3) )
		seb.hat <<- seresid/sqrt(ssx)
    seb2.hat <<- seresid/sqrt(ssx2)
  
		lista.alfa.estim[i] <<- coef(r)[1]
  
		lista.beta.estim[i] <<- coef(r)[2]
		lista.test.t[i] <<- (coef(r)[2] - beta) / seb.hat
		interv.conf.inferior[i] <<- coef(r)[2] + qt((siglevel/2), (n-2)) * seb.hat
		interv.conf.super[i] <<- coef(r)[2] + qt((1-(siglevel/2)), (n-2))*seb.hat	
  
    lista.beta2.estim[i] <<- coef(r)[3]
  	lista.test.t[i] <<- (coef(r)[3] - beta2) / seb2.hat
		interv.conf.inferior[i] <<- coef(r)[3] + qt((siglevel/2), (n-3)) * seb2.hat
		interv.conf.super[i] <<- coef(r)[3] + qt((1-(siglevel/2)), (n-3))*seb2.hat
  
    }  
}



########################################################
###        Propriedade dos estimadores - BLUE        ###
########################################################

# Produzimos 100 regressoes usando uma amostra com 50 casos. Os valores alfa=1, beta=2, beta2=0 e sigma=3 foram determinados para verificarmos as propriedades discutidas no texto
dados(alfa=1, beta=2, beta2=0, rho=.001, n=50, p=100, sigma=3, siglevel=.05)

# Histograma com a distribuição dos betas estimados mostra que MQO produz estimadores consistentes e não-enviesados (observe como o verdadeiro beta é encontrado na média)
par(mfrow=c(1,2))
dens <- density(lista.beta.estim)
hist(lista.beta.estim, main="Distribuição Amostral dos \n Betas 1 Estimados", breaks=25, probability=T, xlim=c(0,4))
lines(dens)
abline(v = mean(lista.beta.estim), col="blue", lty=1, lwd=10)

dens <- density(lista.beta2.estim)
hist(lista.beta2.estim, main="Distribuição Amostral dos \n Betas 2 Estimados", breaks=25, probability=T, xlim=c(-2,2))
lines(dens)
abline(v = mean(lista.beta2.estim), col="blue", lty=1, lwd=10)



# Para mostrar a propriedade da eficiência dos estimadores basta diminuir o erro observado na população. Mantendo todos os outros atributos constantes, diminuimos o sigma=1
dados(alfa=1, beta=2, beta2=0, rho=.001, n=50, p=100, sigma=1, siglevel=.05)

# Histograma com a distribuição dos betas estimados mostra que MQO produz estimadores eficientes quando sigma diminui
par(mfrow=c(1,2))
dens <- density(lista.beta.estim)
hist(lista.beta.estim, main="Distribuição Amostral dos \n Betas 1 Estimados", breaks=25, probability=T, xlim=c(0,4))
lines(dens)
abline(v = mean(lista.beta.estim), col="blue", lty=1, lwd=10)

dens <- density(lista.beta2.estim)
hist(lista.beta2.estim, main="Distribuição Amostral dos \n Betas 2 Estimados", breaks=25, probability=T, xlim=c(-2,2))
lines(dens)
abline(v = mean(lista.beta2.estim), col="blue", lty=1, lwd=10)

