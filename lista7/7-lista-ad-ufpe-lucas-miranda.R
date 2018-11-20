##### QUESTÃO 5 #####

##### descriptions of the data #####
# carregando pacotes utilizados no exemplo
install.packages("aod")
library(aod)
library(ggplot2)
library(tidyverse)

# carregando o banco de dados do exemplo
db5 <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# visualização das primeiras linhas do banco de dados
head(db5)

# estatisticas descritivas das variáveis
summary(db5)

# calculando os desvios padrão e aplicando-os às variáveis
sapply(db5, sd)

# tabela de contingência das variáveis dependente e independentes 
xtabs(~admit + rank, data = db5)

##### using the logit model #####
# transformando a variável "rank" em fator para indicar que a mesma é uma 
# variável categórica

db5$rank <- factor(db5$rank)

logit1 <- glm(admit ~ gre + gpa + rank, data = db5, family = "binomial")
summary(logit1)

# intervalos de confiança 
confint(logit1)

# intervalos de confiança utilizando os erros padrão
confint.default(logit1)

# verificando o efeito da variável "rank"
wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 4 : 6)

# criando um vetor para fazer interagir com o modelo
1 <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(logit1), Sigma = vcov(logit1), L = 1)

# exponenciando os coeficientes do modelo para interpretá-los como razões de chance
exp(coef(logit1))

# colocando as odds ratios e os intervalos de confiança numa tabela
exp(cbind(OR = coef(logit1), confint(logit1)))

# criando um novo data frame para calcular as probabilidades de de admissão
newdata1 <- with(db5, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1 : 4)))
newdata1

newdata1$rankP <- predict(logit1, newdata = newdata1, type = "response" ) 
newdata1

# calculando probabilidades preditas variando as variáveis "rank" and "gre"
newdata2 <- with(db5, data.frame(
  gre = rep(seq(from = 200, to = 800, length.out = 100), 4), 
  gpa = mean(gpa), rank = factor(rep(1 : 4, each = 100))))

newdata3 <- cbind(newdata2, predict(logit1, newdata = newdata2, type = "link", 
                                    se = T))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96*se.fit))
  UL <- plogis(fit + (1.96))
  })

# visualizando as primeiras linhas do objeto criado
head(newdata3)

# utilizando gráficos para uma melhor interpretação do modelo
ggplot(data = newdata3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) +
  geom_line(aes(colour = rank), size = 1)
 
# verificando tstatistic
with(logit1, null.deviance - deviance)

# graus de liberdade da diferença entre os modelos
with(logit1, df.null - df.residual)

# obtendo p-value
with(logit1, pchisq(null.deviance - deviance, df.null - df.residual, 
                    lower.tail = F))

# log likelihood
logLik(logit1)

##### QUESTÃO 6 #####
library(aod)
library(ggplot2)
library(tidyverse)

xtabs(~rank + admit, data = db5)

# transformando novamente a variável "rank" em fator
db5$rank <- factor(db5$rank)

# criando modelo probit com o mesmo banco de dados e variáveis da questão anterior
probit1 <- glm(data = db5, admit ~ gre + gpa + rank, family = binomial(link = "probit"))

# summary do modelo
summary(probit1)

# intervalos de confiança dos coeficientes do modelo
confint(probit1)

# efeito médio para cada um dos níveis da variável "rank"
wald.test(b = coef(probit1), Sigma = vcov(probit1), Terms = 4 : 6)

v <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(probit1), Sigma = vcov(probit1), L = v)

# criando data frame para calcular a probabilidade predita para cada nível da variável "rank"
dataq6 <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 
                               4 * 4), gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), 
                   rank = factor(rep(rep(1:4, each = 100), 4)))

head(dataq6)

# criando os gráficos para cada nível da variável "gpa"
exp(cbind(OR = coef(probit1), confint(probit1)))

dataq6[, c("p", "se")] <- predict(probit1, dataq6, type = "response", se.fit = T)[-3]

dataq6b <- cbind(dataq6, predict(probit1, newdata = dataq6, type = "link", se = T))


dataq6b <- within(dataq6, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96*se.fit))
  UL <- plogis(fit + (1.96))
  })

ggplot(data = dataq6, aes(x = gre, y = p, colour = rank)) +
  geom_line() +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) +
  facet_wrap(~gpa)
# sem sucesso em inserir os intervalos de confiança nos gráficos

# change in deviance entre os modelos anteriores
with(probit1, null.deviance - deviance)

# mudança nos gruas de liberdade entre os modelos
with(probit1, df.null - df.residual)

# qui-quadrado e teste p-value
with(probit1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))

# log likelihood
logLik(probit1)

##### QUESTÃO 7 #####
setwd("C:/Users/test/Documents/GitHub/analisedados/lista7")

db7 <- read.csv("sample.csv")

# LETRA A

# verificando a estrutura da variável
str(db7$female)

# transformando a variável em fator
db7$female <- factor(db7$female)

# gerando o modelo
logit71 <- glm(data = db7, hon ~ female, family = "binomial")
summary(logit71)

# intervalos de confiança do modelo com e sem standard errors
confint(logit71)
confint.default(logit71)

# odd ratios das variáveis do modelo
exp(coef(logit71))
exp(cbind(OR = coef(logit71), confint(logit71)))

# de acordo com os coeficientes do modelo, a variável "female", quando 1, possui 
# efeito positivo nas log odds da variável dependente de 0.59
# quando a variável "female" é igual a 1, as razões de chance para a variável 
# dependente são em média de 1.81

# LETRA B

# modelo logit bivariado com a variável independente "math"
str(db7$math)

logit72 <- glm(data = db7, hon ~ math, family = "binomial")
summary(logit72)

# intervalos de confiança
confint(logit72)
confint.default(logit72)

# odd ratios
exp(coef(logit72))
exp(cbind(OR = coef(logit72), confint(logit72)))

# os coeficientes do modelo demonstram que o aumento de uma unidade na variável 
# independente "math" aumenta as razões de chance de ocorrer a variável dependente
# em média em 1.16%

# LETRA C

# modelo com as variáveis independentes "math" and "female"

# criando um novo data frame mantendo as variáveis "math" and "read" constante para acessar
# o efeito da variável "female" na variável dependente

logit73 <- glm(data = db7, hon ~ math + read + female, family = "binomial")

newdataq7 <- with(db7, data.frame(math = mean(math), read = mean(read), 
                                  female = factor(0 : 1)))

# criando nova variável no modelo para calcular as probabilidades preditas
newdataq7$femaleP <- predict(logit73, newdata = newdataq7, type = "response")
newdataq7

# mantendo read and math constantes, o efeito da variável female = 1 aumenta a 
# probabilidade predita da variável dependente hon em 0.23

# novo data frame com os valores das variáveis "math" e "female" variando, a fim
# de verificar o comportamento do modelo com "read" permanencendo na média
newdataq72 <- with(db7, data.frame(math = rep(
  seq(from = 20, to = 100, 
      length.out = 100), 4), 
  read = mean(read), female = factor(rep(0 : 1, each = 100))))

# data frame com os intervalos de confiança
newdataq73 <- cbind(newdataq72, predict(logit73, newdata = newdataq72, 
                                        type = "link", se = TRUE))
newdataq73 <- within(newdataq73, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# plotando o gráfico
ggplot(newdataq73, aes(x = math, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = female), alpha = 0.2) + 
  geom_line(aes(colour = female), size = 1)                   