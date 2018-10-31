load("./vote_growth_usa.RData")

read.dta("fair.dta")

install.packages("foreign")
library("foreign")

##### QUESTAO 1 - A #####
bdstata <- read.dta("fair.dta")
load("./fair.dta")

summary(bdstata)
summary(bd)

##### QUESTAO 1 - B #####
reg1 <- lm(data = bdstata, VOTE ~ GROWTH)
summary(reg1)

# a regressão acima tem como variável dependente o voto do presidente dos USA de 
# 1880 ao ano de 2004. A variável independente do modelo é o crescimento da 
# economia do país, cujo coeficiente é 0.6536, significando que o aumento de uma
# unidade em "GROWTH" é traduzido em uma variação positiva de 0.6536 em "VOTE". 
# O modelo explica aproximadamente 35% da variacao da variavel dependente (VOTE).

library(ggplot2)

# plotando a regressao
ggplot(data = reg1, mapping = aes(x = GROWTH, y = VOTE)) +
  geom_point() +
  geom_abline(data = reg1, slope = 0.6249, intercept = 51.5082)

##### QUESTAO 1 - C #####
reg2 <- lm(data = bdstata, VOTE ~ GROWTH + GOODNEWS)
summary(reg2)

# a variavel "GOODNEWS" possui coeficiente de 0.7177 quando controlada pela 
# variável independente "GROWTH", o que significa dizer que o aumento de uma 
# unidade na variável "GOODNEWS" possui impacto positivo na variacao de "VOTE" em 
# 0.71. O modelo com as duas variáveis independentes explica aproximadamente 46% 
# da variacao da variável dependente "GROWTH" (r² = 0.4639), sendo assim, mais
# robusto que o modelo solicitado no item a da mesma questão.

# comando para solicitar a média dos residuos do modelo "reg2". Média dos 
# resíduos igual a 4.26742
library(tidyverse)
residuals(reg2) %>% mean

# plotando os valores preditos e os resíduos do modelo para verificar homocedasticidade
# o modelo é homocedástico por que o erro varia de forma igual
fitted(reg2)
plot(fitted(reg2), residuals(reg2))

var(residuals(reg2))


##### QUESTAO 1 - D #####
reg3 <- lm(data = bdstata, VOTE ~ GROWTH + GOODNEWS + WAR)
summary(reg3)

# a variável "WAR" não possui significância estatística quando adicionada ao modelo
# controlada por "GROWTH" e "GOODNEWS". o r² permanece igual, porém o r² ajustado
# diminui, significando que o modelo perde qualidade em relação ao anterior, solicitado
# na letra c da questão. Em relação ao modelo solicitado no item anterior é o 
# melhor modelo em relação aos três solicitados, já que possui tanto o maior r² e
# r² ajustado.

# análise sobre os resíduos do modelo
residuals(reg3) %>% mean
plot(fitted(reg3), residuals(reg3))

# a média dos resíduos do modelo é igual a 3.33, e através do plot dos valores 
# preditos e residuais, verifica-se que o erro varia uniformemente 

## a variável independente que possui maior efeito sobre a variável dependente
## "VOTE" é "GOODNEWS", com coeficiente de 0.7177 no modelo com melhro ajuste