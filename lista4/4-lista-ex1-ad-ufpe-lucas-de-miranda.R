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

# a regress�o acima tem como vari�vel dependente o voto do presidente dos USA de 
# 1880 ao ano de 2004. A vari�vel independente do modelo � o crescimento da 
# economia do pa�s, cujo coeficiente � 0.6536, significando que o aumento de uma
# unidade em "GROWTH" � traduzido em uma varia��o positiva de 0.6536 em "VOTE". 
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
# vari�vel independente "GROWTH", o que significa dizer que o aumento de uma 
# unidade na vari�vel "GOODNEWS" possui impacto positivo na variacao de "VOTE" em 
# 0.71. O modelo com as duas vari�veis independentes explica aproximadamente 46% 
# da variacao da vari�vel dependente "GROWTH" (r� = 0.4639), sendo assim, mais
# robusto que o modelo solicitado no item a da mesma quest�o.

# comando para solicitar a m�dia dos residuos do modelo "reg2". M�dia dos 
# res�duos igual a 4.26742
library(tidyverse)
residuals(reg2) %>% mean

# plotando os valores preditos e os res�duos do modelo para verificar homocedasticidade
# o modelo � homoced�stico por que o erro varia de forma igual
fitted(reg2)
plot(fitted(reg2), residuals(reg2))

var(residuals(reg2))


##### QUESTAO 1 - D #####
reg3 <- lm(data = bdstata, VOTE ~ GROWTH + GOODNEWS + WAR)
summary(reg3)

# a vari�vel "WAR" n�o possui signific�ncia estat�stica quando adicionada ao modelo
# controlada por "GROWTH" e "GOODNEWS". o r� permanece igual, por�m o r� ajustado
# diminui, significando que o modelo perde qualidade em rela��o ao anterior, solicitado
# na letra c da quest�o. Em rela��o ao modelo solicitado no item anterior � o 
# melhor modelo em rela��o aos tr�s solicitados, j� que possui tanto o maior r� e
# r� ajustado.

# an�lise sobre os res�duos do modelo
residuals(reg3) %>% mean
plot(fitted(reg3), residuals(reg3))

# a m�dia dos res�duos do modelo � igual a 3.33, e atrav�s do plot dos valores 
# preditos e residuais, verifica-se que o erro varia uniformemente 

## a vari�vel independente que possui maior efeito sobre a vari�vel dependente
## "VOTE" � "GOODNEWS", com coeficiente de 0.7177 no modelo com melhro ajuste