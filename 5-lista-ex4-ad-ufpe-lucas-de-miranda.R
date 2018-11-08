library(tidyverse)
library(ggplot2)

################################################################################
# QUESTAO 4.1 - LETRA A

reg1 <- lm(data = wordrecall, prop ~ time)
summary(reg1)

ggplot(data = reg1, mapping = aes(x = time, y = prop)) +
  geom_point() +
  geom_smooth(data = reg1, method = lm, se = F)

# o modelo apresenta um r² igual a 0.5709, o que significa que a variável independente
# explica aproximadamente 57% da variação em Y. O coeficiente da variável "time"
# é igual a -5.571, estatisticamente significante ao nível 0.001. Porém, ao plotarmos
# o gráfico, verificamos uma distribuição não linear.

logtime <- wordrecall$time %>% log 
# tive que criar um objeto com o log da variável time por que inserindo o argumento
# log(time) dava o seguinte erro na hora de plotar: Error in log(time) : non-numeric argument to mathematical function
                                  

linearlog <- lm(data = wordrecall, prop ~ logtime)
summary(linearlog)

ggplot(data = linearlog, mapping = aes(x = logtime, y = prop)) +
  geom_point() +
  geom_smooth(data =  linearlog, method = lm, se = F)

plot(fitted(linearlog), residuals(linearlog)) #qual é a chamada para este gráfico no ggplot?

# os coeficientes do modelo apresentam uma maior significância e o r² aumentou
# significativamente, mostrando que o modelo explica aproximadamente 98% da variação
# da variável dependente. Os resíduos do modelo apresentam distribuição normal 
# em torno de zero. 

getwd()
setwd("./lista5")

################################################################################
# QUESTAO 4.1 - LETRA B

shortleaf <- read.delim("shortleaf.txt")

View(shortleaf)

regb1 <- lm(data = shortleaf, Vol ~ Diam)
summary(regb1)
ggplot(data = regb1, mapping = aes(x = Diam, y = Vol)) +
  geom_point() +
  geom_smooth(data = regb1, method = lm, se = F)
plot(fitted(regb1), residuals(regb1))

# os valores e seus resíduos não possuem uma distribuição linear, sendo necessário
# transformar tanto a variável dependente quanto a independente

logvol <- shortleaf$Vol %>% log()
logdiam <- shortleaf$Diam %>% log()

regb2 <- lm(data = shortleaf, logvol ~ logdiam)
summary(regb2)

ggplot(data = regb2, mapping = aes(x= logdiam, y = logvol)) +
  geom_point() +
  geom_smooth(data = regb2, method = lm, se = F)

plot(fitted(regb2), residuals(regb2))

# o modelo apresenta coeficientes estatisticamente significantes em nível máximo
# a variável independente explica 97% da variação em Y

################################################################################
# QUESTAO 4.1 - LETRA c
View(mammgest)

regc1 <- lm(data = mammgest, Gestation ~ Birthwgt)
summary(regc1)

ggplot(data = regc1, mapping = aes(x = Birthwgt, y = Gestation)) +
  geom_point()

plot(fitted(regc1), residuals(regc1))

# o modelo apresenta uma variância não uniforme dos parâmetros e dos residuais
# transformando a variável dependente em log

loggest <- mammgest$Gestation %>% log

regc2 <- lm(data = mammgest, loggest ~ Birthwgt)
summary(regc2)

ggplot(data = regc2, mapping = aes(x = Birthwgt, y = loggest)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

plot(fitted(regc2), residuals(regc2))

# a significancia do coeficiente da variavel independente permanece a mesma, o r² 
# variou negativamente. O intercepto diminuiu de 187.08 para 5.27.

################################################################################
# QUESTÃO 4.2 - LETRA B

getwd()
setwd("./lista5")

bluegills <- read.delim("bluegills.txt")
View(bluegills)

reg42b <- lm(data = bluegills, length ~ age)
summary(reg42b)

plot(fitted(reg42b), residuals(reg42b))
ggplot(data = reg42b, mapping = aes(x = age, y = length)) +
  geom_point()

# o modelo possui um r2 relativamente alto (0.73) e coeficientes estatisticamente
# significantes, porem o erro padrao do residuo e alto e a distribuicao dos residuais
# e valores preditos segue um padrao

reg42b2 <- lm(data = bluegills, length ~ age + I(age^2))
summary(reg42b2)

ggplot(data = reg42b2, mapping = aes(x = age, y = length)) + 
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x + I(x^2), se = T)

plot(reg42b2)

summary(reg42b)
summary(reg42b2)
