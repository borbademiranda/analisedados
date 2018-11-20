library(foreign)
install.packages("nnet")
library(nnet)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
library(effects)
library(tidyverse)

##### QUESTÃO 1 #####

# importando a base de dados

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# visualizando algumas estatísticas descritivas das variáveis da base de dados

with(data = ml, table(ses, prog))

with(data = ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), 
                                                                 SD = sd(x)))))

# definindo a categoria de referência da variável resposta

ml$prog2 <- relevel(ml$prog, ref = "academic")

# gerando o modelo multinomial

test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)

# calculando o p-value

z <- summary(test)$coefficients/summary(test)$standard.errors
z

# z-test bicaudal

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# exponenciando os coeficientes para acessar as razões de risco das variáveis

exp(coef(test))

# calculando as probabilidades preditas, visualizando as primeiras linhas do 
# banco de dados

head(pp <- fitted(test))

# examinando as mudanças nas probabilidades preditas 
# criando pequenos datasets mantendo uma das variáveis constantes

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

# média da probabilidade predita da variável "write" para cada nível da variável 
# "ses"

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), 
                     write = rep(c(30 : 70)))

# probabilidades preditas para cada valor de "ses" e "write"

pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = T))

# calculando a média das probabilidades para cada nível da variável "ses"

by(pp.write[, 3 : 5], pp.write$ses, colMeans)

# unindo o objeto "pp.write" às demais variáveis

lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)

# plotando as probabilidades preditas para cada nível da variável "ses" 

ggplot(data = lpp, aes(x = write, y = probability, colour = ses)) +
  geom_line() + 
  facet_grid(variable ~ ., scales = "free")

##### QUESTÃO 2 #####

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")


##### QUESTÃO 3 #####

# letra a)

# modificando a categoria de referência da variável "prog"

ml$prog3 <- relevel(ml$prog, ref = "vocation")

# gerando o modelo

test2 <- multinom(prog3 ~ ses + write, data = ml)
summary(test2)

# os coeficientes do modelo nos mostra que uma mudança em uma unidade na variável
# write aumenta em 0.05 as log odds de estar no programa geral em relação ao
# programa vocacional. Para o programa acadêmico, as log odds aumentam em 0.11 em
# relação ao programa vocational. Em relação ao status socioeconômico, sair do nível
# baixo para o nível alto diminui as log odds do aluno estar em um programa geral
# em relação a um vocacional em -0.18. Para o programa acadêmico, esse efeito aumenta
# as log odds em 0.98 em relação ao programa vocational. A passagem do status 
# socioeconômico baixo para o médio provoca uma diminuição nas log odds do aluno 
# estar em um programa geral em relação a um vocational em -0.82. Para o programa
# academic em relação ao programa vocational, essa queda é de -o.29. 

# modificando a categoria de referência da variável prog para "general"

ml$prog4 <- relevel(ml$prog, ref = "general")

# criando o modelo

test3 <- multinom(prog4 ~ ses + write, data = ml)
summary(test3)

# a mudança do status socioeconômico baixo para médio aumenta as log odds de o aluno 
# estar no programa acadêmico em relação ao programa geral em 0.53. A passagem para
# o ses alto aumenta as log odds em 1.16. A passagem do status socioeconômico 
# baixo para médio aumenta as log odds do aluno estar no programa vocacional
# em relação ao programa geral em 0.82. Já a mudança para o status socioeconômico
# alto aumenta as log odds em 0.18. 

##### QUESTÃO 4 #####

setwd("C:/Users/test/Documents/GitHub/analisedados/lista8")

db4 <- read.dta("nomocc2.dta")
head(db4)
with(data = db4, table(occ))

db4$occ[levels(db4$occ) %in% c("Menial", "BlueCol", "Craft")] <- "Manual" 
with(db4, table(occ))
