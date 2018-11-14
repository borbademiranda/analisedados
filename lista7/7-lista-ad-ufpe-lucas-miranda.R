##### QUESTÃO 5 #####

##### descriptions of the data #####
# carregando pacotes utilizados no exemplo
install.packages("aod")
library(aod)
library(ggplot2)

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