install.packages(akima)
library(boot)
library(bootstrap)
library(colorRamps)
library(diagram)
library(MASS)
library(ggExtra)
library(ggplot2)
library(gmodels)
library(mvtnorm)
library(reshape2)
library(scales)
library(scatterplot3d)
library(stats)
library(stats4)
library(tables)

########### QUESTAO 10 ############

load(url(description = "https://www.ime.usp.br/~pam/dados.RData"))

# tabela2.1
tab2_1 <- read.table("tabela2_1.csv", dec = ",", sep = ";", h = T)
names(tab2_1)
summary(tab2_1$salario)

# tabela2.2
ni <- table(tab2_1$grau_instrucao)
fi <- prop.table(ni)
p_fi <- 100*prop.table(ni)

# adiciona linhas de total
ni <- c(ni, sum(ni))
fi <- c(fi, sum(fi))
p_fi <- c(p_fi, sum(p_fi))
names(ni)[4] <- "Total"
tab2_2 <- rbind(ni, fi = round(fi, digits = 2)), p_fi = round(p_fi, digits = 2))

#tabela2.3
tab2_3 <- as.data.frame(
          t(rbind(
            ni = c(650, 1020, 330, 2000),
            p_fi = c(32.5, 51, 16.5, 1)
          ))
          , row.names = c("Fundamental", "Medio", "Superior", "Total")
          )
tab2_3

##########EXEMPLO 2.3#########
#TABELA2.4
ni < table(cut(tab2_1$salario, breaks = seq(4, 24, by = 4), right = FALSE))
TAB2_4 <- rbind(ni, p_fi = 100*prop.table(ni))

TAB2_4 <- as.data.frame(
          t(cbind(
                  TAB2_4,
                  c(sum(TAB2_4[1, ]), sum(TAB2_4[2, ])
          ))), row.names = c(colnames(TAB2_4), "Total"))
TAB2_4 <- transform(TAB2_4, p_fi = round(p_fi, digits[2]))

#########GRÁFICOS##########
###########################
#########EXEMPLO2.4########
#FIGURA2.2
barplot(
  table(tab2_1$grau_instrucao),
  ylab = "Frequencia",
  cex.names = 0.7,
  names.arg = c("Fundamental", "Medio", "Superior"),
  col = "darkgrey",
  border = NA
  axes = TRUE
  ylim = c(0, 20)
  )



#########3.1 MEDIDAS DE POSIÇÃO########
#######################################
#exemplo 3.1

mean(tab2_1$n_filhos, na.rm = T)

#exemplo3.2

fig27 <- hist(tab2_1$salario, breaks = seq(4, 24, by = 4), right = FALSE, plot = F)
median(tab2_1$salario)
moda2(tab2_1$grau_instrucao)
tab2_1$grau_instr_num <- 1*c(tab2_1$grau_instrucao == "ensino fundamental") + 
  2*c(tab2_1$grau_instrucao == "ensino médio") + 
  3*c(tab2_1$grau_instrucao == "superior")
median(tab2_1$grau_instr_num)
levels(as.factor(tab2_1$grau_instrucao)) [median(tab2_1$grau_instr_num)]
mean(tab2_1$salario[
                   tab2_1$salario > quantile(tab2_1$salario, probs = 0.01)
                   &
                   tab2_1$salario < quantile(tab2_1$salario, probs = 0.99)
                   
                   ])
mean(tab2_1$salario)

###########3.2 MEDIDAS DE DISPERSÃO#############
################################################
#EXEMPLO 3.3

n <- sum(!is.na(tab2_1$n_filhos))
var(tab2_1$n_filhos, na.rm = T)*(n - 1)/n 
sd(tab2_1$n_filhos, na.rm = T)*(n - 1)/n
mean(abs(tab2_1$n_filhos[!is.na(tab2_1$n_filhos)] - mean(tab2_1$n_filhos, na.rm = T)))
x < - c(0, 1, NA, NA, 0, -2, pi, NA)
is.na(x)
!is.na(x)

#EXEMPLO 3.4
mean(tab2_1$salario, na.rm = T)
dm(tab2_1$salario)
varp(tab2_1$salario, na.rm = T)

###########3.3 QUANTIS EMPÍRICOS###############
###############################################
#EXEMPLO 3.5

x <- c(15, 5, 3, 8, 10, 2, 7, 11, 12)
median(x)
mean(x)
quantile(x, probs = c(.25, .50, .75), type = 5)

x <- c(sort(x), 67)
median(x)
mean(x)
quantile(x, probs = c(.25, .50, .75), type = 5)

quantile(x, probs = 0.2)

#EXEMPLO 3.6

fig27 <- hist(tab2_1$salario, breaks = seq(4, 24, by = 4), right = FALSE, 
              probability = T, plot = F)
aux <- with(fig27, 100*density* diff(breaks)[1])
labs <- paste(round(aux), "%", sep = "")

plot(fig27,
     freq = FALSE, labels = labs,
     xlab = "Salário",
     ylab = "",
     col = "darkgrey",
     border = "white",
     yaxt = "n",
     xlim = c(0, 24), xaxp = c(0, 24, 6),
     ylim = c(0, .1), main = "")

median(tab2_1$salario)
quantile(tab2_1$salario, probs = c(.20, .5, .95, .75))

q <- quantile(tab2_1$salario, probs = c(.25, .5, .95, .75))
q

dq = q[3] - q[1]
print(paste("Distancia inter-quatis (d) = ", dq))

#EXEMPLO 3.7

summary(cd_municipios$populacao, digits = 4)
summary(cd_municipios$populacao [-c(1, 2)], digits = 4)
q(y)
