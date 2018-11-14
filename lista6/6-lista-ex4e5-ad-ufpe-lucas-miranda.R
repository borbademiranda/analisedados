##### QUESTÃO 4 #####

library(foreign)
library(tidyverse)

dbsamuels <- read.dta("samuels.dta")
head(dbsamuels)


model1 <- dbsamuels %>% filter(yeardeputy != 1950)

m1 <- lm(data = dbsamuels, enlists ~  logmag + proxgov + engov_proxgov + proxpres + 
           enpres_proxpres + factor(yeardeputy) + factor(state))

summary(m1)

# tentativas frustradas de fazer o modelo utilizando somente os anos das eleições
# que o autor utiliza no artigo

samuels1 <- dbsamuels[dbsamuels$yeardeputy != 1950 &
                      dbsamuels$yeardeputy < 1964 |
                      dbsamuels$yeardeputy > 1989,]
summary(samuels1$yeardeputy)
View(samuels1)

samuels2 <- dbsamuels[dbsamuels$yeardeputy %in% c(1950, 1998),]

samuels3 <- dbsamuels[dbsamuels$yeardeputy %in% c(1994, 1998),]
             
        
dbsamuels2 <- ifelse(dbsamuels, yes = dbsamuels$yeardeputy == c("1950", "1994", "1998"),)
?ifelse

summary(samuels1$proxgov)

summary(dbsamuels$proxgov)



##### QUESTAO 5 #####

hillary <- read.dta("hillary.dta")
head(hillary)

model1hillary <- lm(data = hillary, hillary_thermo ~ income + jewish + income*jewish)
summary(model1hillary)

-0.99 + 1.41

# o efeito da renda sobre a avaliacao de hillary e negativo e estatisticamente significante
# porem quando utilizamos um modelo interativo com as variaveis "income" e "jewish"
# verificamos um efeito positivo da renda quando o individuo e judeu. O coeficiente
# do termo interativo e igual a 0.42, valor correspondente à soma de b¹ e b³

library(ggplot2)

# plots do modelo
plot(model1hillary)

ggplot(data = model1hillary, mapping = aes(x = income + fjewish + income*jewish, 
                                           y = hillary_thermo)) +
       geom_point()

plot(residuals(model1hillary), fitted(model1hillary))

# gráfico com os coeficientes de regressão e com os intervalos de confiança
library(dotwhisker)
library(broom)

dwplot(model1hillary, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))


# plot studentized residuals x valores preditos
library(car)

spreadLevelPlot(model1hillary)





