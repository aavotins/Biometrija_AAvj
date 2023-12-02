#Kovariacijas analize
## Dati

library(readxl)
dati <- read_excel("nogan.xlsx")
str(dati)
head(dati, n = 3)

## Lineara regresija

modelis <- lm(Fruit ~ Root, data = dati)
summary(modelis)

## Modela diagnostika

par(mfrow=c(2,2))
plot(modelis)
par(mfrow=c(1,1))

## Datu grafiskais attelojums 

library(ggplot2)
ggplot(dati, aes(Root, Fruit, color = Grazing)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = "Saknu biomasa", y = "Virszemes biomasa",
             color = "Veids")

## Modela definesana

ancova <- lm(Fruit ~ Grazing*Root, data = dati)
summary(ancova)

## Modela matrice

model.matrix(ancova)

## Modela definesana

ancova2 <- lm(Fruit ~ Grazing + Root, data = dati)
summary(ancova2)

## Modela matrice

model.matrix(ancova2)

## Modelu salidzinajums

anova(ancova, ancova2)
library(MuMIn)
AICc(ancova,ancova2)

## Modela diagnostika

par(mfrow = c(2, 2))
plot(ancova2)
par(mfrow = c(1, 1))

plot(ancova2,1)
plot(ancova2,2)
plot(ancova2,3)
plot(ancova2,5)
plot(ancova2,4)
acf(residuals(ancova2),lag.max = 40)


## Dati - 2. piemers

dati2 <- read_excel("vecums2.xlsx")
str(dati2)
summary(dati2)

## Datu grafiskais attelojums

ggplot(dati2, aes(age, weight, color = sex)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = "Vecums", y = "Svars",
             color = "Dzimums")

## Modela definesana

ancova <- lm(weight ~ sex*age, data = dati2)
ancova2 <- lm(weight ~ sex+age, data = dati2)

AICc(ancova,ancova2)
anova(ancova,ancova2)

ancova <- lm(weight ~ sex*age, data = dati2)
summary(ancova)

## Modela matrice

model.matrix(ancova)

## Modela diagnostika

par(mfrow = c(2, 2))
plot(ancova)
par(mfrow = c(1, 1))

plot(ancova,4)

acf(residuals(ancova))
