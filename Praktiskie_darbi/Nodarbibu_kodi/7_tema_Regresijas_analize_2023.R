#Regresijas analize
## Dati

library(readxl)
bietes <- read_excel("bietes.xlsx")
str(bietes)
summary(bietes)

modelis <- lm(svars ~ udens, data = bietes)
summary(modelis)

confint(modelis)

## Modela diagnostika
par(mfrow = c(2, 2))
plot(modelis)
par(mfrow = c(1, 1))

plot(modelis,4)

acf(residuals(modelis),lag.max = 28)


## Vērtibu prognozesana

koef <- coefficients(modelis)
koef
udens2 <- 301
raza2 <- koef[1] + koef[2] * udens2
raza2

jaunidati <- data.frame(udens = 301)
predict(modelis, jaunidati, interval = "confidence")

predict(modelis, jaunidati, interval = "predict")


## Rezultatu paradisana

library(ggplot2)
ggplot(bietes, aes(udens, svars)) + geom_point() + 
      geom_smooth(method = "lm")

## Daudzfaktoru regresijas analizes funkcijas
## Dati

library(readxl)
dati <- read_excel( "renda.xlsx")
str(dati)

summary(dati)

## Modela definesana

modelis <- lm(hron ~ dec + jan + feb + mar, data = dati)
summary(modelis)

library(car)
vif(modelis)

modelis1 <- lm(hron ~ jan + feb + mar, data = dati)
summary(modelis1)

modelis2 <- lm(hron ~ feb + mar, data = dati)
summary(modelis2)

cbind(coefficients(modelis2),confint(modelis2))

## Modela diagnostika
par(mfrow = c(2, 2))
plot(modelis2)

acf(residuals(modelis2),lag.max=81)


## Modelu salidzinajums

AIC(modelis,modelis2)

library(MuMIn)
AICc(modelis,modelis2)

anova(modelis,modelis2)

