# Dispersijas analize
## Dati 1

library(readxl)
miezi <- read_excel("miezi.xlsx")

str(miezi)
head(miezi)

summary(miezi)

## Parveidosana par faktoru

is.factor(miezi$skirne)
miezi$skirne<-as.factor(miezi$skirne)
is.factor(miezi$skirne)

## Vizualais salidzinajums

library(ggplot2)
ggplot(miezi, aes(skirne, raza)) + geom_boxplot()

## Dispersiju salidzinasana

library(car)
leveneTest(y = miezi$raza, group = miezi$skirne)

## Vienfaktora dispersijas analize

anov.miezi <- aov(raza~skirne,data=miezi)
summary(anov.miezi)

## Atbilstibas parbaude
plot(anov.miezi, 2)
shapiro.test(residuals(anov.miezi))

plot(anov.miezi, 1)

## Faktora ietekmes ipatsvars

275/(275+808)

## Gradacijas klasu salidzinasana

TukeyHSD(anov.miezi)

## Videjo vertibu grafiskais attelojums

ggplot(miezi, aes(skirne, raza)) + 
      stat_summary(fun.data = "mean_cl_normal") + 
      labs(x = "Miežu šķirne", y = "Raža")


## Analize bez homogenam dispersijam

oneway.test(raza ~ skirne, data = miezi)

library(rstatix)
games_howell_test(raza ~ skirne, data = miezi)


## Dati

soja <- read_excel("soja.xlsx")

str(soja)
head(soja)

## Datu parbaude

names(soja)
is.factor(soja$gaisma)
is.factor(soja$stress)
soja$gaisma <- as.factor(soja$gaisma)
soja$stress <- as.factor(soja$stress)

## Dispersiju salidzinasana

library(car)
leveneTest(y=soja$lapas,group=soja$gaisma:soja$stress)

## Daudzfaktoru dispersijas analize 1

modelis <- aov(lapas~gaisma*stress,data=soja)
summary(modelis)

## Ietekmes ipatsvara noteiksana

kv.sum <- 42752 + 14858 + 26 + 42976
42752/kv.sum  #gaisma
14858/kv.sum  #stress

## Gradacijas klasu salidzinasana

TukeyHSD(modelis,"gaisma")

## Grafiska paradisana

ggplot(soja, aes(gaisma, lapas, fill = stress)) + 
      geom_boxplot()

## Kruskola-Volisa tests

kruskal.test(raza ~ skirne, data = miezi)
dunn_test(raza ~ skirne, data = miezi)