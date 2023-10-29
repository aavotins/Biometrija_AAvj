# Paraugkopu salidzinasana
## Dati

library(readxl)
niedr2 <- read_excel("lapas.xlsx")

str(niedr2)


## Normalitates parbaude

library(ggplot2)
library(qqplotr)
ggplot(niedr2, aes(sample = platums)) + facet_wrap(~paraug) +
      geom_qq_band() + stat_qq_line() + stat_qq_point() +
      labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

ggplot(niedr2, aes(paraug, platums)) +
      geom_boxplot()

tapply(niedr2$platums, niedr2$paraug, shapiro.test)

## Dispersiju salidzinasana

var.test(niedr2$platums ~ niedr2$paraug)

## Videjo aritmetisko salidzinasana

t.test(niedr2$platums ~ niedr2$paraug, var.equal = TRUE)

## Saistitu paraugkopu videjo aritmētisko salidzinasana

rokas <- read_excel("rokas.xlsx")
str(rokas)

rokas$starpiba <- rokas$kreisa-rokas$laba 
ggplot(rokas, aes(sample = starpiba)) +   
      geom_qq_band() + stat_qq_line() + stat_qq_point() 
ggplot(rokas,aes(starpiba)) + geom_boxplot() 
shapiro.test(rokas$starpiba) 

t.test(rokas$laba, rokas$kreisa, paired = TRUE)

## T-tests vienai paraugkopai

t.test(niedr2$platums, mu = 3.0)

## Neparametriskas metodes - neatkarigas paraugkopas

wilcox.test(niedr2$garums ~ niedr2$paraug)

## Neparametriskas metodes - atkarigas paraugkopas

wilcox.test(rokas$laba, rokas$kreisa, paired = TRUE)

## hi2  tests 

zirni <- c(315, 108, 101, 32)
#dala ar vertibu summu (16), lai iegutu iespejamibas vertibas
teor.zirni <- c(9, 3, 3, 1)/16 

zirnu_tests <- chisq.test(x = zirni, p = teor.zirni)
zirnu_tests$expected

koki <- matrix(c(12,34,56,23,8,27,33,47,14,11), ncol=2)
rownames(koki) <- c("Priede","Egle","Bērzs","Ozols","Kļava")
colnames(koki) <- c("Paraug A","Paraug B")

koki

koku_tests <- chisq.test(koki) 
koku_tests$expected

koku_tests

