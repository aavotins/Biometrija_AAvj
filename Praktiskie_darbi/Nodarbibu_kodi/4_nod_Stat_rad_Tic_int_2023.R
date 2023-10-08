## Dati

library(readxl)
niedr <- read_excel("niedres2.xlsx")

## Statistiskie raditaji 1

mean(niedr$garums)
sd(niedr$garums)
var(niedr$garums)
median(niedr$garums)

## Statistiskie raditaji 2

round(mean(niedr$garums),1)

## Statistiskie raditaji 3

x <- c(1:20, NA)
mean(x)
mean(x, na.rm = TRUE)

## Statistiskie raditaji 4

min(niedr$garums)
max(niedr$garums)
range(niedr$garums)

## Statistiskie raditaji 5

quantile(niedr$garums)
quantile(niedr$garums, probs = c(0.025, 0.975))

## Statistiskie raditaji 6

summary(niedr$garums)

## Statistiskie raditaji 7

summary(niedr)

## Statistiskie raditaji 8

tapply(niedr$garums,niedr$paraug,sd)

## Ticamibas intervali 1

n <- length(niedr$garums) #noverojumu skaits
briv <- n-1 #brivibas pakapju skaits
but <- 0.01 #butiskuma limenis
vid <- mean(niedr$garums) #videjais aritmetiskais
stand <- sd(niedr$garums) #standartnovirze

## Ticamibas intervali 2

robeza <- qt(1-but/2,briv)*stand/sqrt(n) #t reiz sx
augsa <- vid+robeza #augseja robeza
apaksa <- vid-robeza  #apakseja robeza
augsa
apaksa

## Ticamibas intervali - pakete Hmisc

library(Hmisc)
smean.cl.normal(niedr$garums,conf.int = 0.99)


tabula <- tapply(niedr$garums, niedr$paraug, smean.cl.normal, conf.int = 0.99)
tabula


## Ticamibas intervali - grafiskais attelojums 1

library(ggplot2)
ggplot(niedr, aes(paraug, garums)) + 
      stat_summary(fun.data = "mean_cl_normal")

ggplot(niedr, aes(paraug, garums)) + 
      stat_summary(fun.data = "mean_cl_normal", fun.args = list(conf.int = 0.99))

tic_tabula <- as.data.frame(do.call(rbind,tabula))
tic_tabula

tic_tabula$Parauglaukums <- rownames(tic_tabula)
tic_tabula

ggplot(tic_tabula, aes(x = Parauglaukums, y = Mean, ymin  = Lower, ymax = Upper)) +
      geom_pointrange() + labs(y = "Videjais ar 99% ticamibas intervalu")

## Ticamibas intervali - binomialais sadalijums

set.seed(1278)
alpha <- 0.05
paraugs <- sample(c(0:1), 50, replace = TRUE)
sadalijums <- table(paraugs)
n <- length(paraugs)
p_hat <- sadalijums["1"]/n
z <- qnorm(1 - alpha/2)
p_apaksa <- p_hat - z * sqrt(p_hat * (1 - p_hat)/n)
p_augsa <- p_hat + z * sqrt(p_hat * (1 - p_hat)/n)
p_apaksa
p_augsa

library(Hmisc)
binconf(sadalijums["1"], n , alpha = alpha, method = "all")
