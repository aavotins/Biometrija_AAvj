#####################
#Korelacijas analize#
#####################

library(readxl)
smiltaji <- read_excel("smiltaji.xlsx")

summary(smiltaji)
pairs(smiltaji)

library(ggplot2)
library(cowplot)
p1 <- ggplot(smiltaji, aes(sample = pH)) + geom_qq() + geom_qq_line()
p2 <- ggplot(smiltaji, aes(sample = smilts)) + geom_qq() + geom_qq_line()
p3 <- ggplot(smiltaji, aes(sample = sunas)) + geom_qq() + geom_qq_line()
p4 <- ggplot(smiltaji, aes(sample = sugas)) + geom_qq() + geom_qq_line()
plot_grid(p1, p2, p3, p4, ncol = 2)


shapiro.test(smiltaji$pH)
shapiro.test(smiltaji$smilts)
shapiro.test(smiltaji$sunas)
shapiro.test(smiltaji$sugas)

cor(smiltaji[, 1:3])

library(ltm)
rcor.test(smiltaji[, 1:3])

cor.test(smiltaji$pH, smiltaji$sunas)

cor.test(smiltaji$sugas, smiltaji$smilts, method = "spearman")

cor.test(smiltaji$sugas, smiltaji$smilts, method = "kendall")

priede <- read_excel("priede.xlsx")

str(priede)
acf(priede)

akor <- acf(priede)
akor

library(corrplot)
data("mtcars")
head(mtcars, n = 2)

M <- cor(mtcars)
corrplot(M)

corrplot(M, method = "color")

corrplot(M, method = "ellipse")

corrplot(M, method = "number")

corrplot(M, method = "color", type = "upper")

rez.mat <- cor.mtest(mtcars)
rez.mat

corrplot(M, p.mat = rez.mat$p, insig = "blank")

corrplot.mixed(M, p.mat = rez.mat$p, insig = "blank",
               tl.pos="lt",upper="ellipse",number.cex=0.75)
