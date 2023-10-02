##############################
#"Datu grafiska analize"#
##############################

library(readxl)
niedr <- read_excel("niedres2.xlsx")

str(niedr)

library(ggplot2)
ggplot(niedr, aes(npk, garums)) + geom_point()

ggplot(niedr, aes(garums, platums)) + geom_point()

ggplot(niedr, aes(garums, platums, 
                  colour = paraug, shape = paraug)) + 
      geom_point()

ggplot(niedr, aes(paraug, garums)) + geom_point()

ggplot(niedr, aes(paraug, garums)) + 
      geom_jitter(width = 0.1)

ggplot(niedr, aes(paraug, garums)) + geom_boxplot()

ggplot(niedr, aes(garums)) + geom_histogram()

ggplot(niedr, aes(garums)) + 
      geom_histogram(binwidth = 5)

ggplot(niedr, aes(garums)) + 
      geom_histogram(binwidth = 2.5)

ggplot(niedr, aes(sample = garums)) + geom_qq() + 
      geom_qq_line()

library(qqplotr)
ggplot(niedr, aes(sample = garums)) + geom_qq_band() +
   stat_qq_line() +   stat_qq_point() +
   labs(x = "Teoretiskas kvantiles", y = "Paraugkopas kvantiles")

shapiro.test(niedr$garums)

tapply(niedr$garums,niedr$paraug,shapiro.test)


# dinozauri ----

library(datasauRus)
data("datasaurus_dozen")

round(tapply(datasaurus_dozen$x,datasaurus_dozen$dataset,mean),2)
round(tapply(datasaurus_dozen$y,datasaurus_dozen$dataset,mean),2)

round(tapply(datasaurus_dozen$x,datasaurus_dozen$dataset,sd),2)
round(tapply(datasaurus_dozen$y,datasaurus_dozen$dataset,sd),2)

ggplot(datasaurus_dozen, aes(x = x, y = y))+
  geom_point()+
  theme_void()+
  facet_wrap(~dataset, ncol = 5)
