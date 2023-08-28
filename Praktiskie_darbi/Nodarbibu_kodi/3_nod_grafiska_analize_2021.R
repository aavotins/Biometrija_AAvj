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