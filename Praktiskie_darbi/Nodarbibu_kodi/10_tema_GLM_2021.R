#GLM
## Dati - binara logistiska regresija

data(plasma, package = "HSAUR")
head(plasma)

## Dati - binara logistiska regresija

summary(plasma)

## Datu grafiska analize

layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)

## Modela definesana

plasma_glm_1 <- glm(ESR ~ fibrinogen, data = plasma,  family = binomial("logit"))

## Modela rezultats

summary(plasma_glm_1)

koeficienti=exp(coefficients(plasma_glm_1))
ticamibas_intervali=exp(confint(plasma_glm_1))
cbind(koeficienti,ticamibas_intervali)

## Modela definesana 2

plasma_glm_2 <- glm(ESR ~ fibrinogen+globulin, data = plasma,family = binomial("logit"))
summary(plasma_glm_2)

koeficienti2=exp(coefficients(plasma_glm_2))
ticamibas_intervali2=exp(confint(plasma_glm_2))
cbind(koeficienti2,ticamibas_intervali2)


## Modelu salidzinajums - logistiska regresija

anova(plasma_glm_1, plasma_glm_2, test = "Chisq")
library(MuMIn)
AICc(plasma_glm_1,plasma_glm_2)

## Iespejamibu prognozesana

predict(plasma_glm_1)
predict(plasma_glm_1,type="response")

## Izskaidrota variacija

library(pscl)
pR2(plasma_glm_1)

## Dati - 2. piemers

data("womensrole", package = "HSAUR")
head(womensrole)

## Dati - 2. piemers

summary(womensrole)

## Modela definesana - 2. piemers

womensrole_glm_1 <- glm(cbind(agree, disagree) ~ sex + education, data = womensrole, family = binomial("logit"))

## Modela rezultats - 2. piemers

summary(womensrole_glm_1)

library(biostat3)
eform(womensrole_glm_1)


## Grafiskais attelojums

myplot <- function(role.fitted) {
 f <- womensrole$sex == "Female"
 plot(womensrole$education, role.fitted, type = "n", ylab = "Probability of agreeing",
 xlab = "Education", ylim = c(0, 1))
 lines(womensrole$education[!f], role.fitted[!f], lty = 1)
 lines(womensrole$education[f], role.fitted[f], lty = 2)
 lgtxt <- c("Fitted (Males)", "Fitted (Females)")
 legend("topright", lgtxt, lty = 1:2, bty = "n")
 y <- womensrole$agree/(womensrole$agree + womensrole$disagree)
 text(womensrole$education, y, ifelse(f, "\\VE", "\\MA"),
 family = "HersheySerif", cex = 1.25)
 }


## Grafiskais attelojums

myplot(predict(womensrole_glm_1, type = "response"))

## Modela definesana - 2. piemers

womensrole_glm_2 <- glm(cbind(agree, disagree)~  sex * education,data = womensrole, family = binomial("logit"))
summary(womensrole_glm_2)

eform(womensrole_glm_2)

## Rezultatu grafiskais attelojums - 2. piemers

myplot(predict(womensrole_glm_2, type = "response"))

## Dati - Puasona regresija

library(readxl)
sugas <- read_excel("species.xlsx")

str(sugas)
summary(sugas)

## Modela definesana - Puasona regresija

model1 <- glm(Species ~ Biomass + pH, family=poisson("log"),data=sugas)
summary(model1)

eform(model1)

## Modela definesana - Puasona regresija

model2 <- glm(Species ~ Biomass *pH, family=poisson("log"),data=sugas)
summary(model2)

eform(model2)

## Modela diagnostika

par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

## Modelu salidzinajums

anova(model1, model2, test = "Chisq")
AICc(model1,model2)


## Rezultatu paradisana

library(ggplot2)
ggplot(sugas,aes(Biomass, Species, color = pH)) + geom_point()+
        geom_smooth(method = "glm", method.args = list(family = "poisson"))