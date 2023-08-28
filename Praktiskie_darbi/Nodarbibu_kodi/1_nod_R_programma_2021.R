#Programma R

#Palidzibas iegusana
help(plot)

example(plot)

#R ka kalkulators
4+7
log(8,2)
exp(2)

#Datu importesana
library(readxl)
dati <- read_excel("niedres.xlsx")
dati

#Iebuvetie dati
data()
data(cars, package = "datasets")
head(cars)

#Datu struktura
str(dati)

#Kolonnu nosaukumi
names(dati)

#Datu apskatisana
head(dati)
tail(dati, n = 2)

#Datu atlasisana
dati[2,2]
dati[1,]
dati[,2]

dati$garums
dati["garums"]

#Darbs ar datiem
garums

#Datu atlasisana
dati[c(4,6,8),]

#Objektu veidosana
dati2 <- dati[2,]
dati2


#Datu strukturas

#Vektors
pirmais <- c(1,5,7,4,8,10)
pirmais

otrais <- c("A","B","C","D","E","F")
otrais


#Matrice
tresais <- matrix(1:15,ncol=3)
tresais


#Datu tabula
septitais <- data.frame(kol1 = c(1,2,3,4), kol2 = c(5,6,7,8))
septitais

#Datu atlasisana - vektors
otrais[1]
otrais[c(1,4)]
otrais[-3]