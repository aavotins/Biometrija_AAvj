## Attelu veidosana

library(readxl)
dati <- read_excel("./Dati/augi.xlsx")
head(dati)


dati_stab <- data.frame(Paraugs = c("PA","AD","LL","RT","CV"),
                        Skaits = c(5, 4, 15, 8, 2))
dati_stab

## Attelu sadalisna dalas

library(ggplot2)
ggplot(dati, aes(garums, lapas)) + geom_point() +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(. ~ stress) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(stress ~ .)  +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(gaisma ~ stress) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(gaisma ~ stress, margins=TRUE) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(. ~ stress, scales = "free") +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_grid(. ~ stress, scales = "free", 
              space = "free") +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_wrap(~ stress) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(garums, lapas)) + geom_point() + 
   facet_wrap(~ stress, ncol = 1) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))


## Stabinu seciba

library(ggplot2)
ggplot(dati_stab, aes(Paraugs, Skaits)) + 
      geom_col()

ggplot(dati_stab, aes(reorder(Paraugs,Skaits), Skaits)) + 
      geom_col()

ggplot(dati_stab, aes(reorder(Paraugs,-Skaits), Skaits)) + 
      geom_col()

## Grupu atdalisana

ggplot(dati, aes(gaisma, lapas, shape = stress)) + 
   stat_summary(fun.data = "mean_cl_normal") +
   labs(x = "Stresa līmenis", y = expression(paste("Lapu laukums, ", cm^{2})))

ggplot(dati, aes(gaisma, lapas, shape = stress)) + 
   stat_summary(fun.data = "mean_cl_normal", 
                position = position_dodge(width = 0.25)) +
   labs(x = "Stresa līmenis", y = expression(paste("Lapu laukums, ", cm^{2})))

## Izvairamies slēpt datus

ggplot(dati, aes(gaisma, lapas, shape = stress)) + 
  geom_violin(position = position_dodge(width = 0.9))+
  geom_point(position = position_jitterdodge(jitter.width = 0.25),col="grey")+
  stat_summary(fun.data = "mean_cl_normal", 
               position = position_dodge(width = 0.9)) +
  labs(x = "Stresa līmenis", y = expression(paste("Lapu laukums, ", cm^{2})))+
  theme_classic()

## Izlases apjoms

library(EnvStats)
ggplot(dati, aes(gaisma, lapas, shape = stress)) + 
  geom_violin(position = position_dodge(width = 0.9))+
  geom_point(position = position_jitterdodge(jitter.width = 0.25),col="grey")+
  stat_summary(fun.data = "mean_cl_normal", 
               position = position_dodge(width = 0.9)) +
  labs(x = "Gaismas līmenis", y = expression(paste("Lapu laukums, ", cm^{2})))+
  theme_classic()+
  stat_n_text()

## Grupu atšķirību būtiskums

dati$apvienots=interaction(dati$gaisma,dati$stress)
tests=aov(lapas~apvienots,data=dati)
library(rstatix)
salidzinajumi=tukey_hsd(tests)
salidzinajumi$y.position=c(360,370,380,390,400,410)
salidzinajumi


attels=ggplot(dati, aes(apvienots, lapas)) + 
  geom_violin()+
  geom_point(position = position_jitter(width = 0.25),col="grey")+
  stat_summary(fun.data = "mean_cl_normal", 
               position = position_dodge(width = 0.9)) +
  labs(x = "Gaisma stresa grupās", y = expression(paste("Lapu laukums, ", cm^{2})))+
  theme_classic()+
  stat_n_text()
attels

attels+ggpubr::stat_pvalue_manual(salidzinajumi,label="p.adj.signif",size=3)






## Regresijas attelojums
library(ggplot2)
library(ggpmisc)
ggplot(iris,aes(Petal.Width,Petal.Length)) +
      geom_point() +
      geom_smooth(method = "lm") +
      stat_poly_eq(formula = y ~ x) +
   labs(x = "Vainaglapu platums, cm", y = "Kauslapu garums, cm")

ggplot(iris,aes(Petal.Width,Petal.Length)) +
      geom_point() +
      geom_smooth(method = "lm") +
      stat_poly_eq(formula = y ~ x, parse = TRUE, 
                   aes(label = after_stat(adj.rr.label))) +
   labs(x = "Vainaglapu platums, cm", y = "Kauslapu garums, cm")

ggplot(iris,aes(Petal.Width,Petal.Length)) +
      geom_point() +
      geom_smooth(method = "lm") +
      stat_poly_eq(formula = y ~ x, 
                   mapping=use_label('eq.label')) +
   labs(x = "Vainaglapu platums, cm", y = "Kauslapu garums, cm")

ggplot(iris,aes(Petal.Width,Petal.Length)) +
      geom_point() +
      geom_smooth(method = "lm") +
      stat_poly_eq(formula = y ~ x,
                   mapping=use_label(c("eq.label","adj.rr.label"))) +
   labs(x = "Vainaglapu platums, cm", y = "Kauslapu garums, cm")

ggplot(iris,aes(Petal.Width,Petal.Length, color = Species)) +
      geom_point() +
      geom_smooth(method = "lm") +
      stat_poly_eq(formula = y ~ x,
                   mapping=use_label(c("eq.label","adj.rr.label"))) +
   labs(x = "Vainaglapu platums, cm", y = "Kauslapu garums, cm")

## Vairaku attelu apvienosana viena

library(cowplot)

plot.lapas <- ggplot(dati, aes(garums, lapas, 
                               colour = gaisma)) + geom_point(size=2.5) +
   labs(x = "Auga garums, cm", y = expression(paste("Lapu laukums, ", cm^{2})))
plot.lapas

plot.stabini <- ggplot(dati_stab, aes(Paraugs, Skaits, fill = Paraugs)) + 
   geom_col() +
   theme(axis.text.x = element_text(angle=70, vjust=0.5))
plot.stabini

plot_grid(plot.lapas, plot.stabini, labels = c("A","B"))

plot_grid(plot.lapas, plot.stabini, 
          labels = c("A", "B"), align = "h")

plot_grid(plot.lapas, plot.stabini, 
          labels = c("A", "B"), nrow = 2, align = "v")

plot_grid(plot.stabini,
          plot_grid(plot.lapas, plot.lapas, labels = c("B","C")), 
          nrow = 2, labels = c("A",""))
