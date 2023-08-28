# R grafiskas iespejas (ggplot2 grafiki)
# Dati

library(readxl)
dati <- read_excel("augi.xlsx")
head(dati)

## geom_point()

library(ggplot2)
ggplot(dati, aes(garums, lapas)) + geom_point()

ggplot(dati, aes(garums, lapas)) + 
      geom_point(aes(colour = garums))

ggplot(dati, aes(garums, lapas)) + 
      geom_point(aes(colour = gaisma))

ggplot(dati, aes(garums, lapas)) + 
      geom_point(aes(shape = gaisma))
      
ggplot(dati, aes(garums, lapas)) + 
      geom_point(aes(size = garums))
      
ggplot(dati, aes(garums, lapas)) + 
      geom_point(colour = "red", size = 3)

## geom_bar() un geom_col()
      
ggplot(dati, aes(gaisma)) + geom_bar()
      
ggplot(dati, aes(gaisma)) + 
      geom_bar(fill = "white", colour = "darkgreen")
      
ggplot(dati, aes(gaisma, fill = stress)) + geom_bar()

ggplot(dati, aes(gaisma, fill = stress)) + 
      geom_bar(position = "dodge")

## geom_col()
      
df <- data.frame(Dzimums = c("F","M"),
                 Skaits = c(23,19))
ggplot(df, aes(Dzimums, Skaits)) + 
      geom_col()

## geom_histogram()
      
ggplot(dati, aes(lapas)) + 
      geom_histogram()

ggplot(dati, aes(lapas)) + 
      geom_histogram(binwidth = 10)

ggplot(dati, aes(lapas)) + 
      geom_histogram(binwidth = 5)

## geom_line()
      
ggplot(dati, aes(garums, lapas)) + geom_line()

ggplot(dati, aes(garums, lapas)) + 
      geom_line(aes(colour = stress))

ggplot(dati, aes(garums, lapas)) + 
      geom_line(colour = "red", size = 1)

## annotate()
      
ggplot(dati, aes(garums, lapas)) + geom_point() + 
      annotate("text", x = 80, y = 300, 
               label = "Teksts attela")

ggplot(dati, aes(x = garums, y = lapas)) + geom_point() + 
   annotate("rect", xmin = 80, xmax = 100, 
            ymin = 250, ymax = 300, alpha = 0.5)


ggplot(dati, aes(x = garums, y = lapas)) + geom_point() + 
   annotate("segment", x = 75, xend = 120, 
            y = 200, yend = 300, colour = "blue")


## Skalas

ggplot(dati, aes(garums, lapas, color = gaisma)) + 
   geom_point() + 
   scale_colour_manual(values = c("gold","black"))

ggplot(dati, aes(garums, lapas, color = gaisma)) + 
   geom_point()+ 
   scale_colour_manual("Gaismas\ndaudzums",
                       values = c("gold","black"),
                       labels=c("Pietiekams","Nepietiekams"))


ggplot(dati, aes(garums, lapas, color = gaisma)) + 
   geom_point() + 
   scale_y_continuous("Lapu laukums", 
                      breaks = c(175, 225, 275, 325))


df < -data.frame(grupa= c("A","B","C"),
               proporcija = c(0.5,0.3,0.2))
ggplot(df, aes(grupa, proporcija)) + geom_col()



## Nosaukumi
      
ggplot(dati, aes(garums, lapas, color = gaisma))+ 
      geom_point() + 
      labs(x = "X ass",y = "Y ass",
           title = "Galvenais",color = "Krasas")

## Attela izskata maina
      
ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point()

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + theme_bw()

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + theme_minimal()

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + theme_classic()

## theme()

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + labs(title="Virsraksts")

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + labs(title="Virsraksts") + 
      theme(plot.title = element_text(size = rel(2),
                                      colour = "blue"))


ggplot(dati, aes(garums, lapas,colour = gaisma))+ 
   geom_point() + 
   theme(axis.title.y = element_text(size = rel(1.5), 
                                     angle = 90, face = "bold"))


ggplot(dati, aes(garums, lapas,colour = gaisma)) +
      geom_point() + 
      theme(axis.text.y = element_blank())

ggplot(dati, aes(garums, lapas,colour = gaisma))+ 
   geom_point() + 
   theme(legend.position = "bottom")

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + 
      theme(legend.background=element_rect(fill="green"))

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
      geom_point() + 
      theme(panel.grid.minor = element_line(colour = "red", 
                                            linetype = "dotted"))

ggplot(dati, aes(garums, lapas,colour = gaisma)) + 
   geom_point() + 
   theme(panel.background = element_rect(colour = "pink",
                                         size = 3))


ggplot(dati, aes(garums, lapas,colour = gaisma)) + geom_point()+
      labs(color="Gaismas daudzums")+
      theme(axis.text.y=element_text(size=rel(1.2),face="bold"),
            axis.text.x=element_text(size=rel(1.2),face="bold",angle=90,vjust=0.5),
            axis.title=element_text(size=rel(1.5),face="bold"),
            axis.line=element_line(color="black"),
            panel.background=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_line(color="grey90"),
            legend.position="top",
            legend.key=element_rect(fill="white"),
            legend.title=element_text(size=rel(1.5)),
            legend.text=element_text(size=rel(1.5)))

ggsave("1_attels.png",width = 8,height = 5)