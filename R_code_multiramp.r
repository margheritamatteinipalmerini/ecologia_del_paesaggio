setwd("~/lab/")
load("defor.RData")

#funzione ls()
ls()
#lista di file a disposizione
#d1c : classificazione immagine deforestazione con numero di classi=2- dc2 immagine dopo deforestazione

library(raster)
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# avevamo calcolato le frequenze delle foreste e dei terreni agricoli e valore di copertura prima e dopo deforestazione e e valore di percentuale prima e dopo dforestazione
output
output <- data.frame(cover,before,after)
output
# copertura agricoltura e foresta

#plottaggio di queste informazioni
# ggplot
library(ggplot2)
# colore in base alla copertura
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
dev.off() 

#esercizio: plot after deforstazione
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#serve un altro pacchetto a
install.packages("gridExtra")
library(gridExtra) #anche require(Extra)
#plot finale con tutti i dati all'interno
# funzione grid.arrange

#nominiamo il primo ggplot e poi l'altro
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#funzione per mettere i due grafici all'interno di una stessa finestra
grid.arrange(grafico1, grafico2, nrow = 1)
# analisi di paragoe fra deforestazione e agricoltura





