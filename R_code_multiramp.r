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

load("EN.RData")grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#funzione per mettere i due grafici all'interno di una stessa finestra
grid.arrange(grafico1, grafico2, nrow = 1)
# analisi di paragoe fra deforestazione e agricoltura

## parte 2

library(ggplot2) 

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)
output <- data.frame(cover,before,after)
output

library(gridExtra) # oppure: require(Extra)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow = 1)


grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

# Exercise: use grid.arrange to plot the two graphs 
grid.arrange(grafico1, grafico2, nrow = 1)

## confronto grafici: agricoltura aumnetata mentre la foresta diminusice

# importare tante immagini in una volta 

setwd
("~/lab/")

load("EN.RData")
ls()

rlist <- list.files(pattern=".png")

# lista dei file che hanno una certa configrazione simile (pattern)
## png

rlist
# associamo rlist ala funzione rater e le diamo un nome
listafinale <- lapply(rlist, raster)


EN <- stack(listafinale)

# non resta che fare il grande plot finake
## stessa color ramp palette
cl <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(EN, col=cl)

# c'è stato un cambiaento nelle percentuale di ossidi di azoto in Europa?
setwd("~/lab/esa_no2")
library(raster)
rlist <- list.files(pattern=".png")
 

listafinale <- lapply(rlist, raster) #nome:lista finale
# lapply: applica usa funzione ad una lista. applichiamo la funzione raster ala lista di file (rlist)

# suR
listafinale
EN <- stack(listafinale)
# a questo punto abbiamo fatto la nostra lista di file

EN
# EN digitato su R: abiam un RasterStack con i nmi all'inerno (EN002, ENoo3...=

# differenza immagino fra gennaio e marco, importando stack

difEN <- EN$EN_0013 - EN$EN00_1

# creiamo una colorramp palette prima di plottare
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)
# zone diminuito: diferenze alte, rosso
# contrario: blu

# plot intero set, più informazioni
cl <- colorRampPalette(c('red','orange','yellow'))(100) 
plot(EN, col=cl)

## come è variato l'azoto? (indagine stastica)
boxplot(EN)
# mettiamolo orizzontalmente
boxplot(EN, horizontal=T)
# rimuoviamo dati outliner
boxplot(EN, horizontal=T, outline=F)
# assi
boxplot(EN, horizontal=T, outline=F, axes=T)

# cabiamento: solo sui valori alti (destra)








