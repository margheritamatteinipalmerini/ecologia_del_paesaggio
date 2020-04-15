#### codice R per le analisi di immagini satellitari

#installare pacchetti che servono per questa analisi
install.packages("raster")
#pacchetto per leggere file raster

#library(raster)
#settaggio working directory
setwd("~/lab/")

#prendere un immagine dalla cartella e darle un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011)
#palette di colori

#save Rdata

##parte due

setwd("~/lab/")
#caricare dato di ieri
load("teleril.Rdata")
ls() 
#dovremmo avere l'immagine che abbiamo salvato ieri
##[1] "p224r63" "p224r63_2011"
# path and row (landstat)

#plottare questa immagine
plot(p224r63_2011)
#problem: non abbiamo caricato la libreria giusta , raster
library(raster)
plot(p224r63_2011)

#prima banda, B1: riflettante del blu
#B2: valori riflettante del verde
#rosso
#infrarosso vicino
#infrarosso medio
#infrarosso termico
#infrarosso medio (altra banda)

#a rosino, bianco verde... leggenda standard di R
cl <- colorRampPalette(c('black','grey','light grey'))(100)  # <- colorramppalette assegnata ad un oggetto chiamato cl
plot(p224r63_2011, col=cl) #cambiamo colore al plot, uguale alla colorRampPal# cambiamo colorazione:a scalare bianco-nero che riflettono i colori di riflettanza
ette appena creata
#infrarosso termico: valori molto alti, particolare
# qual'è il nome della banda del blu?
names(p224r63_2011)
#nomi di tutte le bande
#color ramppalette banda blu
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
#attach collegare un datagrame ... ma non può essere usato con raster
#usiamo un simbolo che lega la colonna ad un dataset(immagine satelliatare) : $
plot(p224r63_2011$B1_sre, col=clb) #se non aggiungiamo la color ramppalette, plotta ancora nelle tonalità di R
#parte bianca: eliminata, pixel bianchi

#exercise : plottare la banda dell'infrarosso vicino con colorramp palette che varia dal rosso, arancione, giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

#multiframe con 4 bande (blu, rosso, verde, infrarosso vicino)
par(mfrow=c(2,2)) #row:vicino
#par: a blocchi questa finestra. 2 immagini sopra e sotto
#prima immagine: blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# tutto su R

#verde - green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg)

# red#
clr <- colorRampPalette(c('dark red','red','pink'))(100) #non esiste il light red
plot(p224r63_2011$B3_sre, col=clr)

#near infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

#comando che chiude la finestra dei grafici
dev.off()

#natural colours
#3 componenti nella grafica del computer : R G B : montarle alle bande corrispondenti
# 3 bande per voltaR= red, G= gree, B= blue
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
# lin : per non avere tutto nero

# "false" un colore che l'occhio umanon non può vedere
# falsato, scalati di uno. B=green (2). 
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin") #nir componenente rossa
dev.off()

par(mfrow=c(1,2))
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
# grafico con immagine a colori naturali e poi falsati

par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
dev.off()

# esercizio: nir componente verde
plotRGB(p224r63_2011,r=3,g=4,b=2, stretch="Lin")
#piante  foresta: fluorescei. coltivi: blocchi viola
plotRGB(p224r63_2011,r=3,g=2,b=4, stretch="Lin")

#setwd("~/lab/")
load("teleriv.Rdata")

#lista dati scorsi
ls()

p224_1988 <- brick ("p224r63_1988_masked.grd")

#facciamo plot dell'immagine
plot(p224r63_1988)              
                 

#multiframe con 4 bande (blu, rosso, verde, infrarosso vicino)
par(mfrow=c(2,2)) #row:vicino
#par: a blocchi questa finestra. 2 immagini sopra e sotto
#prima immagine: blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)
# tutto su R

#verde - green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)

# red#
clr <- colorRampPalette(c('dark red','red','pink'))(100) #non esiste il light red
plot(p224r63_1988$B3_sre, col=clr)

#near infrarosso, nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir)

#comando che chiude la finestra dei grafici
dev.off()

# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
## bande sensore del satellite

#plot RGB con le bande 

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")


#esercizio: plot the imagine using the nir on the "r" component in the RGB
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")

#plot delle due immagini, 1988 e 2011
##par
#mf: multirme
par(mlrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin", main="2011")
# diamo un titolo ai singoli garfici: main

#spectral indices 
#dvi(different vegetation index) 1988 = nir1988 - red1988
dvi(1988) <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
#se la pianta è sana il valore sarà molto alto
plot(dvi1988)
#stessa cosa dvi 2011
dvi(2011) <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)

cldvi<- colorRampPalette(c('light blue','light green','green'))(100)

plot(dvi2011 col=cldvi)
#meglio colori più distanti


#differenza nel tempo fra i due indici 

## multitemporal analysis

difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi<- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdiv)

# par multiframe per vedere tutte  e 3 immagini insieme: 88RGB 2011RGB differenza dvi




par(mlrow=c(3,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plot(difdvi, col=cldifdiv)
dev.off()

#  changing the grain (resolution)
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
# fact: fattore, #aggregate : funzione per aggregare un immagine
# se ho un pixel di 30 metri, aggiungo factor 10,avrò un pixel di 300 metri
#lr : low 

par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
plot(div201050)
dev.off()

#dvi1988 low resolution
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi188lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

#differenza dvilr50
difdvilr50 <- dvi2011lr50 - dvi1988lr50


