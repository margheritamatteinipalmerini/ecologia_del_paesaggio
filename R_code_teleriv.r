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
