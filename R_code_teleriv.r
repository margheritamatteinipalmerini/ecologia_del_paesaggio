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

