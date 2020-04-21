#R code land cover
setwd("~/lab")
library(raster)
library(RStoolbox)
 p224r63_2011 <- brick("p224r63_2011_masked.grd")
 #rgb
 #landsat bands. 1b, 2g,3r, 4 nir
 plotRGB(p224r63_2011, r=4,  g=3, b=2, stretch="Lin")
 
 unsuperClass(p224r63_2011, nClasses=4)
 #classi di copertura suolo
 
 p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
 
 p224r63_2011c
 #$map: simbolo per unire vari pezzi insieme. uniamo la mappa alnostro modello che abbiamo chiamato p224r63_2011c
 
# andiamo a plottare
plot(p224r63_2011c$map)

#cambiamo i colori. legenda continua (in caso queso si ouò cambiare)
#colorRamppalette

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100)
plot(p224r63_2011c$map, col=clclass)


# in funzione del numero di classe aumenta l'incertezzadell' arloritmo automatic


 
 p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)





