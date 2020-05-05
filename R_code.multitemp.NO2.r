## analisi NO2 da dati ESA, Gennaio-Marzo 2020

setwd("~/lab/")

#librerie che ci servono
library(raster)
#importiamo le singole immagini scaricate e nella cartella lab


EN01 <- raster("EN_00001.png")
plot(EN01)
dev.off()

# importare tutte le immagii
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

cl <- colorRampPalette(c('red','orange','yellow'))(100) 
cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN01, col=cl)  #inizio gennaio
plot(EN13, col=cl) # marzo

# operiamo una vera e propria statistica

    par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)
 
 #metodo più lungo
 
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

