## copernicus global land 
# vegettion properties



## analisi cambiamenti climatici sulla copertura nevosa
#cryosphere: snow cover
#basic: data di inizio e fine
# downoad, cartella lab

setwd("~/lab/")
# setwd("/Users/utente/lab") #mac
#libreria ncdf4: dati coe estenzione nc

install.packages("ncdf4")
library(ncdf4)
library(raster)
#il pacchetto di primo potrebbe essere inserito in alcune versioni nel pachetto raster

##visualizzare il file nc: prima va importato
#raster(),: raster importa un singolo livello: la copertura della neve quel singolo giorno brick () : brick importa diverse bandesno
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# esecizio: plot snow color con cl
plot(snowmay, col=cl)

#18 maggio
## cosa ci aspettiamo: che nel tempo la copertura nevosa è diminuita. dati iol: snow.zip <- download in lab (nuova cartell DENTRO LAB COÌ POSSIAO importarli tutti insieme)


#### import snow data
setwd("~/lab/snow")
# setwd("/Users/utente/lab/snow") #mac
#lapply

#preniamo una lista di file e le associamo un nome (rlist): file all'interno della cartella
rlist <- list.files(pattern=".tif")
rlist 
#lapppy: applica dei comandi ad un intero set di dati
## funzione che ci interessa:raster
## creiamo uno stack: un tipo di oggetto all'iterno di R a cui daremo un nom
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

plot(snow.multitemp,col=cl)

# differenza netta fra anno 2000 e 2020
## previsione per anno 2025

 
#plot della prima immagine e dell'ultima
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# la secoda immagine non arriva a 250 nella legenda masolo a 200 : zlim
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff) #differenza pincipale fra i due set: sotratta imagine 2000 a quella 20202

# previsione multitemporale: 2025, quale sarà la copertura nevosa ?
#iol: prediction.r <- creare previsione futura
#lm: modello linere, x:rispetto linea del tempo... incognita che vogliamo avere come valore. time
# salva prediction.r nella snow in lab

# funzione per richiamare codice dall'esterno
source("prediction.r")
#richiamiamo il codice contenuto in prediction



predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
## importa file da iol

## plottaggio previsione

plot(predicted.snow.2025.norm, col=cl)

# queste prevsiioni, fatte sulla base di variazioni attuali, si chiamano scenari



