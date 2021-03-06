## codice per analisi dei point patterns

install.packages("ggplot2")
library(ggplot2)

install.packages("spatstat")
library(spatstat)

install.packages("rgdal")

setwd("~/lab")

#importare dati tabellari
covid <- read.table("covid_agg.csv", head=T)
#tabella covid_agg.csv in lab
#dobbiamo spiegare che c'è una intestazione (nomi variabil) 
head(covid)
#ogn paese è rappresntato da un punto ed ogni paese ha il proprio numero di casi
 
#primo plot per visualizzare come sono distribuiti i dati nel mondo
plot(covid$country,covid$cases)
#country:x
#y:numero casi
# $ collega una colonna al proprio dataset 
## in questo grafico il paese che ha più alto conteuo di casi è la Cina, ma non si vede sulla x. mettiamola sulla x
# la funzione è las
plot(covid$country,covid$cases,las=0)
# vediamo cosa è successo con 0: le etichette sono sempre paralelle all'asse
# proviamo con las=1
plot(covid$country,covid$cases,las=1)
# asse y cambiato: tutte le etichette sono orizzontali
plot(covid$country,covid$cases,las=2)
# las = 2 : lables perpedicolari al proprio asse
plot(covid$country,covid$cases,las=3)
#las = 3 : tutte le labels sono verticali (è quella che ci piace di più)
#diminuiamo la grandezza dei punti (cex) => una cosa simile cex.lab, per diminuire leatichette
plot(covid$country,covid$cases,las=3, cex.lab=0.5) 
#non è quello giusto 
plot(covid$country,covid$cases,las=3, cex.lab=0.5, cex.axis=0.5)
#ce.axis cambia la dimensione di tutti gli assi

#passiamo ad una visuaizzazione spaziale

##ggplot2 
install.packages("ggplot2")
library(ggplot2)
#andrebbbero messi all'inizio del codice, per informare di quali librerie ci serviremo.

data(mpg)
head(mpg)

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
# questo tipo viene utilizzato spesso per le variazioni ditemperatura
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()
# poco senso

#ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()
#covid: datast
#aes
names(covid)
head(covid)
# dimenione punti (size) in relazione numero casi
#geometria: punti

##esercizio : misurare densità ounti : quale pate del mondo ha una più alta densità di paesi che hanno avuto il coronavirus
#density
#ci serve un altro pacchetto, spatstat
# crear dataset per spatstat
#diamo un nome al dataset
library(spatstat)
attach(covid)
covidppp <- ppp(lon, lat, c(-180,180), c(-90,90))
d <- density(covids)
#le abbiamo dato un nome per utilizzarla meglio
#facciamo un plot della densità
plot(d)
#aggiungiamo due informazioni interessanti: origine e contorni paesi
points(covids, pch=19)

plot(d)
points(covids)

#aggiungiamo i contorni dei paesi
#database internazionale: natural hearth data

#salviamo l'R data compltamente, l'intero progetto, workspace, .rdata

#linux: 
q()
# invio, e yes
# dentro la cartella lab, dovrebbe esserci il file appeana salvato 

### day after

# ripartiamo da setwd("~lab")
setwd("~lab")
# load: andiamo a caricare pointpattern.Rdata
load(pointpattern.Rdata)
ls()
# ls() per vedere i file (di ieri)
library(spatstat)
#per visualizzare le immagini della densità
plot(d)
# cambiamo set di colori. invertiamo blu e gialli, hostpost di covid:rossi (color ramp palette)
cl <- colorRampPalette(c('yellow','orange','red'))(100) 
# c: simbolo per idicare una serie di cose, in questo caso di colori
# virgolette singole
# quanti minilivelli fra un livello all'altro .più sono meglio è (gradazioni) : (100)
## spatstat è una delle librerie che permette di utilizzare questa funzione
plot(d,col=cl)
# colore = alla color palette appena creata (cl)

# Exercizio: plot della densità dal verde al blu
points(covids,pch=19,cex=0.5)
#casi di covid
## confini stati : come caricare dati geografici dall'esterno
# diamo un nome ai file delle nazioni: coastline
coastline <- readOGR("ne_10m_coastline.shp")
#readOGR è parte della libraria gdal osgeo
##osgeo.org
# inseriamo la libreria rgdal
install.packages("rgdal")
library(rgdal)
coastline <- readOGR("ne_10m_coastline.shp")

# plot della mappa con aggiunta delle coastline
plot(coastline, add=T)
#T= true
## che cos'è questa mappa: rappresenta quanto densi sono i punti nel mondo.

# esercizio plot della mappa di densità con una nuova colorazione e aggiunta delle coastline

cl <- colorRampPalette(c('pink','purple','violet','dark violet'))(100) 
plot(d, col=cl)
plot(coastline, add=T, col="light blue")





