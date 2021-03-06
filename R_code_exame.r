#CODICI PER ESAME
# R_code_exam.r

# Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

# 1. R_code_first.r	
# 2. R_code_spatial.r	
# 3. R_code_spatial2.r
# 4. R_code_point_pattern	
# 5. R_code_teleril.r	
# 6. R_code_landcover.r	
# 7. R_code_multitemp.r	
# 8. R_code_multitemp_NO2.r	
# 9. R_code_snow.r	
# 10. R_code_patches.r	
# 11. R_code_crop.r -  exam simulation
# 12. R_code_ Species Distribution modelling


### PRIMO CODICE 
### PAIRS AND PLOT

# INSTALLIAMO IL PRIMO PACCHETTO: sp. USIAMO LE VERGOLETTE PERCHÈ STIAMO ANDANDO A CHIAMARE QUALCOSA AL DI FUORI DEL NOSTRO R
# USIAMO LA FUNZIONE install.packages("sp")
install.packages("sp")

#UNA VOLTA INSTALLATO, RICHIAMIAMO IL PACCHETTO CON LA FUNZIONE library(). IN CASO, UN ALTRO COMANDO PER FARE PARTIRE LE LIBRERIE È require().
library(sp)

# CON LA FUNZIONE data POSSIAMO VEDERE I DATI A DISPOSIZIONE IN QUESTA LIBRERIA, MENTRE meuse INDICA IL DATASET DI RIFERIMENTO
data(meuse)
meuse # PER VEDERE I DATI


head(meuse)
# IN QUESTO MODO NON SOLO VEDIAMO GLI ELEMENTI DEL DATASET, MA ANCHE LE COORDINATE AD ESSI ASSOCIATE

names(meuse)

summary(meuse) 
# SUMMARY CI PERMETTE DI VEDERE I PRINCIPALI INDICI STATISTICI

# LA FUNZIONE pairs CI FA AVERE UNA MATRICE DI DIAGRAMMI
pairs(meuse) # CREAZIONE PRIMO GRAFICO

pairs(~ cadmium + copper + lead , data = meuse)
#APPLICHIAMO LA FUNZIONE PAIRS AL CADMIO, RAME E PIOMBO

# Exercie: cadmium copper lead zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)

# [,3:6] I NUMERI FANNO RIFERIMENTO ALLE RIGHE 3 4 5 6, APPUNTO CADMIO, RAME, PIOMBO E ZINCO
pairs(meuse[,3:6])
pairs(meuse[,3:6], col="purple")
pairs(meuse[,3:6], col="purple", pch=19)
pairs(meuse[,3:6], col="purple", pch=19, cex=3)
pairs(meuse[,3:6], col="purple", pch=19, cex=3, main="Primo pairs")
# COL= FUNZIONE PER CAMBIARE COLORE, PCH= FUNZIONE PER CAMBIARE CARATTERE, CEX= FUNZIONE PER CAMBIARE DIMENSIONE
# MAIN: FUNzIONE PER DARE UN NOME A QUESTO GRAFICO
## LA VIRGOLA SI USA COME SEPARATORE PER INSERIRE PIÙ FUNZIONI.

# Exercise: do the same for the relationship between elevation and the elements

pairs(meuse[,3:7], col="purple", pch=19, cex=3, main="Primo pairs")


# PRENDIAMO LE SEGUENTI FUNZIONE ESTERNE
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}


panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}


panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


# MANDANDO QUESTE 3 FUNZIONI ESTERNE, POTREMO AVERE LA POSSIBILITÀ DI CREARE DEI GRAFICI NELLO STESSO GRAFICO PER RAPPRESENTARE SITUAZIONI DIVERSE
pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# EXERCISE: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni

pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)

# RICHIAMIAMO IL SET DI DATI " MEUSE"
library(sp)
data(meuse)
head(meuse)

#FUNZIONE PLOT: CREAZIONE DI UN GRAFICO. 
plot(meuse$cadmium,meuse$copper)
#$: QUALE COLONNA DEL DATASET

attach(meuse) 
# ATTACH SI USA PER METTERE IN EVIDEZIA IL PACCHETTO DI DATI: IN QUESTO MODO POSSIAMO EVITARE DI SCRIVERE meuse$ MA SCRIVERE SOLO IL NOME DEI DELLE COLONNE

#VIA COL PLOT!
plot(cadmium,copper)
# COLORI E FORME DI DEFAULT: CAMBIAMOLE!
plot(cadmium,copper,pch=17,col="pink",main="Primo plot")
plot(cadmium,copper,pch=17,col="pink",main="Primo plot",xlab="cadmio",ylab="copper")

#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### SECONDO CODICE
### R SPAZIALE!

############ R spatial: scopriamo le funzioni spaziali di R 

install.packages("GGally")
install.packages("sp")  
# MMP chiamiamo un paccheto dall'esterno, quindi virgolette
library(sp) 
# MMP serve per spiegare al software quale pacchetto richiamare
data(meuse)
# MMP data: funzione per i dati
# MMP meuse: dataset
head(meuse)
#MMP in questo modo vediamo direttamente non solo gli elementi ma anche le coordinate che utilizzeremo ora
# esercizi: plot del rapporto fra cadmio e piombo
# MMP alla fine faremo un multipannello in cui compaiono più plot
# MMP alleghiamo il dataframe 
attach(meuse)
plot(cadmium,lead,col="purple",pch=19,cex=2)
#MMP virgola: separatore
#MMP col: cambio colore dei punti come più mi piace
#MMP dentro R la funzione per cambiare carattere è pch
#MMP carattere exateration... aumentare (>1) diminuire (<1)
#MMP dovrebbe uscire un grafico
#MMP exercise: plot di copper e zinco con  caratteree (simbolo) trinagolo e colore verde
#MMP triangolo pch=17
plot(copper,zinc,col="green",pch=17,cex=2)
#MMP 2: quindi il doppio 
#MMP solitamnete le vergolette si mettono su un testo: avviso che ci troviamo di fronte ad un testo, ma anche quando facciamo dialogare  il nostro software con qualcosa di esterno.
#MMP xlab: cambio etichetta
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab"zinco")
#MMP multiframe (mf) o multipanel: funzione importantissima per mettere più di un grafico all'interno di una finestra
#MMP par: funzioe per tutti i pannelli, quindi anche tutti i grafici. numeri di righe e colonne
#MMP numeri in fila: vettori, c davanti
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
# MMP copiare tutto il blocco (le 3 righe sopra), incollare in R e premere invio
# MMP invertiamo riga/colonna colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#MMP multipannelo con pairs --> esiste un pacchetto che può aiuare
#MMP multiframe automatico: andiamo ad installare 
install.packages("GGally") ##MMP tutti i pacchetti da installare per rieseguire un codice andrebbero messi tutti nelle prime righe
#MMP virgolette perchè dall'esterno
library(GGally) #MMP richiamiamolo
#MMP pairs:multipannello di multivariabili
ggpairs(meuse) #MMP ci vorrà un sacco, subset []
ggpairs(meuse[,3:6])
# MMP , vuol dire "partiamo da"
#MMP : "fino a"
#MMP grafico: che cos'è? sulla parte diagonale vediamo le singole varibiali e la distribuzione dei dati
#MMP plot delle variabili in relazione fra loro (in modo simetrico)
#MMP correlazioni
#MMP coeficente fra -1 e 1

####MMP  spatial!

head(meuse)

#MMP coordinate x e y come in tutti i grafici
#MMP diciamo al software che ci sono delle coordinate:
coordinates(meuse)
### MMP non incolliamola subito su R: è particolare
coordinates(meuse)=~x+y
#MMP in R,per raggruppare dei pezzi usiamo la tilde (#Linux: Alt Gr + ì)
#MMP adesso possiamo incollarlo su R
plot(meuse)
#MMP grafico
spplot(meuse,"zinc")
#MMP spplot: funzione per plottare i dati spazialmente
#MMP all'interno della funzion spplot le variabili vanno scitte con le virgolette perchè vengono prese dall'esterno

############################
################### PARTE DUE
#MMP  richiamiamo la libreria sp e i dati da usare
library(sp)
data(meuse)
# MMP coordinate xy, ascisse e ordinate, del dataset
# MMP head serve per dare un occhiata veloce al dataset! (memo!)
coordinates(meuse)=~x+y
#MMP ATTENZIONE: quando si inserisce una funzione su R e non vediamo risposta, vuol dire che tutto è andato bene!
spplot(meuse,"zinc")
#MMP sp (il pacchetto) + plot  in questa funzione le variabili vanno indicate fra virgolette
#DEVE USCIRE UNA IMMAGINE

#esercizio: spplot dei dati del rame
#MMP per vedere come si chiama il rame: head(meuse) : xy in coordinate 
head(meuse)
#MMP oppure: per vedere SOLO i nomi delle colonne
names(meuse)
#MMP quindi:
spplot(meuse,"copper")
#MMP avevamo visto che erano coordinati rame e zinco, infatti anche il primo presenta valori decrescenti in base alla lontananza dal fiume

#MMP stesso plottaggio,altra funzione di sp : bubble
bubble(meuse,"zinc")
#MMP grafico: stessa rapresentazione di prima, ma invece che dei colori che si relazionano ai diversi valori, qui vengono relazionati a delle bolle (bubble) in base ad alte o basse concentraione
# MMP conferma le concentrazioni dell'altro grafico
# MMP questo tipo di grafico, infografico, forse è più immediato ed elegante

#MMP esercizio: funzione bubble del rame colorato di rosso
bubble(meuse,"copper",col="red")
#MMP red va scitto fra virgolette perchè fa riferimento a valori numerici che indicano come viene fatto il colore rosso

#MMP esempio con dati inventati
# foraminiferi (Sofia), carbon capture (Marco)
10, 20, 35, 55, 67, 80
#MMP in R, questa serie di numeri è un vettore (array)
foram <- c(10, 20, 35, 55, 67, 80)
#MMP abbiamo dato un nome al dataset : abbiamo creato un oggetto
carbon <- c(5, 15, 30, 70, 85, 99)
#MMP abbiamo dato nome anche all'altra serie di numeri (di Marco)
# MMP inseriamoli su R
#MMP plottiamo i due dati per vedere se sono relazionati fra loro
plot(foram, carbon, col="green", cex=2, pch=19)
#MMP stretta relazione fra il carbonio intrappolato dai foraminiferi e la densità di questi ultimi


#### MMP scarica tabella da iol, covid_agg.csv
#MMP crea una nuova cartella in home: percorso più corto che si può fare!! "lab"

# MMP dati dall'esterno: covid19
#MMP specificare la cartella al software con cui utilizziamo
#MMP cartella appena create 
#MMP setwd("~/lab")
setwd("~/lab")
covid <- read.table("covid_agg.csv",head=TRUE)
#MMP agg : dati aggregati
#MMP dobbiamo spiegare a r che ci sono dei titoli
#MMP TRUE: variabili 1-0, vero falso. Poteva essere anche solo T
#MMP relazionato al nome covid
head(covid)



#############################################################################################################################################################################################
#############################################################################################################################à
############################################################################################################################
##########################################################################################################################
#########################################################################################################################
### QUARTO CODICE
### R POINT PATTERN!

## MMP codice per analisi dei point patterns
### TUTTI I PACCHETTI CHE CI SERVIRANNO PER QUESTO CODICE: 
install.packages("ggplot2")
library(ggplot2)

install.packages("spatstat")
library(spatstat)

install.packages("rgdal")

setwd("~/lab")

#MMP importare dati tabellari
covid <- read.table("covid_agg.csv", head=T)
#MMP tabella covid_agg.csv in lab
#MMP dobbiamo spiegare che c'è una intestazione (nomi variabil) 
head(covid)
#MMP ogni paese è rappresntato da un punto ed ogni paese ha il proprio numero di casi
 
#MMP primo plot per visualizzare come sono distribuiti i dati nel mondo
plot(covid$country,covid$cases)
#MMP country:x - y:numero casi
# MMP $ collega una colonna al proprio dataset 
##MMP  in questo grafico il paese che ha più alto conteggio di casi è la Cina, ma non si vede sulla x. mettiamola sulla x
# MMP la funzione è las
plot(covid$country,covid$cases,las=0)
#MMP vediamo cosa è successo con 0: le etichette sono sempre paralelle all'asse
#MMP proviamo con las=1
plot(covid$country,covid$cases,las=1)
# MMP asse y cambiato: tutte le etichette sono orizzontali
plot(covid$country,covid$cases,las=2)
#MMP las = 2 : lables perpedicolari al proprio asse
plot(covid$country,covid$cases,las=3)
#MMP las = 3 : tutte le labels sono verticali (utilizziamo questa)
#MMP diminuiamo la grandezza dei punti (cex) => una cosa simile cex.lab, per diminuire le etichette
plot(covid$country,covid$cases,las=3, cex.lab=0.5) 
#MMP non è quello giusto 
plot(covid$country,covid$cases,las=3, cex.lab=0.5, cex.axis=0.5)
#MMP ce.axis cambia la dimensione di tutti gli assi

## MMP passiamo ad una visuaizzazione spaziale

##MMP ggplot2 
# MMP pacchetto per creare grafici superpersonabilizzabili 
# MMP in questo pacchetto bisogna stare attenti a specificare le componenti
install.packages("ggplot2")
library(ggplot2)
#MMP andrebbbero messi all'inizio del codice, per informare di quali librerie ci serviremo. 

data(mpg) #MMP  mpg: data
head(mpg)

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point() #MMP aes: aestetic
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()  ## MMP nelle parentesi inseriamo le coordinate
# MMP questo tipo viene utilizzato spesso per le variazioni di temperatura
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()
#MMP varie geometrie

#MMP ggplot per covid, richiamiamo prima i nomi con names(covid); IN QUESTO CASO SI RIPETE UTILIZZANDO I DATI RELATIVI ALLA TABELLA COVID
#MMP CON L'AGGIUNTA DELL'ARGOMENTO size= PER ATTRIBUIRE LA DIMENSIONI AI PUNTI SUL GRAFICO, CHE RAPPRESENTANO IL NUMERO DI CASI.

names(covid)

ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()
#MMP covid: datast
#MMP aes
names(covid)
head(covid)
#MMP dimensione punti (size) in relazione numero casi
#MMP geometria: punti

##esercizio : misurare densità punti : quale pate del mondo ha una più alta densità di paesi che hanno avuto il coronavirus?
#MMP density
#MMP ci serve un altro pacchetto, spatstat
# MMP crear dataset per spatstat
#MMP diamo un nome al dataset
library(spatstat)
attach(covid)
covidppp <- ppp(lon, lat, c(-180,180), c(-90,90)) # MMP ppp: questo comando permette di convertire i dati in point pattern
d <- density(covidppp)
#MMP le abbiamo dato un nome per utilizzarla meglio (abbiamo creato un oggetto)
#MMP facciamo un plot della densità
plot(d)
#MMP aggiungiamo due informazioni interessanti: origine e contorni paesi
points(covidppp, pch=19)

plot(d)
points(covidppp)

#MMP aggiungiamo i contorni dei paesi
#MMPdatabase internazionale: natural hearth data

#MMP salviamo l'R data completamente, l'intero progetto, workspace, .rdata 
q() #PER LINUX
#MMP invio, e yes
#MMP dentro la cartella lab, dovrebbe esserci il file appeana salvato 

### SECONDA PARTE (GIORNO SEGUENTE)

# MMP ripartiamo da setwd("~lab")
setwd("~lab")
# MMP load: andiamo a caricare pointpattern.Rdata
load(pointpattern.Rdata)
ls()
#MMP ls() per vedere i file (di ieri)
library(spatstat)
#MMP per visualizzare le immagini della densità
plot(d)
# MMP cambiamo set di colori. invertiamo blu e gialli, hostpost di covid:rossi (color ramp palette)
 cl <- colorRampPalette(c('yellow','orange','red')) (100)
# MMP c: simbolo per idicare una serie di cose, in questo caso di colori
###MMP virgolette singole
#MMP quanti minilivelli fra un livello all'altro, più sono meglio è (gradazioni) : (100)
## MMP spatstat è una delle librerie che permette di utilizzare questa funzione
plot(d,col=cl)
# MMP colore = alla color palette appena creata (cl)

# Exercizio: plot della densità dal verde al blu
points(covidppp,pch=19,cex=0.5)
#MMP casi di covid
##MMP confini stati : come caricare dati geografici dall'esterno
# MMP diamo un nome ai file delle nazioni: coastline
coastline <- readOGR("ne_10m_coastline.shp")
#MMP readOGR è parte della libraria gdal osgeo
##osgeo.org
#MMP inseriamo la libreria rgdal
install.packages("rgdal")
library(rgdal)
coastline <- readOGR("ne_10m_coastline.shp")

# MMP plot della mappa con aggiunta delle coastline
plot(coastline, add=T)
#MMP T= true
##MMP che cos'è questa mappa: rappresenta quanto densi sono i punti nel mondo.

# MMP esercizio plot della mappa di densità con una nuova colorazione e aggiunta delle coastline

cl <- colorRampPalette(c('pink','purple','violet','dark violet'))(100) 
plot(d, col=cl)
plot(coastline, add=T, col="light blue")

################### INTERPOLAZIONE

setwd("~/lab")

# MMP caricare il file RData che useremo
load("sanmarino.RData")

#MMP visualiziamo i dati 
ls()

# MMP dT è la density map
# MMP Tesi è un dataset che si trovava all'interno di "Tesi.RData"
# MMP Tesippp è il point pattern:  coordinate della tabella originale 
# MMP la density map siamo riusciti a farla a partire da Tesippp

# MMP carichiamo la libreria
library(spatstat)
# MMP visualizziamo la densità di campionamento
points(Tesippp, col="green")
 
head(Tesi)

#MMP  la funzione marks() associa i valori della variabile che vogliamo interpolare al point pattern (punti spaziali)
marks(Tesippp)<-Tesi$Species_richness
# MMP $ indica la colonna del dataset da considerare

#MMP possiamo procedere con l'interpolazione: la stima. Creeremo una mappa continua partendo da valori discreti

#MMP smooth stima i valori dove questi non sono stati misurati
interpol<-Smooth(Tesippp)

# MMP mappa
plot(interpol)
points(Tesippp,col="blue")
# MMP maggiore ricchezza nella parte Sud-Est e nella parte centrale


setwd("~/lab")
# MMP : libreria rgdal:visualizzare tutti i file di tipo vettoriale
library(rgdal)
# MMP possiamo leggere il file .shp
sanmarino<-readOGR("San_Marino.shp")
# PLOT
plot(sanmarino) #visualizziamo il territorio di San Marino
plot(interpol,add=T) #add=T sovrappone la mappa dell'interpolazione alla mappa di San Marino
points(Tesippp,add=T) #sovrappone i punti alle mappe di prima
plot(sanmarino,add=T) #per far vedere di nuovo i confini di San Marino


## Exercise: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")

# esercizio: due colonne e una riga
par(mfrow=c(1,2))

plot(dT,main="Density of points")
points(Tesippp,col="blue")

plot(interpol,main="Estimate of species richness")
points(Tesippp,col="blue")

#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### QUINTO CODICE
### R TELERIV!
#### codice R per le analisi di immagini satellitari

# MMP installare pacchetti che servono per questa analisi
install.packages("raster")
# MMP pacchetto per leggere file raster

# MMP library(raster)
# MMP settaggio working directory
setwd("~/lab/")

# MMP prendere un immagine dalla cartella e darle un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")
plot(p224r63_2011)
# MMP palette di colori

# MMP save Rdata

## MMP parte due

setwd("~/lab/")
# MMP caricare dato di ieri
load("teleril.Rdata")
ls() 
# MMP dovremmo avere l'immagine che abbiamo salvato ieri
##[1] "p224r63" "p224r63_2011"
# MMP path and row (landstat)

# MMP plottare questa immagine
plot(p224r63_2011)
# MMP problema: non abbiamo caricato la libreria giusta,raster
library(raster)
plot(p224r63_2011)

# MMP prima banda, B1: riflettante del blu
# MMP B2: valori riflettante del verde
# MMP B3 rosso
# MMP infrarosso vicino
# MMP infrarosso medio
# MMP infrarosso termico
# MMP infrarosso medio (altra banda)

#  MMP da rosino, bianco verde... legenda standard di R
cl <- colorRampPalette(c('black','grey','light grey'))(100)  # MMP <- colorramppalette assegnata ad un oggetto chiamato cl
plot(p224r63_2011, col=cl) # MMP cambiamo colore al plot, uguale alla colorRampPalette cambiamo colorazione:a scalare bianco-nero che riflettono i colori di riflettanza naturali
#  MMP palette appena creata
# MMP infrarosso termico: valori molto alti, particolare
#  MMP qual'è il nome della banda del blu?
names(p224r63_2011)
# MMP nomi di tutte le bande
# MMP color ramppalette banda blu
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
# MMP attach si usa per collegare un datagrame ... ma non può essere usato con raster
# MMP usiamo un simbolo che lega la colonna ad un dataset(immagine satelliatare) : $
plot(p224r63_2011$B1_sre, col=clb) # MMP se non aggiungiamo la color ramppalette, plotta ancora nelle tonalità di R
# MMP parte bianca: eliminata, pixel bianchi

#exercise : plottare la banda dell'infrarosso vicino con colorramp palette che varia dal rosso, arancione, giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# MMP multiframe con 4 bande (blu, rosso, verde, infrarosso vicino)
par(mfrow=c(2,2)) # MMP row:vicino
# MMP par: più grafici in una finestra. 2 immagini sopra e sotto (2 righe e 2 colonne: 2,2)
# MMP prima immagine: blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
#  MMP tutto su R = 
par(mfrow=c(2,2))
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

# MMP verde - green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg)

# MMP red
clr <- colorRampPalette(c('dark red','red','pink'))(100) #non esiste il light red
plot(p224r63_2011$B3_sre, col=clr)

#MMP near infrarosso
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

#MMP comando che chiude la finestra dei grafici
dev.off()

#MMP natural colours
#MMP 3 componenti nella grafica del computer : R G B : montarle alle bande corrispondenti
# MMP 3 bande per volta : R= red, G= gree, B= blue
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
#MMP lin serve per non avere tutto nero

#MMP "false" un colore che l'occhio umano non può vedere
#MMP falsato, scalati di uno. B=green (2). 
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin") #MMP nir componenente rossa
dev.off()

par(mfrow=c(1,2))
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
#MMP grafico con immagine a colori naturali e poi falsati

par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
dev.off()

# esercizio: nir componente verde
plotRGB(p224r63_2011,r=3,g=4,b=2, stretch="Lin")
#piante  foresta: fluorescei. coltivi: blocchi viola
plotRGB(p224r63_2011,r=3,g=2,b=4, stretch="Lin")

#MMP setwd("~/lab/")
load("teleriv.Rdata")

#MMP lista dati scorsi
ls()

p224_1988 <- brick ("p224r63_1988_masked.grd")

#MMP facciamo plot dell'immagine
plot(p224r63_1988)              
                 

#MMP multiframe con 4 bande (blu, rosso, verde, infrarosso vicino)
par(mfrow=c(2,2)) #MMP row:vicino
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)
# tutto su R

#MMP verde - green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)

# MMPre
clr <- colorRampPalette(c('dark red','red','pink'))(100) #MMP non esiste il light red
plot(p224r63_1988$B3_sre, col=clr)

#MMP near infrarosso, nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir)

#MMP comando che chiude la finestra dei grafici
dev.off()

# MMP B1: blue - 1
# MMP B2: green - 2
# MMP B3: red - 3
# MMP B4: near infrared (nir) - 4
##MMP  bande sensore del satellite

#MMP plot RGB con le bande 

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")


#esercizio: plot the imagine using the nir on the "r" component in the RGB
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")

#MMP plot delle due immagini, 1988 e 2011
##MMP par
#MMP mf: multirme
par(mlrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin", main="2011")
#MMP  diamo un titolo ai singoli garfici: main

#MMP spectral indices 
#MMP dvi(different vegetation index) 1988 = nir1988 - red1988
dvi(1988) <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
#MMP se la pianta è sana il valore sarà molto alto
plot(dvi1988)
#MMP stessa cosa dvi 2011
dvi(2011) <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)

cldvi<- colorRampPalette(c('light blue','light green','green'))(100)

plot(dvi2011 col=cldvi)
#MMP meglio colori più distanti


#MMP differenza nel tempo fra i due indici 

##MMP  multitemporal analysis

difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi<- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdiv)

#MMP par multiframe per vedere tutte  e 3 immagini insieme: 88RGB 2011RGB differenza dvi




par(mlrow=c(3,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plot(difdvi, col=cldifdiv)
dev.off()

# MMP changing the grain (resolution)
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
# MMP fact: fattore, #aggregate : funzione per aggregare un immagine
# MMP se ho un pixel di 30 metri, aggiungo factor 10,avrò un pixel di 300 metri
#MMP lr : low 

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

#MMP dvi1988 low resolution
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi188lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

#MMP differenza dvilr50
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

#MMP multiframe finale con risultati ottenuti
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)


#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### SESTO CODICE
### R LAND COVER!
#### codice R per le analisi di immagini satellitari

#R code land cover
setwd("~/lab")
library(raster)
library(RStoolbox)
 p224r63_2011 <- brick("p224r63_2011_masked.grd")
 # MMP R G B 
 #MMP landsat bands. 1b, 2g,3r, 4 nir
 plotRGB(p224r63_2011, r=4,  g=3, b=2, stretch="Lin")
 
 unsuperClass(p224r63_2011, nClasses=4)
 #MMP classi di copertura suolo
 
 p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
 
 p224r63_2011c
 #MMP $map: simbolo per unire vari pezzi insieme: uniamo la mappa al nostro modello che abbiamo chiamato p224r63_2011c
 
#MMP andiamo a plottare
plot(p224r63_2011c$map)

#MMP cambiamo i colori. legenda continua (in questo caso si può cambiare)
#MMP colorRamppalette

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100)
plot(p224r63_2011c$map, col=clclass)


# MMP in funzione del numero di classe aumenta l'incertezza dell'algoritmo automatico

 p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### SETTIMO CODICE
### R MULTITEMP!
# R ANALISI MULTITEMPORALE DELLA LAND COVER

install.packages("gridExtra")
setwd("~/lab/")

# MMP richiamiamo la libreria raster
library(raster)

# MMP brick() è una funzione di raster che permette di caricare dei dati dall'esterno, caricando tutte le singole bande se si stratta di una immagine satellitare
defor1<-brick("defor1_.jpg")
defor2<-brick("defor2_.jpg")

defor1 #  MMP nel dataet abbiamo 3 bande
# MMP names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

# MMP plottaggio RGB, associamo ogni singola banda ad una componente rgb
# MMP banda del rosso alla componente NIR, banda del green alla componente red, banda del blu alla componente green

plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

# Exercize plot della seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# MMP plottiamo le due immagini, confronto della foreste pluviale in due momenti diversi, prima e dopo la deforestazione
par(mfrow=c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

dev.off()

#  MMP per la classificazione si usa unsuperClass che significa classificazione non supervisionata, cioè non diamo dei training set al pc
#  MMP per far ciò dobbiamo caricare un'atra libreria
library(RStoolbox)
d1c<-unsuperClass(defor1,nClasses=2)
#  MMP in d1c abbiamo la mappa
d1c$map

plot(d1c$map)

cl1<-colorRampPalette(c('green','blue'))(100)
plot(d1c$map,col=cl1)

# Excersize: classificare con due classi l'immagine satellitare defor2
#  MMP consideriamo adesso la seconda immagine
d2c<-unsuperClass(defor2,nClasses=2)
#  MMP in d1c abbiamo la mappa
d2c$map
plot(d2c$map)

cl2<-colorRampPalette(c('green','blue'))(100)
plot(d2c$map,col=cl2)

# MMP  confrontiamo le due immagini
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)

par(mfrow=c(1,2))
plot(d1c$map, col=cl1)
plot(d2c$map, col=cl2)

dev.off()

###########
#  MMP classificazione con tre classi dell'immagine satellitare defor2
#  MMP consideriamo adesso la seconda immagine
d2c<-unsuperClass(defor2,nClasses=3)
# MMP in d1c abbiamo la mappa
d2c$map

plot(d2c$map)
cl3<-colorRampPalette(c('orange','green','blue'))(100)
plot(d2c$map,col=cl3)
###########


#  MMP quatificare la quantità di foresta che è stata persa
# MMP  area aperta = 306059
#  MMP foresta = 35233

#  MMP calcoliamo dapprima il totale
totd1<- 306059 + 35233
#  MMP totd1 = 341292
#MMP  possiamo calcolare la percentuale
percent1<-freq(d1c$map)*100/totd1
### MMP percentuali
# MMP foreste = 89.7
# MMP aree aperte = 10.3

# MMP defor2
freq(d2c$map)
#MMP foreste = 179087
# MMP aree aperte = 163639

totd2<-179087 + 163639
# MMP totd2 = 342726
# MMP percentuale
percent2<-freq(d2c$map)*100/totd2
# MMP foreste = 52.2
#MMP  aree aperte = 47.8


#MMP creiamo un nuovo dataset con i dati ricavati
cover <- c("Agriculture","Forest")
before<-c(10.3,89.7)
after<-c(47.8,52.2)
output <- data.frame(cover,before,after)
View(output)


##### PARTE DUE!!

# MMP riapriamo il file dell'utlima lezione
setwd("~/lab/")
load("multitemp.RData")
ls()

# MMP  rivediamo le immagini a confronto della foresta prima e dopo il disboscamento
library(raster)
par(mfrow=c(2,1))
plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)
dev.off()

# MMP  controlLiamo il dataframe contenete l'agricoltura e la foresta prima e dopo il disboscamento
output


# MMP  possiamo quindi fare un plot della percentuale di foresta attuale e precedente

#MMP   plot della percentuale precedente
# MMP carichiamo la libreria ggplot2
library(ggplot2)
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")
grafico1
# MMP  faremo degli istogrammi del dataframe di output
# MMP  aes: sulla x agricoltura e foresta, sulla y la percentuale di copertura prima della deforestazione
#MMP  Il colore si baserà sulla cover (agricoltura e foresta)
# MMP stat sono le statistiche che considera, in questo caso le identità
#MMP  fill dà il colore alla copertura

## Exercize: plot the histograms of the land cover after deforestation
grafico22<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")
grafico2

#MMP   possiamo fare un plot dei due istogrammi per confrontarli
# MMP  dobbiamo però usare un'altra libreria, la gridExtra 
install.packages("gridExtra")
library(gridExtra)

# MMP possiamo quindi procedere a fare un plot con la funzione grid.arrange. La funzione prende vari plot e li mette insieme all'interno di uno stesso grafico (funzione simile a par)
grid.arrange(grafico1,grafico2,nrow=1)
# MMP la percentuale di agricoltura relativa a prima della deforestazione sale vertiginosamente fino a raggiungere quasi il livello della foresta dopo la deforestazione

#MMP   mettiamo la scala dell'asse delle y da 0 a 100 così i due grafici possono essere confrontati meglio
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grafico22<-ggplot(output,aes(x=cover,y=after,color=cover))+geom_bar(stat="identity",fill="white")+ylim(0,100)
grid.arrange(grafico1,grafico2,nrow=1)

#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### OTTAVO CODICE
### R MULTITEMP NO2!
## ANALISI NO2 DA DATI ESA, GENNAIO-MARZO 2020
setwd("~/lab/")

# MMP Librerie che ci servono
library(raster)
#MMP importiamo le singole immagini scaricate e nella cartella lab


EN01 <- raster("EN_00001.png")
plot(EN01)
dev.off()

# MMP  importare tutte le immagii
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
plot(EN01, col=cl)  #MMP  inizio gennaio
plot(EN13, col=cl) # MMP marzo

# MMP  operiamo una vera e propria statistica

    par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)
 
 #MMP  metodo più lungo
 
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

dev.off()


### PARTE DUE!!


setwd("~/lab/")
load("EN.RData")
ls()

library(raster)# MMP  per importare più immagini insieme 
## MMP  per importare più immagini insieme bisogna creare una cartella contenente tutte le immagini EN e selezionare questa cartella come nuova WD
setwd("~/lab/esa_no2")

#  MMP creiamo una rlist contenente la lista di file .png
rlist<-list.files(pattern=".png")
rlist # MMP  visualiziamo il contenuto del nuovo data creato

# MMP save raster into list
# MMP  lapply applica una funzione su una lista o un vettore (serie di elementi). 
# MMP  In questo caso la funzione che vogliamo applicare per caricare i singoli dati sono brick (immagine satellitare con tutti i sensori) e raster (immagine con un solo sensore)
#  MMP  applichiamo alla listra rlis la funzione raster
listafinale<-lapply(rlist, raster)

listafinale 
# MMP  visualizziamo i RasterLayer 

#  MMP  funzione stack permette di impacchettare tutte le immagini in una unica
EN <- stack(listafinale)

cl<-colorRampPalette(c('red','orange','yellow'))(100)
# MMP  possiamo dunque plottare l'immagine
plot(EN,col=cl)


####

library(raster)
setwd("~/lab/esa_no2")

rlist
listafinale<-lapply(rlist,raster)
listafinale
EN<-stack(listafinale)

# MMP  una volta caricate le immagini creiamo una differenza tra l'ultima immagine (EN 13 corrispondente a Marzo) e la prima immagine (EN 01 corrispondente a Gennaio) 
difEN<- EN$EN_0013-EN$EN_0001

# MMPcolorRampPalette
cld<-colorRampPalette(c('blue','white','red'))(100)

#  MMP  plottiamo la differenza delle immagini con il colore creato così da visualizzare le differenze tra Marzo e Gennaio
plot(difEN,col=cld)

# MMP  boxplot
boxplot(EN,horizontal=T,outiline=F,axes=T)
#  MMP horizontal=T -> disposizione orizzontale
#  MMP outline=F -> senza linee esterne
# MMP  axes=T di defoult, mette gli assi (il contrario axes=F li toglie)


#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### NONO CODICE
### R SNOW COVER!

# COPERNICUS GLOBAL LAND

## MMP analisi cambiamenti climatici sulla copertura nevosa
# MMP cryosphere: snow cover
# MMP basic: data di inizio e fine
#  MMP downoad, cartella lab

setwd("~/lab/")
# MMP  libreria ncdf4: dati coMe estenzione nc

install.packages("ncdf4")
library(ncdf4)
library(raster)
# MMP il pacchetto di prima potrebbe essere inserito in alcune versioni nel pachetto raster

## MMP visualizzare il file nc: prima va importato
# MMP raster(),: raster importa un singolo livello: la copertura della neve quel singolo giorno 
#  MMP brick () : brick importa diverse bandesno
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# esecizio: plot snow color con cl
plot(snowmay, col=cl)

##  MMP cosa ci aspettiamo: che nel tempo la copertura nevosa è diminuita. dati iol: snow.zip <- download in lab (nuova cartell DENTRO LAB COÌ POSSIAO importarli tutti insieme)


####  MMP import snow data
setwd("~/lab/snow") # CARTELLA SNOW

#  MMP preniamo una lista di file e le associamo un nome (rlist): file all'interno della cartella
rlist <- list.files(pattern=".tif")
rlist 
# MMP lapppy: applica dei comandi ad un intero set di dati
## MMP funzione che ci interessa:raster
## MMP creiamo uno stack: un tipo di oggetto all'iterno di R a cui daremo un nom
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

plot(snow.multitemp,col=cl)

# MMP differenza netta fra anno 2000 e 2020
## MMP previsione per anno 2025

 
# MMP plot della prima immagine e dell'ultima
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
#  MMP la secoda immagine non arriva a 250 nella legenda ma solo a 200 : zlim
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250)) ## SE NON HANNO GLI STESSI VALORI NON POSSONO ESSERE CONFRONTATE
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff) # MMP differenza pincipale fra i due set: sotratta imagine 2000 a quella 2020

# MMP  previsione multitemporale: 2025, quale sarà la copertura nevosa ?
# MMP Iol: prediction.r <- creare previsione futura
# MMP lm: modello linere, x:rispetto linea del tempo... incognita che vogliamo avere come valore. time
#  MMP salva prediction.r nella snow in lab

#  MMP source : funzione per richiamare codice dall'esterno
source("prediction.r")
# MMP richiamiamo il codice contenuto in prediction



predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
##  MMP importa file da iol

## MMP plottaggio previsione
plot(predicted.snow.2025.norm, col=cl)

# MMP  queste previsioni, fatte sulla base di variazioni attuali, si chiamano scenari

#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
### DECIMO CODICE
### R PATCHES!

setwd("~/lab/")
library(raster)

# MMP D1c e D2c erano le due mappe classificate l'altra volta
# MMP writeRaster (d1c$map, "d1c.tif")

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c, col=cl)
plot(d2c, col=cl)

# FORESTA: CLASSE NUMERO UNO, COLORATA DI VERDE. 
## CBIND ELIMINA ALCUNI VALORI. IN QUESTO CASO,TUTTI I VALORI DI AGRICOLTURE=1 (L'ALTRA CLASSE) => NA: VALORI NULLI ATTRAVERSO FUNZIONE RECLASSIFY

# FOREST= CLASSE DUE. AGRICOLTURA= CLASSE UNO

d1c.for <- reclassify(d1c, cbind(1,NA))

# PLOT
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c, col=cl)
plot(d1c.for)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green')(100))
plot(d1c, col=cl)
plot(d1c.for, col=cl)

## ABBIAMO RICLASSIFICATO LA NOSTRA MAPPA INIZIALE, E A QUESTO PUNTO POSSIAMO FARE LA STESSA OPERAZIONE PER L'ALTRO PERIODO
d2c.for <- reclassify(d2c, cbind(1,NA))

## PLOT DI ENTRAMBE LE MAPPE CLASSIFICATE SOLO PER LA FORESTA
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# CREATING PATCHES
clump() # FUNZIONE CHE ANDREMO AD USARE
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# SALVARE DATI VERSO L'ESTERNO
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

#writeRaster: esportare file

# ESERCIZIO: plottare entrambe le mappe una accanto all'altro
par(mfrow=c(1,2))
plot(d1c.for.patches)
plot(d2c.for.patches)

# CAMBIAMO I COLORI, PER OGNI PATCHES UN COLORE DIVERSO
## NUOVA COLOR RAMP PALETTE

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

d1c # PER VEDERE NUMERO DI PATCHES

time <- c("Before deforestation","After deforestation")
npatches <- c(301 1212)

outpout <- data.frame(time,npatches)
attach(output)

# PLOT FINALE CON GGPLOT

library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identify",fill="white")

# GRAFICO FINALE
## VARIAZIONE DEL NUMERO DEI PATCHES.
                    


###################################################################################################################
#####################################################################################################################
######################################## #########################################################################

#### MMP  R_code_crop - EXAM SIMULATION
### MMP crop di neve

library(raster)

setwd("~/lab/snow/") # non solo lab, ma anche snow perchè abbiamo creato la cartella snow dentro lab
## MMP scaricare dati da copernicus

# ESERCIZIO: CARICARE TUTTE LE IMMAGINI SNOW
## CONTROL F PER CERCARE NEL CODICE 
#LAPPLY

rlist <- list.files(pattern="snow") # tutti i file che contengono snow nel nome
rlist

list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

# COLOR RAMP PALETTE
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(snow.multitemp,col=clb)
# COSÌ FACCIAMO IL PLOT TUTTO INSIEME, SENZA USARE PAR

#ZOOM

# QUALI SONO I NOMI DI SNOW MULTITEMP?
snow.multitemp
#  PRENDIAMO QUELLO DEL 2010
plot(snw.multitemp$snow2010, col=clb)

# ZOOM SULLA ITALIA
# PRIMO METODO: COORDINATE . X: 0-50 (6-15).Y: 40 -50
extention <- c(6, 18, 40, 50)
# 6 E 18 SONO LA X MINIMA E MASSIMA, COSÌ COME 40 E 50 SONO LA Y MINORE E MAGGIORE IN PUÒ CASCARE L'ITALIA
zoom(snow.multitemp$snow2010r, ext=extension) #MANCA QUALCHE PEZZETTINO
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent())
#R STA ASPETTANDO CHE NOI DISEGNIAMO UN RETTANGOLO SULL'IMMAGINE ORIGINALE: PARTIRE DA PUNTO IN ALTO A SINISTRA,TENERE PREMUTO, FARE UN RETTANGOLO (NON SI VEDE), LASCIARE E CLICCARE UNA VOLTA

#CROP : NUOVA IMMAGINE RITAGLIATA SULLA ZONA DEFINITA
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension) # SU CROP NON VA UTILIZZATO EXT
# NON È UNO ZOOM ,MA UNA VERA IMMAGINE
plot(snow.italy, col=clb)

##ESERCIZIO : CROP DI ITALY EXTENT DELL'INTERO STACK
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)

# FACCIAMO LEGENDE TUTTE UGUALI
# GUARDIAMO I VALORI PIÙ ALTI E PIÙ BASSI
snow.multitemp.italy
# MAX= 195 - , MIN= 20
## FACCIAMO VARIARE DA 20 A 200
plot(snow.multitemp.italy, col=clb, zlim=c(20,200)) # DI SOLITO SI USANO I VALORI DA 0 A 255
#ZLIM: SE NON CI FOSSE, AVREI LEGEDE CHE VARIANO IN MODO DIVERSO E QUINDI NON COMPARABILI. 
## ZLIM QUINDI DEFIISCE I LIMITI DELLE LEGENDE

## ANALISI VELOCE SU TUTTO LO STACK: 
#BOXPLOT: COME SI COMPORTANO I VALORI? 

boxplot(snow.multitemp.italy, horizontal=T, outline=F) # VEDERE QUANTITATIVAMENTE I VALORI (DI NEVE)NELLE VARIE IMMAGINI. STIMIAMO QUANTO È LA VARIAZIONE DI COPERTURA NEVOSA NELLE VARIE IMMAGINI


###########################################################################################################################
#############################################################################################################################
##########################################################################################################################
######################################################################################################################
## CODICE DODICESIMO
# R_code_ Species Distribution modelling

install.packages("sdm")
library(sdm)

library(raster)
library(rgdal)

# CARICARE IL FILE
file <- system.file("external/species.shp", package="sdm")

# FUNZIONE RGDAL : CARICARE I FILE E LE IMMAGINI
species <- shapefile(file)

species # SPATIAL POINT DATAFRAME: VEDIAMO TUTTE LE INFORMAZIONI

species$Occurrence

plot(species)

# PUNTI VALORI 1: DOVE È STATA CAMPIONATA E C'ERA (PRESENZA). 0: CAMPIONATA MA ASSENTE
## ALTRA VISUALIZZAZIONE
## CONDIZIONE : ==

plot(species[species$Occurrence == 1,],col='blue',pch=16)
 # SOLO PUNTI IN CUI L'OCCORRENZA ERA UNO

##AGGGIUNGERE PUNTI AD UN PLOT PRECEDENTE: FUNZIONE POINT
# ANDIAMO A AGGIUNGERE I PUNTI IN CUI OCCORRENCE=0

points(species[species$Occurrence == 0,],col='red',pch=16)

# VARIABILI AMBIENTALI
## TEMPERATUR

## IMPORTIAMO PATH , CARTELLA EXTERNAL
path <- system.file("external", package="sdm") 

# LISTA FILE, IN UN CERTO PERCORSO (PATH)
# PATTERN ASC : FILE ASC. 
lst <- list.files(path=path,pattern='asc$',full.names = T)

# PREDITTORI: VARIABILI (AMBIENTALI) CHE PREDICONO LA DISTRIBUZIONE DELLA SPECIE NELLO SPAZIO

lst 

preds <- stack(lst)
cl <- colorRampPalette(c('pink','violet','dark violet')) (100)

# PLOT DI UNA SOLA VARIABILE
plot(preds$elevation, col=cl)

# AGGIUNGIAMO I PUNTI CHE HANNO OCCORRENZA ==1 (PRESENZA)

points(species[species$Occurrence == 1,], pch=16)

## LA SPECIE È PRESENTE A BASSE ELEVATION
# PLOT TEMPERATURA

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# FORSE AMA LA TEMPERATURA ALTA? 
# INFATTI È COSÌ 

## PLOT PRECIPITAZIONI
plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# SITUAZIONE INTERMEDIA

# INDICE DI VEGETAZIONE
plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# SITUAZIONE OMBREGGIATA 

# MODELLIZZAZIONE
sdmData
# DUE ARGOMENTI: TRAIN (AREE DI SAGGIO: SPECIES) E PREDICTORS. 

d <- sdmData(train=species, predictors=preds)
d
# Y: OCCORRENZE ALL'INTERNO DELLA SPECIE

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, d, methods="glm")
# D: DATASET

p1 <- predict( m1, newdata= preds)
# MODELLO E DATI
# NEW DATA: NOSTRI PREDITTORI


plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16) 
## MAPPA DI DISTRIBUZIONE PREVISTA DELLA DISTRIBUZIONE DI QUESTA SPECIE


########################################################################################################################
####### EXAM PROJECT
# MMP Analisi delle variazioni del valore el LAI dal 2000 al 2020, con dati che si riferisono alla situazione agli inizi di marzo degli anni pari. 
#MMP per il 2020 sono stati presi alla fine di Gennaio, dal momento che non erano disponibili più recenti.

#MMP librerie utili per questa analisi
library(raster)
library(ncdf4)
library(rgdal)

#MMP settare la working directory
#MMP cartella lai creata dentro la cartella lab
setwd("~/lab/lai")

# MMP iniziamo con importare la prima immagine, del  2000
# MMP funzione raster: per importare tutti i livelli.
lai2000 <- raster("LAI_2000.tiff")
lai2000 # visualizziamo i suoi valori
# creiamo una colorpalette
cl <- colorRampPalette(c("pink", "violet", "purple", "blue", "dark blue"))(100)
#  plot prima immagine
plot(lai2000, col=cl)

#MMP molto lungo importare ogni singola immagine in questo modo

#MMP creiamo una lista di file contenuti nella cartella, accumunati da ".tiff"
llist <- list.files(pattern=".tiff")
llist 
#MMP  applichiamo, con apply, la funzione "raster" all'intero dataset per importarlo
list_rast <- lapply(llist, raster)
list_rast
#MMP  con stack: creiamo un oggetto in R a cui diamo un nome, che varrà come dataset
lai_multitemp <- stack(list_rast)
lai_multitemp # MMP tutti gli oggetti contenuti dentro lai_multitemp
#MMP così possiamo procedere con il plottggio, inserendo tutte le immagini in un solo
# MMP grafico senza utilizzare par
plot(lai_multitemp, col=cl)
dev.off()

#MMP boxplot di confronto dal 2000 al 2020
boxplot(lai_multitemp, horizontal=T, outline=F, main= "boxplot 2000-2020",col="steel blue")
dev.off()

#MMP  paragoniamo la prima immagine (2000), la 6 (2010) e l'ultima (2020): così 
#MMP può essere eseguito un confronto più evidente di quello che è avvenuto ogni 10 anni

#MMP  nomi delle singole immagini?
names(lai_multitemp)

par(mfrow=c(1,3)) # MM Pnon mi piace
plot(lai_multitemp$LAI_2000, col=cl,main=" LAI 2000" , zlim=c(0,250)) #MMP zlim: impostiamo gli stessi valori di legenda per potere efettuare un paragone
plot(lai_multitemp$LAI_2010, col=cl, main="LAI 2010 ", zlim=c(0,250))
plot(lai_multitemp$LAI_2020, col=cl, main="LAI 2020", zlim=c(0,250))
dev.off()

par(mfrow=c(2,2)) #MMP più bello 
plot(lai_multitemp$LAI_2000, col=cl,main=" LAI 2000" , zlim=c(0,250))
plot(lai_multitemp$LAI_2010, col=cl, main="LAI 2010 ", zlim=c(0,250))
plot(lai_multitemp$LAI_2020, col=cl, main="LAI 2020", zlim=c(0,250))
dev.off()

#### MMP procedimo confrontando le coppie "2000-2010", "2010-2020" e "2000-2020"

## MMP confronto 2000 e 2010
par(mfrow=c(2,2))
plot(lai_multitemp$LAI_2000, col=cl,main=" LAI 2000" , zlim=c(0,250))
plot(lai_multitemp$LAI_2010, col=cl, main="LAI 2010", zlim=c(0,250))
dev.off()
# MMP calcolo e plottaggio differenza
diflai10 <- lai_multitemp$LAI_2010 - lai_multitemp$LAI_2000
cldiff <- colorRampPalette(c('dark violet',' snow','blue'))(100)
plot(diflai10, col=cldiff, main=" Differenza fra 2010 e 2000")
dev.off()

## MMP confronto 2010 e 2020
par(mfrow=c(2,2))
plot(lai_multitemp$LAI_2010, col=cl,main=" LAI 2010" , zlim=c(0,250))
plot(lai_multitemp$LAI_2020, col=cl, main="LAI 2020", zlim=c(0,250))
dev.off()
#MMP calcolo e plottaggio differenza
diflai20 <- lai_multitemp$LAI_2020 - lai_multitemp$LAI_2010
cldiff <- colorRampPalette(c('dark violet',' snow','blue'))(100)
plot(diflai20, col=cldiff, main=" Differenza fra 2020 e 2010")
dev.off()

## MMP confronto 2000 e 2020
par(mfrow=c(2,2))
plot(lai_multitemp$LAI_2000, col=cl,main=" LAI 2000" , zlim=c(0,250))
plot(lai_multitemp$LAI_2020, col=cl, main="LAI 2020", zlim=c(0,250))
dev.off()
#MMP calcolo e plottaggio differenza
diflai <- lai_multitemp$LAI_2020 - lai_multitemp$LAI_2000
cldiff <- colorRampPalette(c('dark violet',' snow','blue'))(100)
plot(diflai, col=cldiff, main="Differenza fra 2020 e 2000")
dev.off()


###### MMP zoom su Europa/Italia con boxplot  di tutti gli anni

####Europa: 
#MMP coordinate X:- 15: 40, Y: 15:90
extension <- c(-15,40,15,90)# MMP creiamo un vettore extension, con le coordinate
zoom(lai_multitemp$LAI_2000, ext=extension)
# MMP zoom crea , appunto, uno zoom della area definite: utile per capire
# MMP se abbiamo inserito bene le coordinate

extension <- c(-15,40,15,90)
lai_multitemp.Europe <- crop(lai_multitemp, extension) #MMP crop crea una vera immagine della zona definita
#MMP  crop lo applichiamo a tutto lai_multitemp

# MMP per l'Europa, eseguiamo il plottaggio e costruiamo un boxplot per tutti gli anni

plot(lai_multitemp.Europe, col=cl, zlim=c(0,250))

boxplot(lai_multitemp.Europe, horizontal=T, outline=F, col="orchid", main="boxplot Europe")

names(lai_multitemp.Europe)

par(mfrow=c(2,2))
plot(lai_multitemp.Europe$LAI_2000, main="Europe 2000", col=cl)
plot(lai_multitemp.Europe$LAI_2010, main="Europe 2010", col=cl)
plot(lai_multitemp.Europe$LAI_2020, main="Europe 2020", col=cl)

##MMP e procediamo con un confronto del 2000, 2010 e 2020: e poi le singole coppie
# MMP confronto 2000 - 2010
par(mfrow=c(2,2))
plot(lai_multitemp.Europe$LAI_2000, main="Europe 2000", col=cl)
plot(lai_multitemp.Europe$LAI_2010, main="Europe 2010", col=cl)
dev.off()
#MMP calcolo e plottaggio della differeza
difEuropa10 <- lai_multitemp.Europe$LAI_2010 - lai_multitemp.Europe$LAI_2000
plot(difEuropa10, main="Differenza Europa 2010-2000", col=cldiff)

# MMP confronto 2010 - 2020
par(mfrow=c(2,2))
plot(lai_multitemp.Europe$LAI_2010, main="Europe 2010", col=cl)
plot(lai_multitemp.Europe$LAI_2020, main="Europe 2020", col=cl)
dev.off()
#MMP calcolo e plottaggio della differenza
difEuropa20 <- lai_multitemp.Europe$LAI_2020 - lai_multitemp.Europe$LAI_2010
plot(difEuropa20, main="Differenza Europa 2020 - 2010", col=cldiff)

# MMP confronto 2000 - 2020
par(mfrow=c(2,2))
plot(lai_multitemp.Europe$LAI_2000, main="Europa 2000", col=cl)
plot(lai_multitemp.Europe$LAI_2020, main="Europa 2020", col=cl)
dev.off()
#MMP calcolo e plottaggio differenza
difEuropa <- lai_multitemp.Europe$LAI_2020 - lai_multitemp.Europe$LAI_2000
plot(difEuropa, main=" Differenza Europa", col=cldiff)

####Italia
extension <- c(0,20,30,50) #MMP vettore coordinate
zoom(lai_multitemp$LAI_2000, ext=extension)

extension <- c(0,20,30,50)
lai_multitemp.Italy <- crop(lai_multitemp, extension) # MMP creazione della crop

# MMP plot e boxplot dell'Italia per tutti gli anni
plot(lai_multitemp.Italy, col=cl, zlim=c(0,250))

boxplot(lai_multitemp.Italy, horizontal=T, outline=F, col="salmon", main="boxplot Italy")


#MMP confronto 2000- 2010 e 2020 sull'Italia
par(mfrow=c(2,2))
plot(lai_multitemp.Italy$LAI_2000, main="2000", col=cl)
plot(lai_multitemp.Italy$LAI_2010, main="2010", col=cl)
plot(lai_multitemp.Italy$LAI_2020, main="2020", col=cl)
dev.off()

#MMP confronto 2000-2010
par(mfrow=c(2,2))
plot(lai_multitemp.Italy$LAI_2000, main="2000", col=cl)
plot(lai_multitemp.Italy$LAI_2010, main="2010", col=cl)
dev.off()
#MMP differenza
difItalia10 <- lai_multitemp.Italy$LAI_2010 - lai_multitemp.Italy$LAI_2000
plot(difItalia10, main="Differenza 2010 - 200", col=cldiff)

#MMP confronto 2010 - 2020
par(mfrow=c(2,2))
plot(lai_multitemp.Italy$LAI_2010, main="2010", col=cl)
plot(lai_multitemp.Italy$LAI_2020, main="2020", col=cl)
dev.off()
#MMP differenza
difItalia20 <- lai_multitemp.Italy$LAI_2020 - lai_multitemp.Italy$LAI_2010
plot(difItalia20, main="Differenza Italia 2020-2010", col=cldiff)

#MMP confronto 2000-2020
par(mfrow=c(2,2))
plot(lai_multitemp.Italy$LAI_2000, main="2000", col=cl)
plot(lai_multitemp.Italy$LAI_2020, main="2020", col=cl)
dev.off()
#MMP differemza
difItalia <- lai_multitemp.Italy$LAI_2020 - lai_multitemp.Italy$LAI_2000
plot(difItalia, main="Differenza Italia", col=cldiff)

###
# previsione di cosa accadrà nwl futuro 
ext <- c(-180,180,-100,100)
estention <- crop(lai_multitemp, ext)
time <- 1:nlayers(lai_multitemp)

fun <- function(x){if
  (is.na(x[1])){NA} else
  {lm(x ~ time)$coefficients[2]}}
predicted.lai.2030 <- calc(estention, fun)
predicted.lai.2030n <- predicted.lai.2030*255/39.513600
#MMP dopo aver creato la previsione, possiamo eseguire il plottaggio
plot(predicted.lai.2030n, main="2030", col=cl)
plot(predicted.lai.2030n, main="2030", col=cl, zlim=c(0,250))

# MMP creo una colorpalette per la previsione e la uso nel plottaggio
cl30 <- colorRampPalette(c("yellow","dark green","dark blue"))(100)
plot(predicted.lai.2030n, main="2030", col=cl30, zlim=c(0,250))

# MMP zoom sull' Europa del 2030
extension <- c(-15,40,15,90)
Europe30 <- crop(predicted.lai.2030n, extension)
plot(Europe30, col=cl, zlim=c(0,250))

# MMP e sull'Italia del 2030
extension <- c(0,20,30,50)
Italy30 <- crop(predicted.lai.2030n, extension)
plot(Italy30, col=cl30, zlim=c(0,250))

