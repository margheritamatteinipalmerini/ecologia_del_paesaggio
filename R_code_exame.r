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


### PRIMO CODICE 
### PAIRS AND PLOT

# INSTALLIAMO IL PRIMO PACCHETTO: sp. USIAMO LE VERGOLETTE PERCHÈ STIAMO ANDANDO A CHIAMARE QUALCOSA AL DI FUORI DEL NOSTRO R
# USIAMO LA FUNZIONE install.packages("sp")
install.packages("sp")

#UNA VOLTA INSTALLATO, RICHIAMIAMO IL PACCHETTO CON LA FUNZIONE library(). IN CASO, UN ALRO COMANDO PER FARE PARTIRE LE LIBRERIE È require().
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
# MAIN: FUNIONE PER DARE UN NOME A QUESTO GRAFICO
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

# RICHIAMIMO IL SET DI DATI " MEUSE"
library(sp)
data("meuse")
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
# MMP finestre con luoghi: tutti luoghi i cui viene sviluppato R al momento (CRAN => CRAN MIRROR): meraviglioso!
library(sp) 
# MMP serve per spiegare al software quale pacchetto richiamare
data(meuse)
# MMP data: funzione per i dati
# MMP meuse: dataset
head(meuse)
#MMP in questo modo vediamo direttamente non solo gli elementi ma anche le coordinate che utilizzeremo ora
# esercizi: plot di rapporto fra cadmio e piombo
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
#MMPtriangolo pch=17
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
# MMP copiare tutto il blocco (le 3 righe sopra) , incollare in R e premere invio
# MMP invertiamo riga/colonna colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#MMP multipannelo con pairs --> esiste un pacchetto che può aiuare
#MMP multiframe automatico: andiamo ad installare 
install.packages("GGally") ##MMP tutti i pacchetti da installare per rieguire un codice andrebbero messi tutti nelle prime righe
#MMP virgolette perchè dall'esterno
library(GGally) #MMP richiamiamolo
#MMP pairs:multipannello di multivariabili
ggpairs(meuse) #MMP ci vorrà un sacco, subset []
ggpairs(meuse[,3:6])
# MMP, vuol dire "partiamo da"
#MMP  : "fino a"
#gMMP rafico: che cos'è? sulla parte diagonale vediamo le singole varibiali e la distribuzione dei dati
#MMP plot variabili in relaione fra di loro (in modo simetrico)
# MMP correlazioni
#MMP coeficente fra -1 e 1

####MMP  spatial!

head(meuse)

#MMP coordinate: x e y come in tutti i grafici
#MMP diciamo al software che ci sono delle coordinate:
coordinates(meuse)
### MMP non incolliamola subito su R: è particolare
coordinates(meuse)=~x+y
#MMP in R,per raggruppare dei pezzi usiamo la tilde
# MMP adesso possiamo incollarlo su R
plot(meuse)
#MMPg rafico
spplot(meuse,"zinc")
#MMP funzione per plottare i dati spazialmente
#MMP all'interno della funzion spplot le variabili vanno scitte con le virgolette perchè vengono prese dall'esterno
#MMP il nostro primo grafico spaziale: che emozione!

################### PARTE DUE
#MMP  richiaiamo la libreria sp e i dati da usare
library(sp)
data(meuse)
# MMP coordinate xy, ascisse e ordinate, del dataset
# MMPhead serve per dare un occhiata veloce al dataset! (memo!)
coordinates(meuse)=~x+y
#MMP ATTENZIONE: quando si inserisce una funzione su R e non vediamo risposta, vuol dire che tutto è andato bene!
spplot(meuse,"zinc")
#MMP sp (il pacchetto) + plot  in questa funzione le variabili vanno indicate fra virgolette
#MMP deve uscire una immagine 

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
# MMP questo tipo digrafico, infografico, forse èpiù immediato ed elegante

#MMP esercizio: funzione bubble del rame ma colorato di rosso
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
#MMP specifcare la cartella al software con cui utilizziamo
#MMP cartella appena create 
#MMP setwd("~/lab")
setwd("~/lab")
covid <- read.table("covid_agg.csv",head=TRUE)
#MMP agg : dati aggregati
#MMP dobbiamo spiegare a r che ci sono dei titoli
#MMP TRUE: variabili 1-0, vero falso, .poteva essere anche solo T
#MMP relazionato al nome covid
head(covid)



#############################################################################################################################################################################################
#############################################################################################################################à
###############################################################################################################
###############################################################################################################
###################################################################################################################
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
#MMP ogn paese è rappresntato da un punto ed ogni paese ha il proprio numero di casi
 
#MMP primo plot per visualizzare come sono distribuiti i dati nel mondo
plot(covid$country,covid$cases)
#MMP country:x - y:numero casi
# MMP $ collega una colonna al proprio dataset 
##MMP  in questo grafico il paese che ha più alto conteuo di casi è la Cina, ma non si vede sulla x. mettiamola sulla x
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
#MMP diminuiamo la grandezza dei punti (cex) => una cosa simile cex.lab, per diminuire leatichette
plot(covid$country,covid$cases,las=3, cex.lab=0.5) 
#MMP non è quello giusto 
plot(covid$country,covid$cases,las=3, cex.lab=0.5, cex.axis=0.5)
#MMP ce.axis cambia la dimensione di tutti gli assi

## MMP passiamo ad una visuaizzazione spaziale

##MMP ggplot2 # MMP pacchetto per creare grafici superpersonabilizzabili 
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

# MMP ggplot per covid, richiamiamo prima i nomi con names(covid); IN QUESTO CASO SI RIPETE UTILIZZANDO I DATI RELATIVI ALLA TABELLA COVID
#MMP CON L'AGGIUNTA DELL'ARGOMENTO size= PER ATTRIBUIRE LA DIMENSIONI AI PUNTI SUL GRAFICO, CHE RAPPRESENTANO IL NUMERO DI CASI.


ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()
#MMPcovid: datast
# MMP aes
names(covid)
head(covid)
#MMP dimenione punti (size) in relazione numero casi
#MMPgeometria: punti

##esercizio : misurare densità punti : quale pate del mondo ha una più alta densità di paesi che hanno avuto il coronavirus
#MMP density
#MMP ci serve un altro pacchetto, spatstat
# MMP crear dataset per spatstat
#MMP diamo un nome al dataset
library(spatstat)
attach(covid)
covidppp <- ppp(lon, lat, c(-180,180), c(-90,90)) # MMP ppp: questo comando permette di convertire i dati in point pattern
d <- density(covids)
#MMP le abbiamo dato un nome per utilizzarla meglio (abbiamo creato un oggetto)
#MMP facciamo un plot della densità
plot(d)
#MMP aggiungiamo due informazioni interessanti: origine e contorni paesi
points(covids, pch=19)

plot(d)
points(covids)

#MMP aggiungiamo i contorni dei paesi
#MMPdatabase internazionale: natural hearth data

#MMPsalviamo l'R data compltamente, l'intero progetto, workspace, .rdata 
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
cl <- colorRampPalette(c('yellow','orange','red'))(100) 
# MMP c: simbolo per idicare una serie di cose, in questo caso di colori
###MMP virgolette singole
#MMP quanti minilivelli fra un livello all'altro, più sono meglio è (gradazioni) : (100)
## MMP spatstat è una delle librerie che permette di utilizzare questa funzione
plot(d,col=cl)
# MMP colore = alla color palette appena creata (cl)

# Exercizio: plot della densità dal verde al blu
points(covids,pch=19,cex=0.5)
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
# MMP maggiore richchezza nella parte Sud-Est e nella parte centrale


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
### R POINT PATTERN!





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












