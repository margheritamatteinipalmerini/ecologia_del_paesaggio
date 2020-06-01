#CODICI PER ESAME

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










