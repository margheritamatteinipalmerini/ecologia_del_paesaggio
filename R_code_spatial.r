############ R spatial: scopriamo le funzioni spaziali di R 


install.packages("sp")  
#chiamiamo un paccheto dall'esterno, quindi virgolette
#finestre con luoghi: tutti luoghi i cui viene sviluppato R al momento (CRAN => CRAN MIRROR): meraviglioso!
library(sp) 
# serve per spiegare al software quale pacchetto richiamare
data(meuse)
#data: funzione per i dati
#meuse: dataset
head(meuse)
#in questo modo vediamo direttamente non solo gli elementi ma anche le coordinate che utilizzeremo ora
#esercizi: plot di rapporto fra cadmio e piombo
#alla fine faremo un multipannello in cui compaiono più plot
#alleghiamo il dataframe 
attach(meuse)
plot(cadmium,lead,col="red",pch=19,cex=2)
#virgola: separator
#col: cambio colore dei punti come più mi piace
#dentro R la funzione per cambiare carattere è pch
#caratter exateration... aumentare (>1) diminuire (<1)
#dovrebbe uscire un grafico
#exercise o esercizio: è un esercizio da fare
#exercise: plot di copper e zinco con  caratteree (simbolo) trnagolo e colore verde
#triangolo pch=17
plot(copper,zinc,col="green",pch=17,cex=2)
#2: quindi il doppio 
#solitamnete le vergolette si mettono su un testo: avviso che ci troviamo di fronte ad un testo, ma anche quando facciamo il nostro software con qualcosa di esterno.
#xlab: cambio etichetta
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rsme",ylab"zinco")
#multiframe (mf) o multipanel: funzione importantissima per mettere più di un grafico all'interno di una finestra
#par: funzioe per tutti i pannelli, quindi anche tutti i grafici. numeri di righe e colonne
par(mfrow=c(1,2))
#numeri in fila: vettori
#prima bisogna metterci le cose dentro prima di copiare ed incollare su R
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#copiare tutto il blocco (le 3 righe sopra) , incollare e premere invio
#invertiamo riga/colonna colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
#multipannelo con pairs --> esiste un pacchetto che può aiuare
#multiframe automatico: andiamo ad installare 
install.packages("GGally")
#virgolette perchè dall'esterno
library(GGally)
#richiamiamolo
#pairs:multipannello di multivariabili
ggpairs(meuse)
#ci vorrà un sacco, subset []
ggpairs(meuse[,3:6])
# , vuol dire "partiamo da"
# : "fino a"
#grafico: che cos'è? sulla parte diagonale vediamo le singole varibiali e la distribuzione dei dati
#plot variabili in relaione fra di loro (in modo simetrico)
#correlazioni 
#coeficente fra -1 e 1

#spatial!

head(meuse)

#coordinate: x e y come in tutti i grafici
#diciamolo al software che ci sono delle coordinate
coordinates(meuse)
#non incolliamola subito su R: è particolare
coordinates(meuse)=~x+y
#in R,per raggruppare dei pezzi usiamo la tilde (Linux:Alt Gr + ì - Windows: Alt + 126) 
#adesso possiamo incollarlo su R
plot(meuse)
#grafico
spplot(meuse,"zinc")
#funzione per plottare i dati spazialmente
#all'interno della funzion spplot le variabili vanno scitte con le virgolette perchè vengono prese dall'esterno
#il nostro primo grafico spaziale: che emozione!

# finiamo domani!

### parte due (giorno seguente)
#richiaiamo la libreria sp e i dati da usare
library(sp)
data(meuse)
# coordinate xy, ascisse e ordinate, del dataset
# head serve per dare un occhiata veloce al dataset! (memo!)
coordinates(meuse)=~x+y
#quando si inserisce una funzione su R e non vediamo risposta, vuol dire che tutto è andato bene!
spplot(meuse,"zinc")
#sp (ilpacchetto) + plot! in questa funzione le variabili vanno indicate fra virgolette
#deve uscire una immagine 

#esercizio: spplot dei dati di rame
#per vedere come si chiama il rame o head(meuse) : xy in coordinate 
head(meuse)
##oppure: per vedere SOLO i nomi delle colonne
names(meuse)
#quindi:
spplot(meuse,"copper")
# avevamo visto che erano coordinati rame e zinco, infatti anche il primo ha valori decrescenti in base alla lontananza dal fiume

#stesso plottaggio,altra funzione di sp : bubble
bubble(meuse,"zinc")
#grafico: stessa rapresentazione di prima, ma invece che dei colori che si relazionano ai diversi valori, qui vengonorelazionati a delle bolle "bubble" in base ad alte o basse concentraione
# conferma le concentrazioni dell'altro grafico
## questo mi sembra più immediato ed elegante. si chiama infografico

#esercizio: funzione bubble del rame ma colorato di rosso
##se si cerca la funzione in rete, vengono fornite tutte le informazioni del caso
bubble(meuse,"copper",col="red")
## red va scitto fra virgolette perchè fa riferimento a valori numerici che indicano come viene fatto il colore rosso

#esempio: ci inventiamo dei dati
# foraminiferi (Sofia), carbon capture (Marco)
10, 20, 35, 55, 67, 80
#in R, questa serie di numeri è un vettore (array)
foram <- c(10, 20, 35, 55, 67, 80)
#abbiamo dato un nome al dataset!abbiamo creato un oggetto
carbon <- c(5, 15, 30, 70, 85, 99)
# abbiamo dato nome anche all'altra serie di numeri (di Marco)
#inseriamoli su R
#plottiamo i due dati per vedere se sono relazionati fra loro
plot(foram, carbon, col="green", cex=2, pch=19)
#stretta relazione fra il carbonio intrappolato dai foraminiferi e la densità di questi ultimi
##piccolo esercizio per creare oggeti

#scarica tabella da iol, covid_agg.csv
#crea una nuova cartellahome: percorso più corto che si può fare!! "lab"

#dati dall'esterno: covid19
#specifcare la cartella al software con cui utilizziamo
#cartella appena create 
#setwd("~/lab")
setwd("~/lab")
covid <- read.table("covid_agg.csv",head=TRUE)
#agg : dati aggregati
#dobbiamo spiegare a r che ci sono dei titoli
#TRUE: variabili 1-0, vero falso, .poteva essere anche solo T
#relazionato al nome covid
head(covid)
#vedi la tabellina?

## continua nella prossima puntata...




