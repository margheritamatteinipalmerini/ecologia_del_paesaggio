# R spatial: scopriamo le funzioni spaziali di R 
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

#finiamo domani!






