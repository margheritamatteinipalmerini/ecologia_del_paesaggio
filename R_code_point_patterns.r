## codice per analisi dei point patterns

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









