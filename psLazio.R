library(rvest)
library(htmltab)

ateam <- read_html("https://www.regione.lazio.it/accessiprontosoccorso/")
tabella<-html_node(ateam, xpath='//*[@id="tab_right"]/table')
html_table(tabella, fill = TRUE)
##################################################################################
View(ateam)
google_form("1M9B8DsYNFyDjpwSK6ur_bZf8Rv_04ma3rmaaBiveoUI")
google2 <- read_html(httr::GET("http://google.com"))
box_office <- read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
box_office %>% html_node("form") %>% html_form()

##################################################################################
# FUNZIONANTE:

library(htmltab)
url <- "https://www.regione.lazio.it/accessiprontosoccorso/"
# ritardo in secondi
delay<-300
contatore<-as.numeric(c(1:6))
ripetizioni<-6
for (i in contatore) {
   if (i<=ripetizioni) {
      nome<-paste("pslazio",i,".csv",sep="")
      table<-htmltab(doc = url, which="//*[@id='tab_right']/table" )
      write.table(table, file=nome, sep=",")
      Sys.sleep(delay)
   }
}

for (i in contatore) {
   if (i==1) {
      nome<-paste("pslazio",i,".csv",sep="")
      psLazioT<-read.csv(nome)
   }   
   else if (i<ripetizioni || i!=1) {
      nome<-paste("pslazio",i,".csv",sep="")
      psLazio<-read.csv(nome)
      psLazioT<-rbind(psLazioT,psLazio)
   }
}

psLazioBak<-psLazioT

psLazioT<-data.frame(psLazioT)
psLazioT[, c(7:12,14:19,21,23:27,29)] <- sapply(psLazioT[, c(7:12,14:19,21,23:27,29)], as.numeric)
colnames(psLazioT)<-c("Struttura","Comune","Asl","Tipo","Aggiornamento","k6","RossoAt","GialloAt","VerdeAt","BiancoAt","NonAt","TotAt","k13","RossoTr","GialloTr","VerdeTr","BiancoTr","NonTr","TotTr","k20","AttesaRic","k22","RossoOs","GialloOs","VerdeOs","BiancoOs","TotOs","k28","TotTot")
psLazioT3<-psLazioT[,c(-6,-13,-20,-22,-28)]
psLazioT4<-psLazioT3[rowSums(is.na(psLazioT3))==0,]
write.table(psLazioT4, file="psLazioTot.csv", sep=",")

attach(psLazioT4)
aggdata <-aggregate(psLazioT4, by=list(Struttura), 
                    FUN=mean, na.rm=TRUE)
print(aggdata)
detach(psLazioT4)

################################################################################################


psLazioDF2<-data.frame(ID=psLazioT4[,1], Means=rowMeans(psLazioT4[,7]))

#pslazioTTT$VerdeAt<-as.numeric(as.character(pslazioT$VerdeAt))
#pslazioTT<-transform(pslazioT, pslazioT[,[7:12,14:19,21,23:27,29]]<- as.numeric(pslazioT[,[7:12,14:19,21,23:27,29]]))
psLazioG<-group_by(psLazioT4, Struttura)
vettoreColori<-c("RossoAt","GialloAt","VerdeAt","BiancoAt","NonAt","TotAt","RossoTr","GialloTr","VerdeTr","BiancoTr","NonTr","TotTr","AttesaRic","RossoOs","GialloOs","VerdeOs","BiancoOs","TotOs","TotTot")
i<-'GialloAt'
for (i in vettoreColori) {
   psLazioM<-mean(i)
   psLazioM<-summarise(psLazioG, mean(i))
}
psLazioM<-summarise(psLazioG, mean(VerdeAt))
