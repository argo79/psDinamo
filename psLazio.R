library(htmltab)
url <- "https://www.regione.lazio.it/accessiprontosoccorso/"
# ritardo in secondi
delay<-5
# contatore rilevazioni
contatore<-as.numeric(c(1:4))
# Ripetizioni del ciclo di durata1C
ripetizioni<-as.numeric(c(1:3))
# Durata ciclo
durata1C<-(length(contatore)*delay)/3600
ciclo<-paste("Durata un ciclo: ", durata1C," ore.", sep="")
print(ciclo)
durataRC<-(durata1C*length(ripetizioni))
ripTot<-paste("Durata totale: ",durataRC," ore.",sep="")
print(ripTot)
# Ora di partenza
ora<-0
for (r in ripetizioni) {
   if (r<=length(ripetizioni)) {
      for (i in contatore) {
         if (i<=length(contatore)) {
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
         else if (i<length(contatore) || i!=1) {
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
      write.table(psLazioT4, file="psLazioTot1.csv", sep=",")
      attach(psLazioT4)
      aggdata <-aggregate(psLazioT4, by=list(Struttura), 
                          FUN=mean, na.rm=TRUE)
      #print(aggdata)
      detach(psLazioT4)
      ora<-ora+0.5
      aggdata$Aggiornamento<-ora
      nomeFT<-paste("psLazioT",ora,".csv",sep="")
      write.table(aggdata, file=nomeFT, sep=",")
   }
}
ora<-0
r<-0
for (r in ripetizioni) {
   if (r==1) {
      ora<-ora+0.5
      nomeFT<-paste("psLazioT",ora,".csv",sep="")
      psLazioFT<-read.csv(nomeFT)
   }   
   else if (i<length(ripetizioni) || r!=1) {
      ora<-ora+0.5
      nomeFT<-paste("psLazioT",ora,".csv",sep="")
      psLazioT<-read.csv(nomeFT)
      psLazioFT<-rbind(psLazioFT,psLazioT)
   }
}

psLazioBak<-psLazioFT
write.table(psLazioFT,file="psLazioTot1.csv",sep=",")