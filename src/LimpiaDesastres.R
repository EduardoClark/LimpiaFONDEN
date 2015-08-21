########################
## Clean Reconstruccion
##
##
########################
require(readxl)
require(dplyr)
require(foreign)
require(lubridate)

#Read Desastres
De <- rbind(read_excel(path = "data/FONDEN2014.xlsx",sheet = 1,col_types = c("text","text","date","text","text","numeric")),
            read_excel(path = "data/FONDEN2014.xlsx",sheet = 4,col_types= c("text","text","date","text","text","numeric")))
Fecha2 <- rbind(read_excel(path = "data/FONDEN2014.xlsx",sheet = 1,col_types = c("text","text","text","text","text","numeric")),
      read_excel(path = "data/FONDEN2014.xlsx",sheet = 4,col_types= c("text","text","text","text","text","numeric")))[,3]
De <- cbind(De,Fecha2);remove(Fecha2)
names(De) <- c("NEnt","Evento","Fecha","NMun","Sector","API","Fecha2")

#Separte Muns
De$NMun <- gsub(" y ",replacement = " , ",x = De$NMun)
Muns <- unique(De[,1:4])
Muns <- strsplit(Muns$NMun,",")
ID <- unique(De$Evento)
RowMuns <- function(NumberList){
  i <- NumberList
  TMP <- data.frame(ID=ID[i],Muns = c(Muns[i]))
  names(TMP) <- c("Evento","cve_mun")
  return(TMP)
}
Muns <- rbind_all(lapply(1:length(ID),FUN = RowMuns))
remove(ID,RowMuns)

#JoinDF
De$NMun <- NULL
De <- unique(left_join(De,Muns))
for(i in 1:10){De$cve_mun <- gsub(pattern = "  ",replacement = " ",De$cve_mun)}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
De$cve_mun <- trim(De$cve_mun)
remove(i, Muns,trim)

##Match States
Edo <- read.dbf(file = "MunsData/mge2013v6_2.dbf",as.is = TRUE)
Edo$NOM_ENT <- iconv(Edo$NOM_ENT,from = "ISO 8859-1","utf-8")
Edos <- data.frame(NOM_ENT=unique(De$NEnt))
Edo$NOM_ENT <- gsub("México","Estado de México",Edo$NOM_ENT)
Edos <- full_join(Edo,Edos)
EdoLeft <- Edos[is.na(Edos)==TRUE,][,2]
MatchEstado <- function(Estado){
  TMP <- agrep(pattern = Estado,Edo$NOM_ENT,value=TRUE)
  TMP <- data.frame(NOMVIEJO=Estado,NOM_ENT=TMP)
  return(TMP)
}
EdosNonMatch <- rbind_all(lapply(EdoLeft,MatchEstado)) 
Edos <- Edos[is.na(Edos)==FALSE,]
EdoLeft <- left_join(EdosNonMatch,Edo)
EdoLeft$NOM_ENT <- NULL
names(EdoLeft)[1] <- "NOM_ENT"
Edos <- rbind(Edos,EdoLeft)
Edos <- Edos[is.na(Edos$NOM_ENT)==FALSE,]
Edos <- arrange(Edos,CVE_ENT)
remove(EdoLeft,Edo,EdosNonMatch,MatchEstado)
write.csv(Edos,"data-out/Estados.csv",row.names=FALSE)

##Match Muns
MunsO <- read.dbf(file = "MunsData/mgm2013v6_2.dbf",as.is = TRUE)
MunsO$NOM_MUN <- iconv(MunsO$NOM_MUN,from = "ISO 8859-1","utf-8")
Muns <- unique(De[,c(1,7)])
names(Muns)[1] <- "NOM_ENT"
Muns <- left_join(Muns,Edos)
Muns2 <- left_join(Muns,MunsO,by=c("CVE_ENT","cve_mun"="NOM_MUN"))
MunsLeft <- Muns2[is.na(Muns2$CVE_MUN)==TRUE,][,2:3]
MunsLeft <- MunsLeft[MunsLeft$cve_mun!="S",]
MatchEstado <- function(Mun,Estado){
  TMP1 <- MunsO[MunsO$CVE_ENT==Estado,]
  TMP <- agrep(pattern = Mun,TMP1$NOM_MUN,value=TRUE)
  TMP <- data.frame(NOMVIEJO=Mun,NOM_MUN=TMP)
  return(TMP)
}
MunsLeft$cve_mun <- gsub("\\(Archipiélago de Revillagigedo\\)","",MunsLeft$cve_mun)
MunsLeft$cve_mun <- gsub("Isla Clarión en el Archipiélago de Revillagigedo","Manzanillo",MunsLeft$cve_mun)
MunsLeft$cve_mun <- gsub("Coyuca de Ca","Coyuca de Catal",MunsLeft$cve_mun)
MunsNonMatch <- c()
for(i in 1:nrow(MunsLeft)){
  print(i)
  TMP <- MatchEstado(MunsLeft[i,1],MunsLeft[i,2])
  MunsNonMatch <- rbind(MunsNonMatch,TMP)
}
Muns2 <- Muns2[is.na(Muns2$CVE_MUN)==FALSE,]
MunsLeft <- left_join(MunsNonMatch,MunsO)
names(Muns2) <- c("NEnt","cve_mun","CVE_ENT","CVE_MUN","CVE")
names(MunsLeft) <- c("cve_mun","NOM_MUN","CVE_ENT","CVE_MUN","CVE")
MunsLeft <- left_join(MunsLeft,Edos)
names(MunsLeft) <- c("cve_mun","NOM_MUN","CVE_ENT","CVE_MUN","CVE","NEnt")
MunsLeft$NOM_MUN <- NULL
Muns2 <- rbind(Muns2,MunsLeft)
remove(Edos,Muns,MunsLeft,MunsLeft,MunsO,TMP,TMP1,Estado,Mun,i,MatchEstado,MunsNonMatch)
De2 <- left_join(De,Muns2)
names(De2) <- c("NOM_ENT","Evento","Fecha","Sector","MontoAPI","Fecha2","NOM_MUN","CVE_ENT","CVE_MUN","CVE")
remove(De,Muns2)
De3 <- De2 %>% select(Evento,Fecha,CVE,NOM_ENT,CVE_ENT,NOM_MUN,CVE_MUN,Sector,MontoAPI,Fecha2)
De3 <- De3 %>% mutate(Fecha2=ifelse(grepl(":",x = Fecha2)==TRUE,
                                    substr(Fecha2,regexpr(":",Fecha2)+1,regexpr(":",Fecha2)+10),
                                    substr(Fecha2,regexpr(" y ",Fecha2)+3,1000)),Fecha2 = dmy(Fecha2))

De3$Fecha <- ymd(ifelse(is.na(De3$Fecha)==TRUE,as.character(De3$Fecha2),as.character(De3$Fecha)) )
De3$Fecha2 <- NULL
write.csv(De3,"data-out/Desastres.csv",row.names=FALSE)
remove(list=ls())
