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
De <- rbind(read_excel(path = "data/FONDEN2014.xlsx",sheet = 2),
            read_excel(path = "data/FONDEN2014.xlsx",sheet = 3))
names(De) <- gsub(" ","",names(De))

#Separte Muns
De$nombre_del_municipio <- gsub(" y ",replacement = " , ",x = De$nombre_del_municipio)
De$nombre_del_municipio <- gsub(" Y ",replacement = " , ",x = De$nombre_del_municipio)
De$nombre_del_municipio <- gsub(" e ",replacement = " , ",x = De$nombre_del_municipio)
Muns <- unique(De[,c(2,5,7)])
Muns <- strsplit(Muns$nombre_del_municipio,",")
ID <- unique(De$numero_de_boletin)
RowMuns <- function(NumberList){
  i <- NumberList
  TMP <- data.frame(ID=ID[i],Muns = c(Muns[i]))
  names(TMP) <- c("Evento","cve_mun")
  return(TMP)
}
Muns <- rbind_all(lapply(1:length(ID),FUN = RowMuns))
remove(ID,RowMuns)

#JoinDF
De$nombre_del_municipio <- NULL
De <- unique(left_join(De,Muns,by = c("numero_de_boletin" = "Evento")))
for(i in 1:10){De$cve_mun <- gsub(pattern = "  ",replacement = " ",De$cve_mun)}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
De$cve_mun <- trim(De$cve_mun)
remove(i, Muns,trim)


##Match States
Edos <- read.csv("data-out/Estados.csv",stringsAsFactors=FALSE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
De$nombre_de_la_entidad <- trim(De$nombre_de_la_entidad)
Edos$NOM_ENT <- gsub("Michoacán de Ocampo","Michoacán",Edos$NOM_ENT)
De$nombre_de_la_entidad <- gsub("Michoacan","Michoacán",De$nombre_de_la_entidad)
De$nombre_de_la_entidad <- gsub("San Luis Potosi","San Luis Potosí",De$nombre_de_la_entidad)
De1 <- left_join(De,Edos,by=c("nombre_de_la_entidad"="NOM_ENT"))
remove(trim,De)

##Match Muns
MunsO <- read.dbf(file = "MunsData/mgm2013v6_2.dbf",as.is = TRUE)
MunsO$NOM_MUN <- iconv(MunsO$NOM_MUN,from = "ISO 8859-1","utf-8")
Muns <- unique(De1[,c(54,55)])
names(Muns)[1] <- "NOM_MUN"
Muns <- left_join(Muns,Edos)
MunsO$CVE_ENT <- as.numeric(MunsO$CVE_ENT)
Muns2 <- left_join(Muns,MunsO,by=c("CVE_ENT","NOM_MUN"))
MunsLeft <- Muns2[is.na(Muns2$CVE_MUN)==TRUE,][,1:2]
MunsLeft <- MunsLeft[MunsLeft$NOM_MUN!="",]
MatchEstado <- function(Mun,Estado){
  Estado <- as.character(Estado)
  TMP1 <- MunsO[MunsO$CVE_ENT==Estado,]
  TMP <- agrep(pattern = Mun,TMP1$NOM_MUN,value=TRUE)
  TMP <- data.frame(NOMVIEJO=Mun,NOM_MUN=TMP)
  return(TMP)
}
# MunsLeft$cve_mun <- gsub("\\(Archipiélago de Revillagigedo\\)","",MunsLeft$cve_mun)
# MunsLeft$cve_mun <- gsub("Isla Clarión en el Archipiélago de Revillagigedo","Manzanillo",MunsLeft$cve_mun)
# MunsLeft$cve_mun <- gsub("Coyuca de Ca","Coyuca de Catal",MunsLeft$cve_mun)
MunsLeft$NOM_MUN <- gsub("Parras.","Parás",MunsLeft$NOM_MUN)
MunsLeft$CVE_ENT <- ifelse(MunsLeft$NOM_MUN=="Ensenada",2,MunsLeft$CVE_ENT)
MunsLeft <- MunsLeft[-26,]
MunsLeft$NOM_MUN <- gsub("Dr. Arroyo","Doctor Arroyo",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("San Pedro Mixtepec Distrito 22","San Pedro Mixtepec",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("San Juan Mixtepec Distrito 26","San Juan Mixtepec",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("San Pedro Mixtepec Distrito 26","San Pedro Mixtepec",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("El Parral","Villa Corzo",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("Magdalena de Quino","Magdalena",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("San Pedro Mixtepec -Dto. 22","San Pedro Mixtepec",MunsLeft$NOM_MUN)
MunsLeft$NOM_MUN <- gsub("San Pedro Mixtepec -Dto. 26","San Pedro Mixtepec",MunsLeft$NOM_MUN)

MunsNonMatch <- c()
for(i in 1:nrow(MunsLeft)){
  print(i)
  TMP <- MatchEstado(MunsLeft[i,1],MunsLeft[i,2])
  MunsNonMatch <- rbind(MunsNonMatch,TMP)
}
MunsNonMatch <- unique(MunsNonMatch)
names(MunsNonMatch) <- c("NOMVIEJO","NOM_MUN")


Muns2 <- Muns2[is.na(Muns2$CVE_MUN)==FALSE,]
MunsLeft <- left_join(MunsNonMatch,MunsO)
names(Muns2) <- c("NOMVIEJO","CVE_ENT","NOM_ENT","CVE_MUN","CVE")
names(MunsLeft) <- c("NOMVIEJO","NOM_MUN","CVE_ENT","CVE_MUN","CVE")
MunsLeft <- left_join(MunsLeft,Edos)
# names(MunsLeft) <- c("cve_mun","NOM_MUN","CVE_ENT","CVE_MUN","CVE","NEnt")
MunsLeft$NOM_MUN <- NULL
Muns2 <- rbind(Muns2,MunsLeft)
remove(Edos,Muns,MunsLeft,MunsLeft,MunsO,TMP,TMP1,Estado,Mun,i,MatchEstado,MunsNonMatch)
De2 <- left_join(De1,Muns2,by=c("CVE_ENT","cve_mun"="NOMVIEJO"))
# names(De2) <- c("NOM_ENT","Evento","Fecha","Sector","MontoAPI","Fecha2","NOM_MUN","CVE_ENT","CVE_MUN","CVE")
remove(De,Muns2,De1)
De2$NOMVIEJO <- NULL
names(De2)[44] <- "BOLSASPARACADAVER"
De2$numero_de_boletin <- gsub("\\n","",De2$numero_de_boletin)
De2$numero_de_boletin <- gsub("\\r","",De2$numero_de_boletin)
write.csv(De2,"data-out/Emergencias.csv",row.names=FALSE)
