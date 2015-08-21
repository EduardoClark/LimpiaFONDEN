#Emergencia 
Em <- read.csv("data/fonden/Emergencias.csv",stringsAsFactors = FALSE)
Em$Número <- NULL
Em <- Em[,c(53:57,1:6,7:52)]
Em$nombre_de_la_entidad <- NULL
names(Em)[1:5] <- c("nom_mun","cve_ent","nom_ent","cve_mun","cve")
Em$fecha_final_del_evento <- NULL
names(Em)[8] <- "fecha"
names(Em)[6] <- "descripcion"
names(Em) <- tolower(names(Em))
names(Em) <- gsub(pattern ="\\.",replacement =   "_",x=names(Em))
Em <- Em[,c(5,2,3,4,1,8,7,6,9:55)]

#Desastre
De <- read.csv("data/fonden/Desastres.csv",stringsAsFactors = FALSE)
De <- De[,c(3,5,4,7,6,1,2,8,9)]
names(De) <- tolower(names(De))
names(De)[6] <- "descripcion"
require(dplyr)
require(tidyr)
De$montoapi <- ifelse(is.na(De$montoapi)==TRUE,0, De$montoapi)
De <- unique(De)
De <- De %>% spread(sector,montoapi)
names(De) <- gsub(" ","_",names(De))
names(De) <- tolower(names(De))
for(i in 8:28){
  De[,i] <- ifelse(is.na(De[,i])==TRUE,0,De[,i])
}
remove(i)
names(De)[9] <- "areas_naturales_protegidas"
names(De)[15] <- "hidraulico"
names(De)[18] <- "monumentos_arqueologicos"
names(De)[25] <- "turistico"
names(De)[21] <- "pesquero_y_acuicola"
names(De)[23] <- "residuos_solidos"

#Reconstruccion
Re <- read.csv("data/fonden/Recontruccion.csv",stringsAsFactors = FALSE)
Re <- Re[,c(8,6,5,8,7,2,3,1,4,9:31)]
names(Re)[1:5] <- names(De)[1:5]
Re$cve_mun <- substr(x = formatC(Re$cve_mun,width = 5,flag = 0),3,5)
Re$descripcion <- paste(Re$EVENTO,Re$NOTAEVENTO,sep=" ")
Re <- Re[,c(1:5,33,9,6,8,10,11,14,15,16,17,18,20:32)]
names(Re) <- tolower(gsub(" ","_",x = gsub(pattern = "\\.","_",names(Re))))
names(Re)[7] <- "fecha"
require(lubridate)
Re$fecha <- dmy(Re$fecha)
names(Re)[9] <- "clave_proyecto"

#Dataout
write.csv(De,file = "data-out/Desastres.csv",row.names = FALSE)
write.csv(Em,"data-out/Emergencias.csv",row.names=FALSE)
write.csv(Re,"data-out/Recontruccion.csv",row.names=FALSE)
