library(dplyr)
## CARGANDO LOS DATOS
capturas <- read.csv('./Files/Capturas_2018.csv',fileEncoding='UTF-8')
armas <- read.csv('./Files/Armas.csv',fileEncoding='UTF-8')
drogas <- read.csv('./Files/estupefacientes.csv',fileEncoding='UTF-8')


## Haciendo filtro sobre risaralda

capturas <- filter(capturas, capturas$Departamento == 'RISARALDA')
armas <- filter(armas, armas$DEPARTAMENTO == 'RISARALDA')
drogas <- filter(drogas, drogas$DEPARTAMENTO == 'RISARALDA')


# FILTRANDO LAS CAPTURAS de PEREIRA,DOSQUEBRADAS y LA VIRGINIA

capturasPereira <- filter(capturas, capturas$Municipio == 'PEREIRA (CT)')
capturasDsq <- filter(capturas, capturas$Municipio == 'DOSQUEBRADAS')
capturasVir <- filter(capturas, capturas$Municipio == 'LA VIRGINIA')

capPEIOnlyDrugs <- filter(capturasPereira, capturasPereira$Código.DANE=='ARTÍCULO 376. TRÁFICO, FABRICACIÓN O PORTE DE ESTUPEFACIENTES')
capDSQOnlyDrugs <- filter(capturasDsq, Código.DANE=='ARTÍCULO 376. TRÁFICO, FABRICACIÓN O PORTE DE ESTUPEFACIENTES')
capVIROnlyDrugs <- filter(capturasVir, capturasVir$Código.DANE=='ARTÍCULO 376. TRÁFICO, FABRICACIÓN O PORTE DE ESTUPEFACIENTES')

# FILTRANDO LAS DROGAS de PEREIRA,DOSQUEBRADAS y LA VIRGINIA
drogasPEI <- filter(drogas,MUNICIPIO == 'PEREIRA (CT)')
drogasDSQ <- filter(drogas,MUNICIPIO == 'DOSQUEBRADAS')
drogasVIR <- filter(drogas,MUNICIPIO == 'LA VIRGINIA')

# FILTRANDO LAS ARMAS de PEREIRA,DOSQUEBRADAS y LA VIRGINIA
armasPEI <- filter(armas,MUNICIPIO == 'PEREIRA (CT)')
armasDSQ <- filter(armas,MUNICIPIO == 'DOSQUEBRADAS')
armasVIR <- filter(armas,MUNICIPIO == 'LA VIRGINIA')


## GUARDANDO LOS ARCHiVOS
write.csv(capturasPereira,file="./Files/Filtrados/capturasPEI.csv",sep=",",fileEncoding = "UTF-8")
write.csv(capturasDsq,file="./Files/Filtrados/capturasDSQ.csv",sep=",",fileEncoding = "UTF-8")
write.csv(capturasVir,file="./Files/Filtrados/capturasVIR.csv",sep=",",fileEncoding = "UTF-8")

write.csv(drogasPEI,file="./Files/Filtrados/drogasPEI.csv",sep=",",fileEncoding = "UTF-8")
write.csv(drogasDSQ,file="./Files/Filtrados/drogasDSQ.csv",sep=",",fileEncoding = "UTF-8")
write.csv(drogasVIR,file="./Files/Filtrados/drogasVIR.csv",sep=",",fileEncoding = "UTF-8")

write.csv(armasPEI,file="./Files/Filtrados/armasPEI.csv",sep=",",fileEncoding = "UTF-8")
write.csv(armasDSQ,file="./Files/Filtrados/armasDSQ.csv",sep=",",fileEncoding = "UTF-8")
write.csv(armasVIR,file="./Files/Filtrados/armasVIR.csv",sep=",",fileEncoding = "UTF-8")


c <- data.frame(capturasPereira$Día,capturasPereira$Barrio,capturasPereira$Código.DANE)

onlyWeapons <- c %>% filter(grepl('ARTÍCULO 365',c$capturasPereira.Código.DANE))
sinpermiso <- armasPEI %>% filter(grepl('SIN',armasPEI$PERMISO))

sinpermiso <- data.frame(sinpermiso$DÍA,sinpermiso$CLASE.DE.ARMA)

w<-merge(onlyWeapons,sinpermiso,by.x='Día',by.y='DÍA')
w<-inner_join(onlyWeapons,sinpermiso,by=c('Día','DÍA'))
w <- onlyWeapons %>% inner_join(sinpermiso,by=c('capturasPereira.Día'='sinpermiso.DÍA'))

string <- "12/31/1899 02:30:00 PM"
w<-gsub(substr(string,1,11),"",string)
w<-gsub(substr(string,11,22),"",string)

# Haciendo limpieza al dato fecha y hora, con tal de que en todos los archivos queden en un mismo formato

capturasPereira$Hora <- gsub(substr(capturasPereira$Hora,1,11),"",capturasPereira$Hora)
capturasPereira$Fecha <-gsub(substr(capturasPereira$Fecha,11,22),"",capturasPereira$Fecha)

drogasPEI$FECHA <-gsub(substr(drogasPEI$FECHA,11,22),"",drogasPEI$FECHA)

capturasDPEI <- filter(capturasPereira, capturasPereira$Código.DANE=='ARTÍCULO 376. TRÁFICO, FABRICACIÓN O PORTE DE ESTUPEFACIENTES')


w<-merge(capturasDPEI,drogasPEI,by.x=c('Fecha','Día','Zona','Clase.de.sitio'),by.y=c('FECHA','DIA','ZONA','CLASE.DE.SITIO'),all.y = FALSE)

#CREAR un subset
library(caTools)
set.seed(123)
split <- sample.split(dataset$Edad, SplitRatio = 0.75)
training_set <- subset(dataset,split==TRUE)
test_set <- subset(dataset,split==FALSE)
