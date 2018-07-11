library(rpart)
library(rpart.plot)
library(dplyr)

capturas <- read.csv('./Files/CapMauro.csv',fileEncoding='UTF-8')
capturas$Fecha <-gsub(substr(capturas$Fecha,11,22),"",capturas$Fecha)

urbanas <- capturas %>% filter(Zona == 'URBANA') 

hombres <- urbanas %>% filter(Sexo == 'MASCULINO')
mujeres <- urbanas %>% filter(Sexo == 'FEMENINO')

perMujeres <- 131/957 #---> 0.13689
perHombres <- 826/957 #---> 0.86311


#-------------------------------------------------
#------------- MUJERES ---------------------------
#-------------------------------------------------

#delitos cometidos en horas de la mañana por mujeres
morning <- mujeres %>% filter(Hora =='mañana')

frame <- morning[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))

barrioCodesM <- unique(frame[c(1,5)])

DaneCodesM <- unique(frame[3:4])

classifier <- rpart(EncBarrios~New.DANE,data = frame)
rpart.plot(classifier,main='Barrios/Delito por mujeres Mañana',extra=100,type=5)

#delitos cometidos en horas de la tardes por mujeres
tarde <- mujeres %>% filter(Hora =='tarde')

frame <- tarde[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))


classifier <- rpart(EncBarrios~New.DANE,data = frame)
rpart.plot(classifier,main='Barrios/Delito por mujeres tarde',extra=100,box.palette=0,type=5)

barrioCodesT <- unique(frame[c(1,5)])

DaneCodesT <- unique(frame[3:4])

#delitos cometidos en horas de la noche por mujeres
noche <- mujeres %>% filter(Hora =='noche')

frame <- noche[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))


classifier <- rpart(EncBarrios~New.DANE,data = frame)
rpart.plot(classifier,main='Barrios/Delito por mujeres noche',extra=100,box.palette=0,type=5)

barrioCodesN <- unique(frame[c(1,5)])

DaneCodesN <- unique(frame[3:4])


#-------------------------------------------------
#------------- HOMBRES ---------------------------
#-------------------------------------------------

#delitos cometidos en horas de la mañana por hombres
morning <- hombres %>% filter(Hora =='mañana')



frame <- morning[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))

barrioCodesM <- unique(frame[c(1,5)])

DaneCodesM <- unique(frame[3:4])

#Seleccionando una muestra de 158 para un error de 5%
indices <- sample(1:nrow(frame),158)
muestra <- frame[ indices, ]

classifier <- rpart(EncBarrios~New.DANE,data = muestra)
rpart.plot(classifier,main='Barrios/Delito por hombres Mañana')

#delitos cometidos en horas de la tardes por hombres
tarde <- hombres %>% filter(Hora =='tarde')

frame <- tarde[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))

#Seleccionando una muestra de 170 para un error de 5%
indices <- sample(1:nrow(frame),170)
muestra <- frame[ indices, ]

classifier <- rpart(EncBarrios~New.DANE,data = muestra)
rpart.plot(classifier,main='Barrios/Delito por hombres tarde',extra=100,box.palette=0,type=5)


barrioCodesT <- unique(frame[c(1,5)])

DaneCodesT <- unique(frame[3:4])

#delitos cometidos en horas de la noche por hombres
noche <- hombres %>% filter(Hora =='noche')

frame <- noche[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))

#Seleccionando una muestra de 155 para un error de 5%
indices <- sample(1:nrow(frame),155)
muestra <- frame[ indices, ]

classifier <- rpart(EncBarrios~New.DANE,data = muestra)
rpart.plot(classifier,main='Barrios/Delito por hombres noche',extra=100,box.palette=0,type=5)


barrioCodesN <- unique(frame[c(1,5)])

DaneCodesN <- unique(frame[3:4])