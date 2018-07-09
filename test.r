
dataset <- read.csv('Capturas_2018.csv',fileEncoding='UTF-8')

#First
library(dplyr)
#Second
library(plyr)

risaralda <- filter(dataset,dataset$Departamento== 'RISARALDA')

policias <- filter(dataset,dataset$Profesión == 'POLICIAS')
delitos<- select(policias,Código.DANE)
delitos <- count(delitos)



dias <- select(risaralda,D�.a)
dias <- count(dias)

delitos<- select(risaralda,Código.DANE)
delitos <- count(delitos)
profesiones<- select(dataset,Profesión)

profesiones <- count(profesiones)
pereira <- filter(risaralda, Municipio == 'PEREIRA (CT)')
lugares <- select(pereira,Barrio,Código.DANE)
lugares <- count(lugares)


dias <- select(dataset,D�.a)
dias <- count(dias)

t <- select(dataset,Departamento,Código.DANE)
t <- count(t)