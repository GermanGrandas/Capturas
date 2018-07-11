#Haciendo pruebas con un Decision Tree

library(rpart)
library(rpart.plot)
library(dplyr)

capturas <- read.csv('./Files/Filtrados/capturasPEI.csv',fileEncoding='UTF-8')
capturas$Hora <- gsub(substr(capturas$Hora,1,11),"",capturas$Hora)
capturas$Fecha <-gsub(substr(capturas$Fecha,11,22),"",capturas$Fecha)
dataset <- capturas[c(10:13,17)]

dataset$Día <- factor(dataset$Día, levels=unique(dataset$Día),labels=c(1,2,3,4,5,6,7))


dataset$Barrio <-factor(dataset$Barrio, levels=unique(dataset$Barrio),labels=1:length(unique(dataset$Barrio)))



dataset$New.DANE <-factor(dataset$Código.DANE, levels=unique(dataset$Código.DANE),labels=1:length(unique(dataset$Código.DANE)))


datos <- dataset[c(1:4,6)]

daneCodes<- dataset[5:6]

daneCodes <- unique(daneCodes)

hombres <- dataset %>% filter(Sexo == 'MASCULINO')
hombres <- hombres[c(1,3:6)]

mujeres <- dataset %>% filter(Sexo == 'FEMENINO')
mujeres <- mujeres[c(1,3:6)]


delitosMujeres <- mujeres %>% count(Código.DANE)
delitosHombres <- hombres %>% count(Código.DANE)

classifier <- rpart(Edad~.,data = datos) #Funcionó
rpart.plot(classifier,main='Hombres y Mujeres')


datosH <- hombres[c(1:3,5)]
classifierH <- rpart(Edad~.,data = datosH)
rpart.plot(classifierH,main='Hombres')

datosM <- mujeres[c(1:3,5)]
classifierM <- rpart(Edad~.,data = datosM)
rpart.plot(classifierM,main='Mujeres')

dataset <- capturas[c(5,7:9,17)] 

#Haciendo un filtro según su zona
urbana <- filter(dataset, dataset$Zona == 'URBANA')
rural <- filter(dataset, dataset$Zona == 'RURAL')


urbana$Barrio <-factor(dataset$Barrio, levels=unique(dataset$Barrio),labels=1:length(unique(dataset$Barrio)))
dataset$Clase.de.sitio <-factor(dataset$Clase.de.sitio, levels=unique(dataset$Clase.de.sitio),labels=1:length(unique(dataset$Clase.de.sitio)))
dataset$New.DANE <-factor(dataset$Código.DANE, levels=unique(dataset$Código.DANE),labels=1:length(unique(dataset$Código.DANE)))

dataset$Zona <-factor(dataset$Zona, levels=unique(dataset$Zona),labels=1:length(unique(dataset$Zona)))
datos <- dataset[c(1:4,6)]


classifier <- rpart(Barrio~New.DANE+Zona, data=datos)

rpart.plot(classifier)

# TRabajando con el archivo modificado de Mauricio
capturas <- read.csv('./Files/CapMauro.csv',fileEncoding='UTF-8')
capturas$Fecha <-gsub(substr(capturas$Fecha,11,22),"",capturas$Fecha)
dataset <- capturas[c(10:13,17)]
dataset$New.DANE <-factor(dataset$Código.DANE, levels=unique(dataset$Código.DANE),labels=1:length(unique(dataset$Código.DANE)))

hombres <- dataset %>% filter(Sexo == 'MASCULINO')
hombres <- hombres[c(1,3:6)]

mujeres <- dataset %>% filter(Sexo == 'FEMENINO')
mujeres <- mujeres[c(1,3:6)]

perMujeres <- 148/1114 #---> 0.13285
perHombres <- 966/1114 #---> 0.86715

delitosMujeres <- mujeres %>% count(Código.DANE)
delitosHombres <- hombres %>% count(Código.DANE)

#Analizando delitos por fracciones de tiempo
morning <- capturas %>% filter(Zona == 'URBANA') %>%filter(Hora =='mañana')

#Analizando las capturas relacionadas a Mujeres
mujeres <- morning %>% filter(Sexo =='FEMENINO')
delitosMujeres <- mujeres %>% count(Código.DANE)

frame <- mujeres[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))


classifier <- rpart(EncBarrios~New.DANE,data = frame)
summary(classifier)

rpart.plot(classifier)

barrioCodes <- frame[c(1,5)]
barrioCodes <- unique(frame[c(1,5)])

DaneCodes <- unique(frame[3:4])

#Analizando las capturas relacionadas a Hombres

hombres <- morning %>% filter(Sexo =='MASCULINO')
frame <- hombres[c(7,9,17)]
frame$New.DANE <-factor(frame$Código.DANE, levels=unique(frame$Código.DANE),labels=1:length(unique(frame$Código.DANE)))
frame$EncBarrios <- factor(frame$Barrio, levels=unique(frame$Barrio),labels=1:length(unique(frame$Barrio)))
frame$NewSitio <- factor(frame$Clase.de.sitio, levels=unique(frame$Clase.de.sitio),labels=1:length(unique(frame$Clase.de.sitio)))

#Desviación Estandar variable EncBarrios




indices <- sample(1:nrow(frame),60)
muestra <- frame[ indices, ]
set.seed(123)
classifier <- rpart(New.DANE~EncBarrios,data = muestra) #demorado
x0<-proc.time()
classifier <- rpart(EncBarrios~New.DANE,data = muestra)
x1<-proc.time()
rpart.plot(classifier)
x1-x0

barrioCodes <- frame[c(1,5)]
barrioCodes <- unique(barrioCodes)

DaneCodes <- unique(frame[3:4])

#Probando con Random Forest
install.packages('randomForest')
library(randomForest)
set.seed(123)

classifier <- randomForest(EncBarrios~Código.DANE,data = frame)

library(ggraph)
library(igraph)

getTree(classifier, 1, labelVar=TRUE)
tree <- getTree(classifier, 1, labelVar=TRUE) %>% tibble::rownames_to_column() %>%mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))

graph_frame <- data.frame(from = rep(tree$rowname, 2),to = c(tree$`left daughter`, tree$`right daughter`))
graph <- graph_from_data_frame(graph_frame) %>%delete_vertices("0")

# set node labels
V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
V(graph)$leaf_label <- as.character(tree$prediction)
V(graph)$split <- as.character(round(tree$`split point`, digits = 2))

plot <- ggraph(graph, 'dendrogram') + 
  theme_bw() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
  geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
  geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE)

print(plot)

plot(classifier)
