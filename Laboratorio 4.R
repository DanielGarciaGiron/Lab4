#------------------------------------------------------------#
# Laboratorio 4
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("stringr","tm", "R.utils","openNLP","rJava","qdap","tm","RWeka")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#---------------------- Lectura de Datos --------------------------------------------#
TwitterData <- "./data/en_US.twitter.txt"
Twitter <- scan(TwitterData, character(0), sep = "\n") 
lineasTwitter<-as.numeric(countLines(TwitterData))


NewsData <- "./data/en_US.news.txt"
News <- scan(NewsData, character(0), sep = "\n") 
lineasNews<-as.numeric(countLines(NewsData))


BlogsData <- "./data/en_US.blogs.txt"
Blogs <- scan(BlogsData, character(0), sep = "\n") 
lineasBlogs<-as.numeric(countLines(BlogsData))

#Se despliegan las cantidades de líneas para ver con cuantos datos se cuenta
lineasTwitter
lineasNews
lineasBlogs

#Debido al gran tamaño de los datos, se debe tomar una pequeña muestra para procesarlos
#Primero, se usa set.seed para que sea reproducible la obtención de la muestra aleatoria
set.seed(123)

#Solo de usan 5% de los datos debido a la gran cantidad de lineas con las que se cuenta
Porciento <- 5/100

#Obtención de las muestras de Tweets, noticias y blogs
MuestraTwitter <- sample(lineasTwitter, Porciento*lineasTwitter)

MuestraNews <- sample(lineasNews, Porciento*lineasNews)

MuestraBlogs <- sample(lineasBlogs, Porciento*lineasBlogs)

#---------------------- Limpieza y Preprocesamiento ---------------------------------#
#Primero, se unifican los datos en un solo objeto
Datos <- paste(MuestraTwitter,MuestraNews,MuestraBlogs)  
Datos <- sent_detect(Datos, language = "en", model = NULL) 

#Se realiza un vector de los datos y se convierten en volátiles para cambiar su contenido
VectorDatos <- VectorSource(Datos)
DatosLimpios <- VCorpus(VectorDatos)

#Preparar transformación
DatosLimpios <- tm_map(DatosLimpios,  content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

#Se transforman los caracteres a minúsculas
DatosLimpios<-tm_map(DatosLimpios, content_transformer(tolower))

#Se eliminan los espacios en blanco adicionales
DatosLimpios<-tm_map(DatosLimpios, stripWhitespace)

#Se eliminan las puntuaciones y símbolos
DatosLimpios<-tm_map(DatosLimpios, removePunctuation)

#Se eliminan los números para que no interfieran con la predicción
DatosLimpios<-tm_map(DatosLimpios, removeNumbers)

#Se eliminan artículos, preposiciones y conjunciones (stopwords)
DatosLimpios<-tm_map(DatosLimpios, removeWords, stopwords('english'))