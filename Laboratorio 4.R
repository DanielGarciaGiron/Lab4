#------------------------------------------------------------#
# Laboratorio 4
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("stringr","tm", "R.utils","openNLP","rJava","qdap","RWeka","ggplot2","stylo")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#---------------------- Lectura de Datos --------------------------------------------#
TwitterData <- "./data/en_US.twitter.txt"
Twitter <- scan(TwitterData, character(0), sep = "\n", encoding="latin1", skipNul = TRUE)
LineasTwitter<-as.numeric(countLines(TwitterData))


NewsData <- "./data/en_US.news.txt"
News <- scan(NewsData, character(0), sep = "\n", encoding="latin1", skipNul = TRUE)
LineasNews<-as.numeric(countLines(NewsData))


BlogsData <- "./data/en_US.blogs.txt"
Blogs <- scan(BlogsData, character(0), sep = "\n", encoding="latin1", skipNul = TRUE)
LineasBlogs<-as.numeric(countLines(BlogsData))

#Se despliegan las cantidades de líneas para ver con cuantos datos se cuenta
LineasTwitter
LineasNews
LineasBlogs

#Debido al gran tamaño de los datos, se debe tomar una pequeña muestra para procesarlos
#Primero, se usa set.seed para que sea reproducible la obtención de la muestra aleatoria
set.seed(123)

#Solo de usan 5% de los datos debido a la gran cantidad de lineas con las que se cuenta
Porciento <- 5/100

#Obtención de las muestras de Tweets, noticias y blogs
MuestraTwitter <- sample(Twitter, Porciento*LineasTwitter)

MuestraNews <- sample(News, Porciento*LineasNews)

MuestraBlogs <- sample(Blogs, Porciento*LineasBlogs)

#---------------------- Limpieza y Preprocesamiento ---------------------------------#
#Primero, se unifican los datos en un solo objeto
Datos <- paste(MuestraTwitter,MuestraNews,MuestraBlogs) 
Datos <- sent_detect(Datos, language = "en", model = NULL)

#Se realiza un vector de los datos y se convierten en volátiles para cambiar su contenido
VectorDatos <- VectorSource(Datos)
DatosLimpios <- VCorpus(VectorDatos)

#Preparar transformación
DatosLimpios <- tm_map(DatosLimpios, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

#Se transforman los caracteres a minúsculas
DatosLimpios<-tm_map(DatosLimpios, content_transformer(tolower))

#Se eliminan los espacios en blanco adicionales
DatosLimpios<-tm_map(DatosLimpios, content_transformer(stripWhitespace))

#Se eliminan los URLs, se detiene al encontrar un espacio
RemoverURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
DatosLimpios <- tm_map(DatosLimpios, RemoverURL)
Espacio <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
DatosLimpios <- tm_map(DatosLimpios, Espacio, "/")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "@")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "\\|")

#Se eliminan las puntuaciones y símbolos
DatosLimpios<-tm_map(DatosLimpios, content_transformer(removePunctuation))

#Se eliminan los números para que no interfieran con la predicción
DatosLimpios<-tm_map(DatosLimpios, content_transformer(removeNumbers))

#Se eliminan artículos, preposiciones y conjunciones (stopwords)
DatosLimpios<-tm_map(DatosLimpios, removeWords, stopwords('english'))

#Eliminación de "malas palabras" por medio de una lista de estas
MP <- file("./data/profanities.txt", "r")
MPVector <- VectorSource(readLines(MP))
DatosLimpios <- tm_map(DatosLimpios, removeWords, MPVector)

#------------------------ Tokenización y ngramas -----------------------------------#

#Tokenización de Unigramas
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))

#Tokenización de Bigramas
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

#Tokenización de Trigramas
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

#Función para obtener las frecuencias de cada palabra
Frecuencia <- function(tdm){
  Frec <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  Frecuencia <- data.frame(word=names(Frec), Frec=Frec)
  return(Frecuencia)
}

#Remoción de unigramas que reaparecen menos de una vez
Unigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = UnigramTokenizer)), 0.998)

#Se obtienen las frecuencias de los unigramas
FrecUnigrama <- Frecuencia(Unigrama)

#Remoción de bigramas que reaparecen menos de una vez
Bigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = BigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los bigramas
FrecBigrama <- Frecuencia(Bigrama)

#Remoción de trigramas que reaparecen menos de una vez
Trigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = TrigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los trigramas
FrecTrigrama <- Frecuencia(Trigrama)

#Función para graficación de los datos
PlotFrec <- function(data, title) {
  ggplot(data[1:25,], aes(reorder(word, -Frec), Frec)) +
    labs(x = "Palabras/Frases", y = "Frecuencia") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
    geom_bar(stat = "identity")
}

#--------------------------- Gráficas de frecuencia --------------------------------------#

#Unigramas
PlotFrec(FrecUnigrama, "Unigramas más comúnes (Top 25)")

#Bigramas
PlotFrec(FrecBigrama, "Bigramas más comúnes (Top 25)")

#Trigramas
PlotFrec(FrecTrigrama, "Trigramas más comúnes (Top 25)")



predictNextWord <- function(input)
{
  wordInput <- cleanInput(input)
  wordCount <- length(wordInput)
  prediction <- c()
  
  if(wordCount>3)
  {
    wordInput <- wordInput[(wordCount-2):wordCount]
    prediction <- matchinFourGranm(wordInput[1],wordInput[2],wordInput[3])
  }
  if(wordCount ==2)
  {
    prediction <- matchThreeGram(wordInput[1],wordInput[2])
  }
  if(wordCount ==1)
  {
    prediction <- matchTwoGram(wordInput[1])
  }
  
  if(wordCount == 0)
  {
    prediction <- "Casilla vacia"
  }
  
  if(length(prediction)==0)
  {
    prediction <- "No se encontraron resultados"
  }
  
  if(length(prediction) < 5)
  {
    prediction
  }
  else
  {
    prediction[1:5]
  }
}


cleanInput <- function(text){
  textInput <- tolower(text)
  textInput <- removePunctuation(textInput)
  textInput <- removeNumbers(textInput)
  textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
  textInput <- stripWhitespace(textInput)
  textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
  return(textInput)
}



matchThreeGram <- function(inputWord1,inputWord2)
{
  predictWord <- filter(Trigrama,( word1 == inputWord1 & word2 == inputWord2))$word3
  if(length(predictWord)==0)
  {
    predictWord <- filter(Trigrama,(word2 == inputWord2))$word3 
    
    if(length(predictWord)== 0)
    {
      predictWord <- filter(Trigrama,(word1 == inputWord2))$word2 
      
      if(length(predictWord) ==0 )
      {
        predictWord <- matchTwoGram(inputWord2)
      }
      
    }
  }
  predictWord
}


matchTwoGram <- function(inputWord1)
{
  predictWord <- filter(Bigrama,( word1 == inputWord1 ))$word2
  
  predictWord
  
}
