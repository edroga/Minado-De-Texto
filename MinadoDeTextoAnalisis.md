Untitled
================

Esta parte solo la cargamos si tenemos problemas con Java para la libreria qdap

Versión con gráficos <http://rpubs.com/ilan_rg/minadodetexto>

``` r
if(Sys.getenv("JAVA_HOME")!=""){
  Sys.setenv(JAVA_HOME="")
}
```

------------------------------------------------------------------------

Asignamos las librerias
=======================

------------------------------------------------------------------------

``` r
library(rJava)
library(qdap)
library(tm)
library(readxl)
library(dplyr)
library(wordcloud)
library(metricsgraphics)
library(ggplot2)
library(plotly)
library(dendextend)
library(RWeka)
library(wordcloud2)
```

------------------------------------------------------------------------

Asignamos el directorio donde se encuentran los archivos insumo

------------------------------------------------------------------------

``` r
#setwd("C:/Documents")
```

------------------------------------------------------------------------

Cargamos los datos

------------------------------------------------------------------------

``` r
Resultados<-read_excel("Mexico Propone.xlsx",
                       sheet=1, 
                       skip=0,
                       col_names = TRUE)
```

------------------------------------------------------------------------

Obtenemos el nombre de las variables

------------------------------------------------------------------------

``` r
names(Resultados)
```

------------------------------------------------------------------------

Nos quedamos solo con las primeras 3 columnas

------------------------------------------------------------------------

``` r
Resultados<-Resultados[c(1:3)]
#renombrarmos las columnas
names(Resultados)<-c("PrincipalPreocupacion","QueNecesitasAprender","QuePropones")
#Quitamos todas las observaciones que tienen NAs
Resultados<-filter(Resultados, !is.na(PrincipalPreocupacion) & 
                               !is.na(QueNecesitasAprender) & 
                               !is.na(QuePropones))
```

------------------------------------------------------------------------

Creamos un corpus para cada pregunta
====================================

------------------------------------------------------------------------

``` r
PrincipalPreocupacion<-VCorpus(VectorSource(Resultados$PrincipalPreocupacion))
QueNecesitasAprender<-VCorpus(VectorSource(Resultados$QueNecesitasAprender))
QuePropones<-VCorpus(VectorSource(Resultados$QuePropones))
```

------------------------------------------------------------------------

Estandarizamos las stopwords
============================

------------------------------------------------------------------------

``` r
stopsespa<-stopwords("spanish")
stopsespa<-gsub("á", "a", stopsespa)    #ELIMINAMOS ACENTUACION
stopsespa<-gsub("é", "e", stopsespa)
stopsespa<-gsub("í", "i", stopsespa)
stopsespa<-gsub("ó", "o", stopsespa)
stopsespa<-gsub("ú", "u", stopsespa)
```

------------------------------------------------------------------------

Creamos una funcion para estandarizar y limpiar la corpora
==========================================================

------------------------------------------------------------------------

``` r
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "á", replacement = "a", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "é", replacement = "e", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "í", replacement = "i", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "ó", replacement = "o", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "ú", replacement = "u", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "redes sociales", replacement = "redessociales", fixed=TRUE)
  corpus <- tm_map(corpus, removeWords, stopsespa)
  corpus <- tm_map(corpus, PlainTextDocument) 
  corpus <- tm_map(corpus, stemDocument, "spanish")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "niña", replacement = "niño", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "padr", replacement = "padres", fixed=TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "papa", replacement = "padres", fixed=TRUE)
  return(corpus)
}
```

------------------------------------------------------------------------

Creamos funcion para obtener la TDM
===================================

------------------------------------------------------------------------

``` r
matriceo<-function(cCorpus){
  cCorpus<-clean_corpus(cCorpus) #limpiamos la corpora
  cCorpus<-TermDocumentMatrix(cCorpus) #Creamos todas las TDMs
  cCorpus<-as.matrix(cCorpus) #Creamos Matrices
  return(cCorpus)
}
```

------------------------------------------------------------------------

Aplicamos la función a la corpora
=================================

------------------------------------------------------------------------

``` r
PrincipalPreocupacion_m <- matriceo(PrincipalPreocupacion)
QueNecesitasAprender_m<-matriceo(QueNecesitasAprender)
QuePropones_m<-matriceo(QuePropones)
```

------------------------------------------------------------------------

Creamos una funcion que recibe una TDA y regresa un DF con frecuencias de los terminos
======================================================================================

------------------------------------------------------------------------

``` r
obtFrec<-function(dfrecS){
  dfrecS<-rowSums(dfrecS) # Calculamos la frecuencia de los terminos
  dfrecS<-sort(dfrecS,decreasing=TRUE) # Ordenamos los terminos por frecuencia
  dfrecS<-data.frame(dfrecS, term = names(dfrecS), num = dfrecS) # Generamos data frames
  
  return(dfrecS)
}
```

------------------------------------------------------------------------

Apliamos la funcion anterior a las matrices

------------------------------------------------------------------------

``` r
df_PrincipalPreocupacion<-obtFrec(PrincipalPreocupacion_m)
df_QueNecesitasAprender <-obtFrec(QueNecesitasAprender_m)
df_QuePropones <-obtFrec(QuePropones_m)
```

------------------------------------------------------------------------

Generamos las frecuencias para cada pregunta
============================================

------------------------------------------------------------------------

``` r
head(df_PrincipalPreocupacion,20) %>%
  mjs_plot(x=num, y=term, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')
```

``` r
head(df_QueNecesitasAprender,20) %>%
  mjs_plot(x=num, y=term, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')
```

``` r
head(df_QuePropones,20) %>%
  mjs_plot(x=num, y=term, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')
```

------------------------------------------------------------------------

Wordclouds para cada pregunta
=============================

------------------------------------------------------------------------

``` r
wordcloud(df_PrincipalPreocupacion$term, 
          df_PrincipalPreocupacion$num,
          max.words = 100, 
          colors = "red")
```

``` r
wordcloud(df_QueNecesitasAprender$term, 
          df_QueNecesitasAprender$num,
          max.words = 100, 
          colors = "red")
```

``` r
wordcloud(df_QuePropones$term, 
          df_QuePropones$num,
          max.words = 100, 
          colors = "red")
```

------------------------------------------------------------------------

Generamos wordclouds interactivas
=================================

------------------------------------------------------------------------

``` r
wordcloud2(df_PrincipalPreocupacion[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

``` r
wordcloud2(df_QueNecesitasAprender[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

``` r
wordcloud2(df_QuePropones[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

------------------------------------------------------------------------

Obtenemos palabras comunes para las 3 preguntas
===============================================

------------------------------------------------------------------------

``` r
encuesta<-c(paste(Resultados$PrincipalPreocupacion, collapse = " "),
            paste(Resultados$QueNecesitasAprender, collapse = " "),
            paste(Resultados$QuePropones, collapse = " "))
# Crea un a VectorSource y convierte el VectorSource a  un corpus
encuesta_c<-VCorpus(VectorSource(encuesta))
#limpiamos el corpus
clean_encuesta<-clean_corpus(encuesta_c)
# creamos all_tdm
all_tdm <- TermDocumentMatrix(clean_encuesta)
# Renombramos las columnas
colnames(all_tdm) <- c("PrincipalPreocupacion", "QueNecesitasAprender","QuePropones")
# creamos all_m
all_m <- as.matrix(all_tdm)
```

------------------------------------------------------------------------

Creamos una commonality cloud para las 3 preguntas
==================================================

------------------------------------------------------------------------

``` r
commonality.cloud(all_m, colors = "steelblue1",
                  max.words = 100)
```

------------------------------------------------------------------------

Creamos comparison clouds
=========================

------------------------------------------------------------------------

``` r
comparison.cloud(all_m[,1:3], colors = c("orange", "blue", "black"), max.words = 40)
```

``` r
comparison.cloud(all_m[,1:2], colors = c("orange", "blue"), max.words = 60)
```

``` r
comparison.cloud(all_m[,2:3], colors = c("orange", "blue"), max.words = 60)
```

``` r
comparison.cloud(all_m[,c(1,3) ], colors = c("orange", "blue"), max.words = 50)
```

------------------------------------------------------------------------

Armamos una matriz de distancias euclidianas y los dendogramas
==============================================================

------------------------------------------------------------------------

Creamos funcion para transformar todo a una matriz

``` r
Dendogrameo<-function(cCorpus){
  cCorpus<-clean_corpus(cCorpus) #limpiamos la corpora
  cCorpus<-TermDocumentMatrix(cCorpus) #Creamos todas las TDMs
  cCorpus<-removeSparseTerms(cCorpus, sparse = 0.95) # quitamos sparcity
  cCorpus<-as.matrix(cCorpus)
  cCorpus<-as.data.frame(cCorpus)
  cCorpus<-dist(cCorpus)
  cCorpus<-hclust(cCorpus)
  cCorpus <- as.dendrogram(cCorpus)
  return(cCorpus)
}
```

------------------------------------------------------------------------

Gráficamos los dendogramas
==========================

------------------------------------------------------------------------

``` r
plot(Dendogrameo(PrincipalPreocupacion))#, horiz = TRUE)
rect.dendrogram(Dendogrameo(PrincipalPreocupacion), k = 6, border = 6) #, horiz = TRUE)
```

``` r
plot(Dendogrameo(QueNecesitasAprender), main = "Dendrograma")
rect.dendrogram(Dendogrameo(QueNecesitasAprender), k = 5, border = "grey50")
```

``` r
plot(Dendogrameo(QuePropones), main = "Dendrograma")
rect.dendrogram(Dendogrameo(QuePropones), k = 5, border = "grey50")
```

------------------------------------------------------------------------

Análisis de correlaciones
=========================

------------------------------------------------------------------------

Creamos una función para buscar las palabras correlacionadas con algun termino

``` r
Correlaciones<-function(cCorpus,palabra){
  cCorpus<-clean_corpus(cCorpus) #limpiamos la corpora
  cCorpus<-TermDocumentMatrix(cCorpus) #Creamos todas las TDMs
  cCorpus<-findAssocs(cCorpus, palabra, 0.2) # Crear associations_df
  cCorpus<-as.data.frame(list_vect2df(cCorpus)[, 2:3])
  
  return(cCorpus)
}
```

------------------------------------------------------------------------

Aplicamos la funcion a la palabra "persona"

------------------------------------------------------------------------

``` r
qp<-Correlaciones(PrincipalPreocupacion,"persona")
```

------------------------------------------------------------------------

Gráficamos las correlaciones
============================

------------------------------------------------------------------------

``` r
g1<-ggplot(qp,aes(y = qp[, 1])) + 
    geom_point(aes(x = qp[, 2]), data = qp, size = 3) 
ggplotly(g1)
```

------------------------------------------------------------------------

Analisis de bigramas
====================

------------------------------------------------------------------------

``` r
####
#### Análisis de bigramas
####

# funcion tokenizadora
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# unigrama
#unigram_dtm<-DocumentTermMatrix(clean_corpus(QueNecesitasAprender))

#QueNecesitasAprender
#PrincipalPreocupacion
#QuePropones

# Crea bigram_dtm con Que propones
bigram_dtm<-DocumentTermMatrix(
  clean_corpus(QueNecesitasAprender), 
  control = list(tokenize = tokenizer)
)

# Examina unigram_dtm
#unigram_dtm
# Examina bigram_dtm
bigram_dtm
# Crea bigram_dtm_m
bigram_dtm_m<-as.matrix(bigram_dtm)
# Crea freq
freq<-colSums(bigram_dtm_m)
# Crea bi_words
bi_words<-names(freq)
# grafica wordcloud
#wordcloud(bi_words,freq,max.words = 20)
```
