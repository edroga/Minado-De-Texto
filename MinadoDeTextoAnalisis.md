Untitled
================

Esta parte solo la cargamos si tenemos problemas con Java para la libreria qdap

Versión interactiva <http://rpubs.com/ilan_rg/minadodetexto>

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

    ##  [1] "¿Cuál es tu principal preocupación respecto al uso de Internet de las niñas, niños y adolescentes de tu familia?"                                 
    ##  [2] "¿Qué necesitas aprender o conocer para poder proteger mejor a tu familia y hacer de Internet un espacio más seguro?"                              
    ##  [3] "¿Qué propones que haga tu gobierno, las escuelas, otros padres o madres de familia para proteger mejor a niñas, niños y adolescentes en Internet?"
    ##  [4] "Selecciona tu entidad"                                                                                                                            
    ##  [5] "Selecciona tu municipio"                                                                                                                          
    ##  [6] "Edad"                                                                                                                                             
    ##  [7] "Género"                                                                                                                                           
    ##  [8] "Nivel educativo"                                                                                                                                  
    ##  [9] "Ocupación"                                                                                                                                        
    ## [10] "Hijos"                                                                                                                                            
    ## [11] NA                                                                                                                                                 
    ## [12] NA                                                                                                                                                 
    ## [13] NA                                                                                                                                                 
    ## [14] NA                                                                                                                                                 
    ## [15] NA                                                                                                                                                 
    ## [16] NA                                                                                                                                                 
    ## [17] NA                                                                                                                                                 
    ## [18] NA                                                                                                                                                 
    ## [19] NA                                                                                                                                                 
    ## [20] NA                                                                                                                                                 
    ## [21] NA                                                                                                                                                 
    ## [22] NA                                                                                                                                                 
    ## [23] NA                                                                                                                                                 
    ## [24] NA                                                                                                                                                 
    ## [25] NA                                                                                                                                                 
    ## [26] NA

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

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<script type="application/json" data-for="mjs-a967e7acbc29a75d4fb2b09232a8ad">{"x":{"forCSS":null,"regions":null,"orig_posix":false,"data":{"dfrecS":[26,17,16,14,14,13,13,12,12,12,11,10,9,9,8,8,8,7,7,7],"term":["niño","persona","pueden","acceso","redessocial","internet","pornografia","contenido","informacion","uso","pagina","cosa","mal","puedan","dan","ser","ver","adecuado","adolescent","edad"],"num":[26,17,16,14,14,13,13,12,12,12,11,10,9,9,8,8,8,7,7,7]},"x_axis":true,"y_axis":true,"baseline_accessor":null,"predictor_accessor":null,"show_confidence_band":null,"chart_type":"bar","xax_format":"plain","x_label":null,"y_label":null,"markers":null,"baselines":null,"linked":false,"title":null,"description":null,"left":80,"right":10,"bottom":60,"buffer":8,"format":"count","y_scale_type":"linear","yax_count":5,"xax_count":6,"x_rug":false,"y_rug":false,"area":false,"missing_is_hidden":false,"size_accessor":null,"color_accessor":null,"color_type":"number","color_range":["blue","red"],"size_range":[1,5],"bar_height":20,"min_y":null,"max_y":null,"bar_margin":1,"binned":true,"bins":null,"least_squares":false,"interpolate":"cardinal","decimals":2,"show_rollover_text":true,"x_accessor":"num","y_accessor":"term","multi_line":null,"geom":"bar","yax_units":"","legend":null,"legend_target":null,"y_extended_ticks":false,"x_extended_ticks":false,"target":"#mjs-a967e7acbc29a75d4fb2b09232a8ad"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
head(df_QueNecesitasAprender,20) %>%
  mjs_plot(x=num, y=term, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')
```

<!--html_preserve-->

<script type="application/json" data-for="mjs-a76eb987c31822c6c4b42e568f9a10">{"x":{"forCSS":null,"regions":null,"orig_posix":false,"data":{"dfrecS":[27,21,13,12,11,10,10,10,10,10,9,8,7,7,7,7,6,6,5,5],"term":["internet","pagina","aprend","segura","bloquear","conoc","hijo","redessocial","tener","uso","seguridad","computadora","acceso","familia","herramienta","informacion","cada","poder","manera","niño"],"num":[27,21,13,12,11,10,10,10,10,10,9,8,7,7,7,7,6,6,5,5]},"x_axis":true,"y_axis":true,"baseline_accessor":null,"predictor_accessor":null,"show_confidence_band":null,"chart_type":"bar","xax_format":"plain","x_label":null,"y_label":null,"markers":null,"baselines":null,"linked":false,"title":null,"description":null,"left":80,"right":10,"bottom":60,"buffer":8,"format":"count","y_scale_type":"linear","yax_count":5,"xax_count":6,"x_rug":false,"y_rug":false,"area":false,"missing_is_hidden":false,"size_accessor":null,"color_accessor":null,"color_type":"number","color_range":["blue","red"],"size_range":[1,5],"bar_height":20,"min_y":null,"max_y":null,"bar_margin":1,"binned":true,"bins":null,"least_squares":false,"interpolate":"cardinal","decimals":2,"show_rollover_text":true,"x_accessor":"num","y_accessor":"term","multi_line":null,"geom":"bar","yax_units":"","legend":null,"legend_target":null,"y_extended_ticks":false,"x_extended_ticks":false,"target":"#mjs-a76eb987c31822c6c4b42e568f9a10"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
head(df_QuePropones,20) %>%
  mjs_plot(x=num, y=term, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')
```

<!--html_preserve-->

<script type="application/json" data-for="mjs-d23f15de8d42259f82d925f92a2a00">{"x":{"forCSS":null,"regions":null,"orig_posix":false,"data":{"dfrecS":[34,24,15,15,15,15,13,12,11,8,7,7,7,7,6,6,6,6,6,6],"term":["internet","padres","escuela","niño","pagina","uso","hijo","informacion","gobierno","dar","educacion","mala","redessocial","tener","campaña","darl","familia","menor","riesgo","seguridad"],"num":[34,24,15,15,15,15,13,12,11,8,7,7,7,7,6,6,6,6,6,6]},"x_axis":true,"y_axis":true,"baseline_accessor":null,"predictor_accessor":null,"show_confidence_band":null,"chart_type":"bar","xax_format":"plain","x_label":null,"y_label":null,"markers":null,"baselines":null,"linked":false,"title":null,"description":null,"left":80,"right":10,"bottom":60,"buffer":8,"format":"count","y_scale_type":"linear","yax_count":5,"xax_count":6,"x_rug":false,"y_rug":false,"area":false,"missing_is_hidden":false,"size_accessor":null,"color_accessor":null,"color_type":"number","color_range":["blue","red"],"size_range":[1,5],"bar_height":20,"min_y":null,"max_y":null,"bar_margin":1,"binned":true,"bins":null,"least_squares":false,"interpolate":"cardinal","decimals":2,"show_rollover_text":true,"x_accessor":"num","y_accessor":"term","multi_line":null,"geom":"bar","yax_units":"","legend":null,"legend_target":null,"y_extended_ticks":false,"x_extended_ticks":false,"target":"#mjs-d23f15de8d42259f82d925f92a2a00"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

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

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
wordcloud(df_QueNecesitasAprender$term, 
          df_QueNecesitasAprender$num,
          max.words = 100, 
          colors = "red")
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
wordcloud(df_QuePropones$term, 
          df_QuePropones$num,
          max.words = 100, 
          colors = "red")
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-19-1.png) \*\*\*

Generamos wordclouds interactivas
=================================

------------------------------------------------------------------------

``` r
wordcloud2(df_PrincipalPreocupacion[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-e1c420a14b96609646f7">{"x":{"word":["niño","persona","pueden","acceso","redessocial","internet","pornografia","contenido","informacion","uso","pagina","cosa","mal","puedan","dan","ser","ver","adecuado","adolescent","edad","sitio","tener","violencia","vean","hijo","inapropiado","joven","menor","mucha","padres","apto","asi","contacto","cualquier","etc","facil","familia","gobierno","poder","preocupacion","propia","pue","red","servicio","acced","acto","adecuada","adulto","alto","amigo","año","buen","cada","comet","comunicacion","desarrollo","desconocida","facebook","hacer","incluso","mala","manera","mayor","medio","mexico","misma","nueva","oportunidad","peligro","pequeño","riesgo","seguridad","sexual","tiempo","tres","accedan","afecta","algun","aun","ayuda","comunidad","confiabl","control","correcta","cultura","dañando","dedican","delito","dema","den","dia","droga","encuentren","entrar","existen","expuesto","facilidad","facilment","falta","familiar","filtro","forma","generacion","grupo","hacen","idea","identidad","inapropiada","indebido","ingresar","lado","maestro","materi","ment","mismo","motivo","mundo","ninguna","nunca","opinion","part","person","piden","podrian","poner","porno","preocupa","problema","pued","pueda","realizar","saben","secuestro","social","solo","tarea","tema","trabajo","trata","usarla","utilizar","vez","victima","video","virtual","web","abierta","abierto","ablar","abran","abrir","abuso","aceptar","acepten","acerca","actualment","adema","ademà","adicto","afectarlo","agresivo","ahi","ajena","ajust","alcanc","alcans","alejen","alguien","alguna","ambito","amor","anuncio","aparec","aparentan","aplicacion","apropiada","aprovech","aprovechars","area","arma","asd","aser","asimismo","asta","asu","atentar","atrapan","basura","benefici","bien","bienestar","blanca","blanco","bueno","caer","call","caricatura","caro","casa","censura","cerrado","chip","ciberbulli","cierta","circula","ciudad","cobigo","comercial","comparta","comunitario","conectado","confianza","confien","conoc","conozcan","consient","contacten","contar","contratarlo","controlar","contruir","convencido","convivencia","corr","coso","creaccion","crean","creo","cual","cuenta","cuidars","dañan","danen","dañen","dar","deben","deberian","debido","defenders","definitivament","dejar","delincuent","demando","demasiada","dentro","desconocido","descubran","deseo","desintegracion","desmedido","dificil","difundacion","dignidad","dinero","distinguir","dominant","economico","educacion","educar","educativa","ejecucionesuso","ejercicio","emocion","empleo","empresa","empresario","encuentr","encuentro","enfoken","engañando","enriptado","entorno","entra","entren","envuelvan","eredan","erronea","error","escudo","escuela","especi","esquina","estabilidad","estereotipo","estilo","estorcionen","examen","existan","expandirlo","expon","extors","extraña","extraño","facebbok","famosa","favorec","fea","feder","fin","firma","fisica","foto","fronterizasaqu","fuent","funcion","fundacion","gent","gore","grafica","gran","grata","gustaria","hablen","hace","haci","hacia","haciendo","hija","honestidad","hoy","httpswwwyoutubecomwatchvvcjtednq","ideologia","ignorancia","igualdad","importancia","inciten","indoumentado","industria","industrial","infant","infantil","influenciado","infomacion","informaciòn","informar","inmobiliaria","inocencia","insegura","institucion","integrada","integridad","inteligencia","intencionada","inutil","invento","investigando","inviten","irresponsabilidad","jovencito","judia","juego","junto","lealtad","libanesa","libertad","linea","llega","llegar","llege","llegu","llevan","localizarla","lugar","machismo","madurez","malicioso","maloo","malversacion","manejarla","manipulacion","manipulado","manipular","mano","meno","mensaj","mensura","mentalidad","mexicana","mexicano","microindustria","mientra","molesta","momento","moverl","muestra","mundial","muro","nacion","nadi","narcotrafico","navegar","negocio","nino","nociva","noma","non","nuevo","observado","pajina","pasan","pasar","patria","patrimonio","peligroso","perdido","permanent","permiten","pesar","piensan","pienso","pod","podemo","policia","pornografiadelincuencia","pornograficasi","pornografico","portal","precio","preferencia","prefieren","pregunta","prematura","preparacion","prepsear","president","primero","princip","pro","problematico","profesion","prohibido","propiedad","propon","propongo","proteg","quisiera","quitaran","realizan","recibir","registrar","regulen","relacionan","relacionen","requerida","resguardo","respaldo","respeto","respuesta","restriccion","rodean","sabemo","saber","salen","secta","secuestrador","segura","sencillo","señor","sepan","sexo","sicologicament","simplement","sino","situacion","sociedad","som","suben","sustent","tal","tan","tecnologia","tejida","tipo","toda","traficador","tratant","tratar","trato","twitterinstagramqu","unica","universidad","uqe","util","utiliza","utilizada","utilizado","utilizarlo","valor","van","vandalico","vayan","vece","ventana","verbal","veridica","vida","violacion","violento","visto","vivan","vivienda","vulner","zdxvd"],"freq":[26,17,16,14,14,13,13,12,12,12,11,10,9,9,8,8,8,7,7,7,7,7,7,6,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":13.8461538461538,"backgroundColor":"white","gridSize":0,"minRotation":-1.5707963267949,"maxRotation":-1.5707963267949,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
wordcloud2(df_QueNecesitasAprender[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-b90352afe5e86e029202">{"x":{"word":["internet","pagina","aprend","segura","bloquear","conoc","hijo","redessocial","tener","uso","seguridad","computadora","acceso","familia","herramienta","informacion","cada","poder","manera","niño","saber","usar","conozco","creo","hace","hacen","lugar","movil","poner","problema","pued","riesgo","seguro","tema","tiempo","ver","acerca","actualment","candado","contenido","control","correcto","dar","debe","dominar","familiar","forma","manejar","necesito","paso","persona","puedan","pueden","quiera","quiero","restringir","ser","solo","tecnologia","adolescent","apto","asegurar","asi","basica","bloqueo","bueno","buscar","capacitacion","casa","causa","celular","comercial","comunidad","confianza","configuracion","conocimiento","cuenta","cuidado","cultura","debida","dia","etc","existen","falla","funcionan","gestionar","gobierno","gratuito","guiar","hacer","hija","identificar","joven","mal","medio","mejor","mundo","navegador","necesita","oportunidad","padres","pajina","part","peligro","platica","platicar","preocupan","primero","rede","una","usarlo","use","viendo","visitan","abrirl","acced","accedan","actualizarm","administrar","adquirir","adverdrio","alcanc","alguien","algun","alo","alternativa","amistad","analisar","anuncio","aparato","aparec","apariencia","apart","apena","aportan","aportar","apoyo","aprendan","apropiado","aquello","aquirirlo","asdf","bando","basado","benefica","beneficio","bien","bloqueada","bloqueen","borren","buscan","busco","busquen","campaña","castigar","cerrada","certera","chiapa","cibernetica","cibernetico","cierta","ciertaent","clase","coki","compartir","computacion","computo","comunicacion","conciencia","conocen","conscient","consecuencia","contraceña","controlar","corran","correcta","correspondient","cosa","cualquier","cuarto","cuestion","curiosidad","curso","cyber","daria","dato","deben","debidament","dedicarl","dediquen","defensa","dejar","delincuencia","dema","den","deport","desarrollar","desenvolvers","detectar","detecten","devido","dicha","diferencia","dificiil","digital","discernir","dispon","dispositivo","distinta","dominando","droga","edad","educar","efecto","encierren","encontrar","encuentra","enferm","enfermedad","enseñarlo","entend","enterarm","entrada","entran","entrar","entrara","erronea","escuchando","espacio","especi","establec","estarlo","estudio","evitar","examen","exchang","extra","facil","facilment","filtrar","financiamiento","gent","gran","grata","groom","gunernament","gustaria","gusto","habilidad","habito","hablando","hablar","hacerno","hacker","haga","hecho","herramineta","higa","histori","historia","historial","hogar","homofobico","hora","httpswwwyoutubecomwatchvlivyipwnw","humild","ileg","ilegal","impidan","impulso","inadecuado","indebida","infant","informatica","informatico","informativo","inicia","inormacion","instalar","intencionada","intensiva","interactuan","introducion","ixp","lado","libr","limit","llegamo","llegen","logren","malo","manejo","manten","manteners","mecanismo","meno","menor","mental","mesoamerica","metan","meterno","mexico","mientra","misma","mismo","moda","momento","monitoriando","moral","nadi","navegacion","navegar","necesaria","necesario","necesitan","nevegador","nido","ningun","noma","nombr","notificacion","obviament","ocasionan","ocasionar","ocupar","ocupemo","ocupo","ojala","opcion","orienten","otorgada","paginaa","pai","pais","pantalla","particular","pendient","permit","permita","person","personal","pigana","plataforma","platicando","podamo","podemo","point","politico","ponerl","pornografia","pos","preparatoria","presentan","pribada","primer","principal","principalment","prohibida","proteg","protocolo","proveedor","provoca","provocar","publicidad","pudieramo","pue","pueda","puedo","puerta","quier","quitar","raiz","raro","realizar","realment","recorrido","red","region","regla","reglamento","restriccion","restringido","retroced","revisando","revisar","saben","saberi","satisfaccion","sdfsd","seguir","segundo","sepan","seriedad","sexo","sitio","sobr","social","softwar","solament","solucionarlo","subiendo","subterranea","supervis","supervisar","tendencia","tercermundista","toda","tomamo","tomar","total","transmitir","tratar","trave","unico","utilicemo","utilisa","utilizada","utilizamo","utilizan","valido","valor","van","verdad","vez","violador","violencia","web","yque"],"freq":[27,21,13,12,11,10,10,10,10,10,9,8,7,7,7,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":13.3333333333333,"backgroundColor":"white","gridSize":0,"minRotation":-1.5707963267949,"maxRotation":-1.5707963267949,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
wordcloud2(df_QuePropones[,2:3] , size = 2, minRotation = -pi/2, maxRotation = -pi/2)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-ab57a568e7c83a035b9f">{"x":{"word":["internet","padres","escuela","niño","pagina","uso","hijo","informacion","gobierno","dar","educacion","mala","redessocial","tener","campaña","darl","familia","menor","riesgo","seguridad","adolescent","ala","deben","den","enseñar","hacer","mal","manera","persona","platica","pongan","programa","pueden","tecnologia","toda","acerca","asi","capacitar","control","debe","evitar","mejor","peligro","primero","pued","usar","acceso","actividad","adulto","año","baño","bien","cada","capacitacion","casa","codo","conoc","contenido","cosa","crear","dejen","enseñen","entren","espacio","hacen","herramienta","libro","maestro","mayor","mediant","mejoren","nms","pendient","primaria","puedan","rapido","rede","ser","taller","tan","tres","ver","alguien","algun","alumno","apropiada","atencion","bloqueo","cibernetica","computacion","conciencia","conferencia","consecuencia","convivencia","cuidado","curso","dejar","delito","desarrollo","digital","dio","educar","ejemplo","enseñarl","equipo","filtro","hacerl","haga","implementar","importancia","informado","informar","joven","limit","malo","manejo","medio","mexico","pai","pajina","policia","pornografia","prohibir","promov","propongo","pue","realizan","red","regla","regul","requier","respons","restriccion","saber","segura","sitio","social","suced","supervisor","tarea","tecnologica","tipo","trave","usan","vez","vigilar","violacion","violencia","virtual","abastec","abundancia","abuso","acced","acen","acometida","actitud","actualizado","adecuada","adolescencia","agan","ahiga","aiga","alentar","alertado","alertar","alguna","alterna","alto","alusivo","ambito","amigo","amor","ampliacion","aparato","apartir","aplicacion","apoyen","apoyo","aprendio","aprovechars","area","asd","aseguren","asesinada","asignar","atento","autosuficient","ayudar","basado","basica","biblioteca","bloc","bloqueado","bloquear","bloqueen","borrar","buen","bueno","busqueda","calidad","call","capacit","cargo","caset","caso","centro","cerrar","cheken","ciberbulli","cierta","cierto","circula","circulan","circustancia","clase","colegio","color","comenxar","comercial","cometen","comienzo","comit","computador","computo","comun","comunicacion","comunidad","concientizacion","conducta","conocen","conocimiento","conozcan","consigo","consumen","contacto","contar","continu","controlado","controlar","conversen","correcta","correcto","corren","correr","creencia","cualquier","cuiden","culpa","daña","dañar","dañino","debido","decicion","dejarla","delictiva","delincuencia","demostrandolo","dentro","denunciar","deport","desarroll","desea","detectar","devida","difucion","difundir","difus","dinamica","director","diseñ","diseñado","diseñar","disfruten","distinta","ecucativo","edad","editar","eduqu","eduquen","eficac","eficiencia","electronica","electronico","embat","empezar","empoderar","encuentr","enfrentar","ensenar","enseñaramo","entend","entrar","entretenimiento","envi","equivocada","error","escuelascasasetcpara","escuelña","esquiena","estapa","estatus","estudiant","estudiantil","etc","evento","exista","expliquen","exponen","facebook","facil","facilitandono","factibl","falsa","familiar","famoso","filtren","fomentar","forma","formacion","fraud","fuent","fuertement","genio","grado","groom","haber","hablan","hablarian","hablen","haciendo","hagan","haiga","hashtag","hij","hora","httpswwwyoutubecomwatchvlivyipwnw","idea","igual","impacten","impacto","impida","impidan","important","importar","inadecuada","inadecuado","inapropiado","indigar","individuo","influencia","informaciòn","informarlo","ingresar","iniciar","inmediatament","instruya","instruyan","integrar","integridad","inteligent","intercambiar","interes","internacional","introducion","inutil","lado","ley","libr","liguen","limitado","llegar","lleve","lugar","malusando","manejar","manten","materi","materia","mayoria","medica","meno","mensaj","ment","mental","meter","metodo","mexicana","miedo","miren","mismo","mostrarl","mundial","municipio","nacion","navegador","necesaria","necesitan","nilo","nino","nivel","normalizado","noticia","nueva","observen","ocult","ocupamo","organic","parental","participativa","pasar","patria","penalizar","pensar","permiten","pidan","plan","plantel","pone","poner","ponerl","ponga","pornografico","posibl","practica","precaucion","prevencion","prevenirla","primerament","principio","prioridad","problema","problematica","procuracion","prohiba","prohibido","proporcionarno","proporcionen","protegan","protocolo","provecho","provoca","proyecto","psicologia","psicologo","publican","publico","pueda","puedo","quelo","quiten","qun","radio","realizada","realizar","realment","reforzar","reparta","repartir","report","resilent","respeto","restringido","restrinjan","reveldia","rodean","rural","salud","sana","sdfsdf","secundaria","segundo","seguro","semejant","sensurar","sepan","servicio","servir","sext","sexual","sinaloa","sino","situacion","soberania","supervisarlo","sustentar","tardia","tema","temor","tercer","terminologia","tiempo","todi","tolerancia","totalment","trabajo","trae","trafico","trata","una","unica","unido","universidad","usando","usarlo","utilizada","utilizalo","utilizar","utilizarla","validen","valor","vean","vecindad","ven","victima","video","vigilado","visitan","visitando","viva","web"],"freq":[34,24,15,15,15,15,13,12,11,8,7,7,7,7,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":0,"weightFactor":10.5882352941176,"backgroundColor":"white","gridSize":0,"minRotation":-1.5707963267949,"maxRotation":-1.5707963267949,"shuffle":true,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->

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

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-24-1.png) \*\*\*

Creamos comparison clouds
=========================

------------------------------------------------------------------------

``` r
comparison.cloud(all_m[,1:3], colors = c("orange", "blue", "black"), max.words = 40)
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
comparison.cloud(all_m[,1:2], colors = c("orange", "blue"), max.words = 60)
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
comparison.cloud(all_m[,2:3], colors = c("orange", "blue"), max.words = 60)
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
comparison.cloud(all_m[,c(1,3) ], colors = c("orange", "blue"), max.words = 50)
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-28-1.png) \*\*\*

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

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
plot(Dendogrameo(QueNecesitasAprender), main = "Dendrograma")
rect.dendrogram(Dendogrameo(QueNecesitasAprender), k = 5, border = "grey50")
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
plot(Dendogrameo(QuePropones), main = "Dendrograma")
rect.dendrogram(Dendogrameo(QuePropones), k = 5, border = "grey50")
```

![](MinadoDeTextoAnalisis_files/figure-markdown_github/unnamed-chunk-32-1.png) \*\*\* \# Análisis de correlaciones

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

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-6a1bc894364cd794fd70">{"x":{"data":[{"x":[0.2,0.22,0.25,0.29,0.37,0.4,0.46,0.46,0.57,0.57,0.57,0.57,0.73,0.73,0.81,0.83,0.83,0.83,0.83,0.83,0.83,0.83,0.83,0.83,0.83],"y":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],"text":["qp[, 2]: 0.2<br>qp[, 1]: ingresar","qp[, 2]: 0.22<br>qp[, 1]: puedan","qp[, 2]: 0.25<br>qp[, 1]: desconocida","qp[, 2]: 0.29<br>qp[, 1]: niño","qp[, 2]: 0.37<br>qp[, 1]: ser","qp[, 2]: 0.4<br>qp[, 1]: adolescent","qp[, 2]: 0.46<br>qp[, 1]: familia","qp[, 2]: 0.46<br>qp[, 1]: propia","qp[, 2]: 0.57<br>qp[, 1]: dedican","qp[, 2]: 0.57<br>qp[, 1]: delito","qp[, 2]: 0.57<br>qp[, 1]: dema","qp[, 2]: 0.57<br>qp[, 1]: trata","qp[, 2]: 0.73<br>qp[, 1]: comet","qp[, 2]: 0.73<br>qp[, 1]: incluso","qp[, 2]: 0.81<br>qp[, 1]: acto","qp[, 2]: 0.83<br>qp[, 1]: area","qp[, 2]: 0.83<br>qp[, 1]: convencido","qp[, 2]: 0.83<br>qp[, 1]: dignidad","qp[, 2]: 0.83<br>qp[, 1]: escuela","qp[, 2]: 0.83<br>qp[, 1]: manipulado","qp[, 2]: 0.83<br>qp[, 1]: observado","qp[, 2]: 0.83<br>qp[, 1]: patrimonio","qp[, 2]: 0.83<br>qp[, 1]: rodean","qp[, 2]: 0.83<br>qp[, 1]: utilizado","qp[, 2]: 0.83<br>qp[, 1]: vandalico"],"key":null,"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":11.3385826771654,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","name":""}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":89.8630136986302},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[0.1685,0.8615],"ticktext":["0.2","0.4","0.6","0.8"],"tickvals":[0.2,0.4,0.6,0.8],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"qp[, 2]","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[0.4,25.6],"ticktext":["ingresar","puedan","desconocida","niño","ser","adolescent","familia","propia","dedican","delito","dema","trata","comet","incluso","acto","area","convencido","dignidad","escuela","manipulado","observado","patrimonio","rodean","utilizado","vandalico"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"qp[, 1]","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"modeBarButtonsToRemove":["sendDataToCloud"]},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->

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
```

    ## <<DocumentTermMatrix (documents: 96, terms: 627)>>
    ## Non-/sparse entries: 669/59523
    ## Sparsity           : 99%
    ## Maximal term length: 38
    ## Weighting          : term frequency (tf)

``` r
# Crea bigram_dtm_m
bigram_dtm_m<-as.matrix(bigram_dtm)
# Crea freq
freq<-colSums(bigram_dtm_m)
# Crea bi_words
bi_words<-names(freq)
# grafica wordcloud
#wordcloud(bi_words,freq,max.words = 20)
```
