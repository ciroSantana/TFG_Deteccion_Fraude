#######################################
##### MODULO OBTENCION DEL MODELO #####
#######################################

#etiqueta las empresas segun los cuartiles indicados: up,down,mod = 2,1,0
categorizador <- function(datos, inf=0.25, sup=0.75){
  
  datos_limpios <- datos[!(datos %in% boxplot.stats(datos)$out)]
  
  percentiles <- quantile(datos_limpios, c(inf,sup))
  
  resultados <- NULL
  for (i in 1:length(datos)) {
    if (datos[i] < percentiles[1]) resultados$Caso[i] <- 1 
    else if (datos[i] >= percentiles[2]) resultados$Caso[i] <- 2
    else resultados$Caso[i] <- 0
  }
  return (resultados$Caso)
}

#devuelve fila con valores que cumplen/no cumplen criterio (1,2)
verificador <- function(datos, caso){
  
  resultados <- NULL
  for(i in 1:length(datos)){
    if(caso == datos[i]) resultados[i] <- 1
    else resultados[i] <- 2
  }
  return (resultados)
}

#calcula la mejor combinacion de variables en regresion segun AIC
#return vector de string con los nombres de las variables
get_mejor_modelo <- function(datos) {
  
  #obtener comb variables con menor AIC
  options(na.action = "na.fail")
  regresion <- lm(datos$Caso ~., data=datos)
  combinaciones <- dredge(regresion)
  aux <- combinaciones[1,]
  
  #obtener nombre variables relevantes
  aux <-- aux[,colSums(is.na(aux))==0]
  aux <- aux[,c(2:(ncol(aux)-5))]
  
  return (colnames(aux))
}
