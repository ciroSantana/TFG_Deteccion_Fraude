#######################################
#### MODULO EVALUACION RENDIMIENTO ####
#######################################


#Clasifica los valores de la prediccion en clase 1 o 2 en funcion del umbral
#return vector de clasificaciones
clasifica_pred <- function(prob, umbral) {
  resultado <- NULL
  for(i in 1:length(prob)){
    if(prob[i]< umbral) resultado$prob[i] <- 1
    else if (prob[i] >= umbral) resultado$prob[i] <- 2
  }
  
  return(resultado$prob)
}

#calcula todas las medidas de rendimiento a partir del valor real y predicho
#return vector con medidas de rendimiento
main_eval <- function(real, pred, umbral) {
  
  pred <- clasifica_pred(pred, umbral)
  
  error <- 100*sum(real != pred)/length(real)
  
  aux <- ROCR::prediction(pred, real)
  
  acc <- performance(aux, "acc")@y.values[[1]][2]
  
  prec <- performance(aux, "prec")@y.values[[1]][2]
  
  f <- performance(aux, "f")@y.values[[1]][2]
  
  sens <- performance(aux, "sens")@y.values[[1]][2]
  
  spec <- performance(aux, "spec")@y.values[[1]][2]
  
  croc <- roc(real, pred, legacy.axes = T)
  
  auc <- croc$auc
  
  return (list(error=error, acc=acc, prec=prec, f=f,
               sens=sens, spec=spec, croc=croc, auc=auc))
}
