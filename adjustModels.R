#######################################
## MODULO IMPLEMENTACION DE TECNICAS ##
#######################################

#entrena y predice con un modelo de regresion lineal
#return matriz de valores reales y predichos
main_regresion <- function(fmla, datos, ptest=0.2, maxtrain=500, nrep=1000){
  
  #calculo tamaño test
  ntest <- round((nrow(datos)*ptest)/2)
  
  y_real <- NULL
  y_pred <- NULL
  
  #bucle que entrena la muestra seleccionando los datos aleatorios teniendo en
  #cuenta el numero de empresas con salvedades
  
  for(i in 1:nrep){
    
    #seleccionar datos para testeo
    test <- rbind(datos[sample(which(datos$Caso == 1), ntest),],
                  datos[sample(which(datos$Caso == 2), ntest),])
    
    #eliminar datos de testeo de la tabla principal
    train <- datos[-match(test$ID, datos$ID),]
    
    #contar los casos 
    casoA <- nrow(train[train[,"Caso"] == 1,])
    casoB <- nrow(train[train[,"Caso"] == 2,])
    
    #contiene el numero minimo de casos 
    ntrain <- min(maxtrain, casoA, casoB)
    
    sample_train <- rbind(train[sample(which(train[,"Caso"] == 1), ntrain),],
                          train[sample(which(train[,"Caso"] == 2), ntrain),])
    
    #mezclo los datos de la lista de entrenamiento
    sample_train <- data.frame(sample_train[sample(nrow(sample_train)),])
    
    #generacion del modelo
    modelo <- lm(fmla, data = sample_train)
    
    pred <- data.frame(predict(modelo, test))
    
    y_real <- rbind(y_real, data.frame(test$Caso))
    y_pred <- rbind(y_pred,pred)
    
  }
  resultado <- data.frame(y_real, y_pred)
  colnames(resultado) <- c("Real", "Pred")
  return(resultado)
}

#calcula los valores de los parametros de la SVM
#return vector con los valores de los parametros
get_param_svm <- function(fmla, datos) {
  
  #preparacion de datos para svm
  datos$Caso <- factor(datos$Caso)
  
  tuned <- tune(svm, train.x=fmla, data = datos, type = "C-classification", kernel="radial", 
                ranges=list(gamma = c(0.001,0.01,0.1,1), cost=2^(2:12),epsilon=c(0.001,0.01,0.1,1),
                            control = tune.control(sampling="cross", cross=5, best.model=T, performances=T)))
  
  return (tuned$best.parameters)
}

#entrena y predice con una SVM
#return matriz de valores reales y predichos
main_svm <- function(fmla, datos, ptest=0.2, maxtrain=500, nrep=1000){
  
  #calculo tamaño test
  ntest <- round((nrow(datos)*ptest)/2)
  
  #obtencion de los parametros del modelo
  #parametros <- get_param_svm(fmla, datos)
  parametros <- list(gamma=0.01,cost=4,control=F) 
  
  #preparacion de los datos SVM
  datos[,variables] <- lapply(datos[,variables],scale, center = T, scale = T)
  datos$Caso <- factor(datos$Caso)
  
  y_real <- NULL
  y_pred <- NULL
  
  #bucle que entrena y predice con la técnica seleccionada
  for(i in 1:nrep){
    
    #seleccionar datos para testeo
    test <- rbind(datos[sample(which(datos$Caso == 1), ntest),],
                  datos[sample(which(datos$Caso == 2), ntest),])
    
    #eliminar datos de testeo de la tabla principal #BUSCAR OTRA FORMA DE HACERLO
    train <- datos[-match(test$ID, datos$ID),]
    
    #contar los casos 
    casoA <- nrow(train[train[,"Caso"] == 1,])
    casoB <- nrow(train[train[,"Caso"] == 2,])
    
    #contiene el numero minimo de casos 
    ntrain <- min(maxtrain, casoA, casoB)
    
    sample_train <- rbind(train[sample(which(train[,"Caso"] == 1), ntrain),],
                          train[sample(which(train[,"Caso"] == 2), ntrain),])
    
    #mezclo los datos de la lista de entrenamiento
    sample_train <- data.frame(sample_train[sample(nrow(sample_train)),])
    
    #preparacion datos SVM
    sample_train$Caso <- as.factor(sample_train$Caso)
    
    #generacion del modelo
    modelo <- svm(formula=fmla, data = sample_train, type = "C-classification", kernel = "radial", cost=parametros$cost,
                  gamma=parametros$gamma, probability = T)
    
    #prediccion
    prediccion <- predict(modelo, test, probability=T)
    aux <- as.numeric(attr(prediccion,"probabilities")[,"2"])
    test$Caso <- as.numeric(test$Caso)
    #guardar resultados iteracion (real y predicho)
    y_real <- matrix(cbind(y_real,test$Caso))
    y_pred <- matrix(cbind(y_pred,aux))
  }
  resultado <- data.frame(cbind(y_real, y_pred))
  colnames(resultado) <- c("Real", "Pred")
  return(resultado)
}

#entrena y predice con un random forest
#return matriz de valores reales y predichos
main_rf <- function(fmla, datos, ptest=0.2, maxtrain=500, nrep=1000) {
  
  #calculo tamaño test
  ntest <- round((nrow(datos)*ptest)/2)
  
  y_real <- NULL
  y_pred <- NULL
  
  #bucle que entrena y predice con la técnica seleccionada
  for(i in 1:nrep){
    
    #seleccionar datos para testeo
    test <- rbind(datos[sample(which(datos$Caso == 1), ntest),],
                  datos[sample(which(datos$Caso == 2), ntest),])
    
    #eliminar datos de testeo de la tabla principal
    train <- datos[-match(test$ID, datos$ID),]
    
    #contar los casos 
    casoA <- nrow(train[train[,"Caso"] == 1,])
    casoB <- nrow(train[train[,"Caso"] == 2,])
    
    #contiene el numero minimo de casos 
    ntrain <- min(maxtrain, casoA, casoB)
    
    sample_train <- rbind(train[sample(which(train[,"Caso"] == 1), ntrain),],
                          train[sample(which(train[,"Caso"] == 2), ntrain),])
    
    #mezclo los datos de la lista de entrenamiento
    sample_train <- data.frame(sample_train[sample(nrow(sample_train)),])
    
    #preparacion datos rf
    sample_train_y <- factor(sample_train[,"Caso"])
    
    #generacion del modelo
    modelo <- tuneRF(x = sample_train[,variables], y = sample_train_y, mtryStart = 1, ntree = 1000,
                     stepFactr = 1, improve = 0.05, doBest=T)
    
    pred <- data.frame(predict(modelo, newdata=test, type="prob")[,2])
    
    y_real <- rbind(y_real, data.frame(test$Caso))
    y_pred <- rbind(y_pred,pred)
    
  }
  resultado <- data.frame(y_real, y_pred)
  colnames(resultado) <- c("Real", "Pred")
  return(resultado)
}

#entrena y predice con un ensamblado de varios metodos
#return matriz de valores reales y predichos
main_hybensemble <- function(variables, datos, ptest=0.2, maxtrain = 500, nrep = 1000) {
  
  #calculo tamaño test
  ntest <- round((nrow(datos)*ptest)/2)
  
  y_real <- NULL
  y_pred <- NULL
  
  #bucle que entrena la muestra seleccionando los datos aleatorios teniendo en
  #cuenta el numero de empresas con salvedades
  
  for(i in 1:nrep){
    
    #seleccionar datos para testeo
    test <- rbind(datos[sample(which(datos$Caso == 1), ntest),],
                  datos[sample(which(datos$Caso == 2), ntest),])
    
    #eliminar datos de testeo de la tabla principal
    train <- datos[-match(test$ID, datos$ID),]
    
    #contar los casos 
    casoA <- nrow(train[train[,"Caso"] == 1,])
    casoB <- nrow(train[train[,"Caso"] == 2,])
    
    #contiene el numero minimo de casos 
    ntrain <- min(maxtrain, casoA, casoB)
    
    sample_train <- rbind(train[sample(which(train[,"Caso"] == 1), ntrain),],
                          train[sample(which(train[,"Caso"] == 2), ntrain),])
    
    #mezclo los datos de la lista de entrenamiento
    sample_train <- data.frame(sample_train[sample(nrow(sample_train)),])
    
    #preparacion datos rf
    sample_train_y <- as.factor(sample_train[,"Caso"])
    
    #generacion del modelo
    modelo <- hybridEnsemble(x = sample_train[,variables], y = sample_train_y,verbose = FALSE,RF.ntree=500,AB.iter=500,
                             NN.size=c(15,20,10),NN.decay=0,SV.gamma = 2^(-5:3),SV.cost = 2^(-5:5),SV.degree=2,SV.kernel="radial")
    
    pred <- predict(modelo, newdata=test[,variables], verbose=F)$predSB #predSB o predMEAN
    
    y_real <- rbind(y_real, data.frame(test$Caso))
    y_pred <- rbind(y_pred, data.frame(pred))
  }
  resultado <- data.frame(y_real, y_pred)
  colnames(resultado) <- c("Real", "Pred")
  return(resultado)
}

#entrena y predice con un mapa autoorganizado de Kohonen
#return matriz de valores reales y predichos
main_koh <- function(variables, datos, ptest=0.2, maxtrain=500, nrep=1000) {
  
  #calculo tamaño test
  ntest <- round((nrow(datos)*ptest)/2)
  
  #preparacion de los datos para kohonen
  datos[,variables] <- lapply(datos[,variables],scale, center = T, scale = T)
  
  y_real <- NULL
  y_pred <- NULL
  
  #bucle que entrena y predice con la técnica seleccionada
  for(i in 1:nrep){
    
    #seleccionar datos para testeo
    test <- rbind(datos[sample(which(datos$Caso == 1), ntest),],
                  datos[sample(which(datos$Caso == 2), ntest),])
    
    #eliminar datos de testeo de la tabla principal
    train <- datos[-match(test$ID, datos$ID),]
    
    #contar los casos 
    casoA <- nrow(train[train[,"Caso"] == 1,])
    casoB <- nrow(train[train[,"Caso"] == 2,])
    
    #contiene el numero minimo de casos 
    ntrain <- min(maxtrain, casoA, casoB)
    
    sample_train <- rbind(train[sample(which(train[,"Caso"] == 1), ntrain),],
                          train[sample(which(train[,"Caso"] == 2), ntrain),])
    
    #mezclo los datos de la lista de entrenamiento
    sample_train <- sample_train[sample(nrow(sample_train)),]
    
    #preparacion de los datos para kohonen
    sample_train[,variables] <- lapply(sample_train[,variables],scale, center = T, scale = T)
    lista <- list(measurements = as.matrix(sample_train[,variables], ncol=length(variables)), 
                  caso = as.matrix(sample_train[,"Caso"], ncol=1))
    
    #generacion del modelo
    modelo <- xyf(X=as.matrix(sample_train[,variables]), Y=as.matrix(sample_train[,"Caso"]),
                  grid = somgrid(30,30, "rectangular"))
    
    #guardar caso real antes de preparacion
    y_real <- rbind(y_real, data.frame(test$Caso))
    
    #preparacion de los datos para kohonen
    test <- list(measurements = as.matrix(test[,variables], ncol=length(variables)), 
                 caso = as.matrix(test[,"Caso"], ncol=1))
    
    pred <- as.data.frame(predict(modelo, newdata=test)$predictions)#$predictions$caso
    pred <- as.data.frame(pred$V1)
    
    y_pred <- rbind(y_pred,pred)
  }
  resultado <- data.frame(y_real, y_pred)
  colnames(resultado) <- c("Real", "Pred")
  return(resultado)
}