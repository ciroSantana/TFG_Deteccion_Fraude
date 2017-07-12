################################################################################
########################### INICIO CODIGO EJECUTABLE ###########################
################################################################################

################################################################################
########################### PREPARACION DEL ENTORNO ############################
################################################################################

#eliminar todas las variables previas del entorno
rm(list = setdiff(ls(), lsf.str()))

#instalacion y carga de los paquetes necesarios

if(require("readxl")==FALSE){install.packages("readxl")}
library(readxl)
if(require("MuMIn")==FALSE){install.packages("MuMIn")}
library(MuMIn)
if(require("e1071")==FALSE){install.packages("e1071")}
library(e1071)
if(require("randomForest")==FALSE){install.packages("randomForest")}
library(randomForest)
if(require("hybridEnsemble")==FALSE){install.packages("hybridEnsemble")}
library(hybridEnsemble)
if(require("kohonen")==FALSE){install.packages("kohonen")}
library(kohonen)
if(require("ROCR")==FALSE){install.packages("ROCR")}
library(ROCR)
if(require("pROC")==FALSE){install.packages("pROC")}
library(pROC)
################################################################################
######################### FIN PREPARACION DEL ENTORNO ##########################
################################################################################


################################################################################
############################ SECTOR: P. INMOBILIARIA ###########################
################################################################################

#1. OBTENCION DEL MODELO

      #CARGA DE DATOS
      base <- read_excel("C:/Datos/programacion.xlsx")
      base <- na.omit(base)
      
      #clasificar observaciones para downgrade
      base$Fraude <- categorizador(base$V1)
      base$Caso <- verificador(base$Fraude, 1)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)
      
      
      #clasificar observaciones
      base$Caso <- verificador(base$Fraude, 2)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)

      #modelo combinado
      variables <- c("AutoF", "CtDeudaT", "CtePRem", "RE", "Liq", "IndEndNRem")
      
      
################ CLASIFICADOR DOWNGRADE ################      

#2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: DOWNGRADE
      base$Caso <- verificador(base$Fraude, 1)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
#3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM

      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))

#4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)

      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
############### FIN CLASIFICADOR DOWNGRADE ################
      
#################  CLASIFICADOR UPGRADE ###################

      #2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: UPGRADE
      base$Caso <- verificador(base$Fraude, 1)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
      #3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM
      
      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))
      
      #4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)

############### FIN CLASIFICADOR UPGRADE ################
      
################################################################################
########################## FIN SECTOR: P. Inmobiliaria #########################
################################################################################
      
      
################################################################################
############################## SECTOR: HOTELES #################################
################################################################################
      
      #1. OBTENCION DEL MODELO
      
      #CARGA DE DATOS
      base <- read_excel("C:/Datos/programacion.xlsx")
      base <- na.omit(base)
      
      #clasificar observaciones para downgrade
      base$Fraude <- categorizador(base$V1)
      base$Caso <- verificador(base$Fraude, 1)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)
      
      
      #clasificar observaciones
      base$Caso <- verificador(base$Fraude, 2)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)
      
      #modelo combinado
      variables <- c("EndCP", "IndEndRem", "RE", "RotExpl", "Endeud", "Margen")
      
      ################ CLASIFICADOR DOWNGRADE ################      
      
      #2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: DOWNGRADE
      base$Caso <- verificador(base$Fraude, 2)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
      #3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM
      
      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))
      
      #4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
############### FIN CLASIFICADOR DOWNGRADE ################
      
#################  CLASIFICADOR UPGRADE ###################
      
#2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: UPGRADE
      base$Caso <- verificador(base$Fraude, 2)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
#3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM
      
      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))
      
#4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
############### FIN CLASIFICADOR UPGRADE ################
      
      
################################################################################
############################## FIN SECTOR: HOTELES #############################
################################################################################


################################################################################
############################ SECTOR: PROGRAMACION ##############################
################################################################################
      
      #1. OBTENCION DEL MODELO
      
      #CARGA DE DATOS
      base <- read_excel("C:/Datos/programacion.xlsx")
      base <- na.omit(base)
      
      #clasificar observaciones para downgrade
      base$Fraude <- categorizador(base$V1)
      base$Caso <- verificador(base$Fraude, 1)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)
      
      
      #clasificar observaciones
      base$Caso <- verificador(base$Fraude, 2)
      
      #variables modelo downgrade
      variables <- get_mejor_modelo(base)
      
      #modelo combinado
      variables <- c("Liq", "RE", "RotExpl", "Margen", "RotAct", "IndEndNRem")
      
      ################ CLASIFICADOR DOWNGRADE ################      
      
      #2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: DOWNGRADE
      base$Caso <- verificador(base$Fraude, 1)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
      #3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM
      
      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))
      
      #4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      ############### FIN CLASIFICADOR DOWNGRADE ################
      
      #################  CLASIFICADOR UPGRADE ###################
      
      #2. PREPARACION DE LOS DATOS
      
      #guardar solo las variables del mejor modelo. Valor fijado: UPGRADE
      base$Caso <- verificador(base$Fraude, 2)
      datos <- base[,variables]
      datos <- cbind(datos,base[,c("ID","Caso")])
      
      #construir formula de regresion
      fmla <- as.formula(paste("Caso ~ ", paste(variables, collapse= "+")))
      
      #3. AJUSTE DEL MODELO MEDIANTE: regresion, random forest, svm, hybrid ensemble, SOM
      
      result_reg <- main_regresion(fmla, datos)
      
      result_rf <- main_rf(variables, datos)
      
      result_svm <- main_svm(fmla, datos)
      
      result_he <- main_hybensemble(variables, datos)
      
      result_koh <- na.omit(main_koh(variables,datos))
      
      #4. OBTENCIÓN DE MEDIDAS DE RENDIMIENTO CON DISTINTOS UMBRALES
      margen = 0.3
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.5
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      margen = 0.6
      meds_reg = main_eval(result_reg$Real, result_reg$Pred, umbral=1+margen)
      meds_rf = main_eval(result_rf$Real, result_rf$Pred, umbral=margen)
      meds_svm = main_eval(result_svm$Real, result_svm$Pred, umbral=margen)
      meds_he = main_eval(result_he$Real, result_he$Pred, umbral=1+margen)
      meds_koh <- main_eval(result_koh$Real,result_koh$Pred, umbral=1+margen)
      
      plot(meds_reg$croc, col="blue", legacy.axes=T)
      plot(meds_rf$croc, add=T, col="green", legacy.axes=T)
      plot(meds_svm$croc, add=T, col="red", legacy.axes=T)
      plot(meds_he$croc, add=T, col="black", legacy.axes=T)
      plot(meds_koh$croc, add=T, col="orange", legacy.axes=T)
      
      ############### FIN CLASIFICADOR UPGRADE ################

###############################################################################  
############################ FIN CODIGO EJECUTABLE ############################ 
###############################################################################
