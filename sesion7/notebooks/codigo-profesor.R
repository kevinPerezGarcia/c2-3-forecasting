#####################################
#                                   #
#           MODELO ARIMAX           #
#    MAG.OMAR CHINCARO DEL CORAL    #   
#       oachincaro@gmail.com        #
#                                   #
#####################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#---------------------------------------------------------



############################################################################
######################### INSTALACI?N DE LIBRERIAS #########################
############################################################################



# Cargando los paquetes
install.packages(c("readxl",
                   "tseries",
                   "stargazer",
                   "lmtest",
                   "car",
                   "strucchange",
                   "TSstudio",
                   "forecast",
                   "ggplot2",
                   "caret",
                   "DataExplorer",
                   "ggplot2",
                   "data.table",
                   "randomForest",
                   "MASS", 
                   "pROC",
                   "foreign",
                   "gmodels",
                   "caTools",
                   "dplyr",
                   "tidyverse",
                   "psych",
                   "corrplot",
                   "seasonal",
                   "tseries",
                   "fGarch",
                   "dynlm"))

library(readxl)
library(tseries)
library(stargazer)
library(lmtest)
library(car)
library(strucchange)
library(TSstudio)
library(forecast)
library(ggplot2)
library(caret)
library(DataExplorer)
library(ggplot2)
library(data.table)
library(randomForest)
library(MASS) 
library(pROC)
library(foreign)
library(gmodels)
library(caTools)
library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
library(seasonal)
library(tseries)
library(fGarch)
library(dynlm)







############################################################################
########################## CARGA DE BASE DE DATOS ##########################
############################################################################


setwd("E:/UNI/MAESTRIA EN CIENCIA DE DATOS/ARCHIVOS FINALES/FORECASTING FOR DATA SCIENCE/BASE DATOS/") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo

#Lectura de datos
datos <- read.csv("DATOS_ARIMAX.csv", header = TRUE, sep = ";" ,dec = ".") %>% as.data.frame()
glimpse(datos) # Ver la estructura de la data

#Seleccionamos la variable respuesta de la swerie
XNPT <- select(datos,XNPT)
XPNOT <- select(datos,XPNOT)
XOTROS <- select(datos,XOTROS)
IMPBCON <- select(datos,IMPBCON)
IMPINS <- select(datos,IMPINS)
IMPBCAP <- select(datos,IMPBCAP)
IMPOTROS <- select(datos,IMPOTROS)
BAL <- select(datos,BAL)

#Indicamos que sea una serie de tiempo
Serie_ExporTrad <- ts(XNPT,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ExportNoTrad <- ts(XPNOT,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ExportOtros <- ts(XOTROS,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ImporBienCons <- ts(IMPBCON,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ImporBienIns <- ts(IMPINS,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ImporBienCap <- ts(IMPBCAP,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_ImporOtros <- ts(IMPOTROS,start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_Balanza <- ts(BAL,start = c(1994,1), end = c(2021,8), frequency = 12)

#print(Serie_Balanza)



####################################################################
########################## ESPECIFICACIóN ##########################
####################################################################


par(mfrow = c(3,3))
plot(Serie_ExporTrad)
plot(Serie_ExportNoTrad)
plot(Serie_ExportOtros)
plot(Serie_ImporBienCons)
plot(Serie_ImporBienIns)
plot(Serie_ImporBienCap)
plot(Serie_ImporOtros)
plot(Serie_Balanza)
par(mfrow = c(1,1))


####################################################################
########################## IDENTIFICACIóN ##########################
####################################################################


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)
adf.test((Serie_ExporTrad),alternative="stationary")
adf.test((Serie_ExportNoTrad),alternative="stationary")
adf.test((Serie_ExportOtros),alternative="stationary")
adf.test((Serie_ImporBienCons),alternative="stationary")
adf.test((Serie_ImporBienIns),alternative="stationary")
adf.test((Serie_ImporBienCap),alternative="stationary")
adf.test((Serie_ImporOtros),alternative="stationary")
adf.test((Serie_Balanza),alternative="stationary")


#Transfromaci?n de Box Cox
lamb.x01 <- BoxCox.lambda(Serie_ExporTrad)
lamb.x02 <- BoxCox.lambda(Serie_ExportNoTrad)
lamb.x03 <- BoxCox.lambda(Serie_ExportOtros)
lamb.x04 <- BoxCox.lambda(Serie_ImporBienCons)
lamb.x05 <- BoxCox.lambda(Serie_ImporBienIns)
lamb.x06 <- BoxCox.lambda(Serie_ImporBienCap)
lamb.x07 <- BoxCox.lambda(Serie_ImporOtros)
lamb.x08 <- BoxCox.lambda(Serie_Balanza)

lamb.x01
lamb.x02
lamb.x03
lamb.x04
lamb.x05
lamb.x06
lamb.x07
lamb.x08


#Realiza la transformacion BOX - COX
tran_box <- function(xt,lamb.x){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1)/lamb.x
  }
}

Serie_ExporTrad_boxCox <- tran_box(Serie_ExporTrad,lamb.x01)
Serie_ExportNoTrad_boxCox <- tran_box(Serie_ExportNoTrad,lamb.x02)
Serie_ExportOtros_boxCox <- tran_box(Serie_ExportOtros,lamb.x03)
Serie_ImporBienCons_boxCox <- tran_box(Serie_ImporBienCons,lamb.x04)
Serie_ImporBienIns_boxCox <- tran_box(Serie_ImporBienIns,lamb.x05)
Serie_ImporBienCap_boxCox <- tran_box(Serie_ImporBienCap,lamb.x06)
Serie_ImporOtros_boxCox <- tran_box(Serie_ImporOtros,lamb.x07)
#Serie_Balanza_boxCox <- tran_box(Serie_Balanza,lamb.x08)

#Tomando primera diferencias
D_Serie_ExporTrad_boxCox <- diff(Serie_ExporTrad_boxCox,differences = 1)
D_Serie_ExportNoTrad_boxCox <- diff(Serie_ExportNoTrad_boxCox,differences = 1)
D_Serie_ExportOtros_boxCox <- diff(Serie_ExportOtros_boxCox,differences = 1)
D_Serie_ImporBienCons_boxCox <- diff(Serie_ImporBienCons_boxCox,differences = 1)
D_Serie_ImporBienIns_boxCox <- diff(Serie_ImporBienIns_boxCox,differences = 1)
D_Serie_ImporBienCap_boxCox <- diff(Serie_ImporBienCap_boxCox,lamb.differences = 1)
D_Serie_ImporOtros_boxCox <- diff(Serie_ImporOtros_boxCox,lamb.differences = 1)
D_Serie_Balanza <- diff(Serie_Balanza,differences = 1)

#Correlogramas
ts_cor(D_Serie_ExporTrad_boxCox, lag = 24)
ts_cor(D_Serie_ExportNoTrad_boxCox, lag = 24)
ts_cor(D_Serie_ExportOtros_boxCox, lag = 24)
ts_cor(D_Serie_ImporBienCons_boxCox, lag = 24)
ts_cor(D_Serie_ImporBienIns_boxCox, lag = 24)
ts_cor(D_Serie_ImporBienCap_boxCox, lag = 24)
ts_cor(D_Serie_ImporOtros_boxCox, lag = 24)
ts_cor(D_Serie_Balanza, lag = 24)




####################################################################
############################ ESTIMACIÓN ############################
####################################################################

library(lmtest)

#Se toma una muestra del 80%
M_Serie_ExporTrad <- ts(XNPT,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ExportNoTrad <- ts(XPNOT,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ExportOtros <- ts(XOTROS,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ImporBienCons <- ts(IMPBCON,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ImporBienIns <- ts(IMPINS,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ImporBienCap <- ts(IMPBCAP,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_ImporOtros <- ts(IMPOTROS,start = c(1994,1), end = c(2016,12), frequency = 12)
M_Serie_Balanza <- ts(BAL,start = c(1994,1), end = c(2016,12), frequency = 12)

DataSeries <- cbind(M_Serie_ExporTrad,M_Serie_ExportNoTrad,M_Serie_ExportOtros,
                    M_Serie_ImporBienCons,M_Serie_ImporBienIns,M_Serie_ImporBienCap,
                    M_Serie_ImporOtros,M_Serie_Balanza)


M_Exogenas <- cbind(M_Serie_ExporTrad,M_Serie_ExportNoTrad,
                    M_Serie_ImporBienCons,M_Serie_ImporBienIns,M_Serie_ImporBienCap)



correlacion <- round(cor(M_Exogenas),2)
correlacion
corrplot(correlacion, method="number", type="upper")

#library(randomForest)
#library(Boruta)
#Seleccion de variables
#boruta_output <- Boruta(as.data.frame(M_Serie_Balanza) ~ M_Serie_ExporTrad+M_Serie_ExportNoTrad+M_Serie_ExportOtros+
#                          M_Serie_ImporBienCons+M_Serie_ImporBienIns+M_Serie_ImporBienCap+M_Serie_ImporOtros, 
#                        data = as.data.frame(DataSeries), 
#                        doTrace=2)  
#boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
#print(boruta_signif)  # significant variables
#plot(boruta_output, 
#     cex.axis=.7, 
#     las=2, 
#     xlab="", 
#     main="Importancia de variables")  # plot variable importance

#DataSeriesFinal <- boruta_signif #Data con variables finales a modelar

#plotImpHistory(boruta_output)
#bank_df <- attStats(boruta_output)
#print(bank_df)



#Corremos el modelo autorima para la variable respuesta
modelo = auto.arima(M_Serie_Balanza)
summary(modelo)
#coeftest(modelo)


# Estimacion del modelo
library(TSA)
arimax <- arimax(M_Serie_Balanza, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12),
                 xreg = cbind(M_Serie_ExporTrad,M_Serie_ExportNoTrad,
                              M_Serie_ImporBienCons,M_Serie_ImporBienIns,M_Serie_ImporBienCap),
                 include.mean = TRUE,
                 method = "ML")
summary(arimax)

#Coeficientes del modelo
coeftest(arimax)
stargazer(arimax, type = "text")



####################################################################
############################ VALIDACIÓN ############################
####################################################################


#Residuales
residuals <- resid(arimax)
adf.test(residuals)
checkresiduals(arimax)


residuos_modelo = resid(arimax)
res_2 <- residuos_modelo*residuos_modelo
par(mfrow = c(3,1))
hist(residuos_modelo)
boxplot(residuos_modelo)
plot(residuos_modelo)
par(mfrow = c(1,1))
residuos_modelo = residuos_modelo[!is.na(residuos_modelo)]
jarque.bera.test(residuos_modelo)


#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(res_2, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(res_2, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

checkresiduals(residuos_modelo)



#####################################################################
############################ PRONÓSTICOS ############################
#####################################################################



#Muestra de validación
M2_Serie_ExporTrad <- ts(XNPT,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ExportNoTrad <- ts(XPNOT,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ExportOtros <- ts(XOTROS,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ImporBienCons <- ts(IMPBCON,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ImporBienIns <- ts(IMPINS,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ImporBienCap <- ts(IMPBCAP,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_ImporOtros <- ts(IMPOTROS,start = c(2017,1), end = c(2021,8), frequency = 12)
M2_Serie_Balanza <- ts(BAL,start = c(2017,1), end = c(2021,8), frequency = 12)


M2_Exogenas <- cbind(M2_Serie_ExporTrad,M2_Serie_ExportNoTrad,
                     M2_Serie_ImporBienCons,M2_Serie_ImporBienIns,M2_Serie_ImporBienCap)

#Predicción
pred <- predict(arimax, newxreg = M2_Exogenas)
pred 

Prediccion <- pred$pred 

V_DataSeries <- cbind(M2_Serie_ExporTrad,M2_Serie_ExportNoTrad,
                      M2_Serie_ImporBienCons,M2_Serie_ImporBienIns,M2_Serie_ImporBienCap,
                      M2_Serie_Balanza,Prediccion)

#Calculo de las metricas de performance de la muestra de validación

DataVal <- as.data.frame(V_DataSeries)

rmse <- sqrt(mean((DataVal$Prediccion - DataVal$M2_Serie_Balanza)^2))
mape <- mean(abs((DataVal$Prediccion - DataVal$M2_Serie_Balanza) / DataVal$M2_Serie_Balanza)) * 100

plot(DataVal$M2_Serie_Balanza,DataVal$Prediccion, xlab = "Balanza Comercial",ylab="Pronóstico")






