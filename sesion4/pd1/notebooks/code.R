#####################################
#                                   #
#           SERIES TEMPORAL         #
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
install.packages(c("caret",
                   "DataExplorer",
                   "VIM",
                   "missForest",
                   "ggplot2",
                   "dummies",
                   "lattice",
                   "colorspace",
                   "data.table",
                   "randomForest",
                   "foreach",
                   "itertools",
                   "MASS",
                   "pROC",
                   "foreign",
                   "gmodels",
                   "InformationValue",
                   "caTools",
                   "MLmeTrics",
                   "dplyr",
                   "iterators",
                   "MLmeTrics",
                   "tidyverse",
                   "kableExtra",
                   "scales",
                   "Boruta",
                   "BRugs",
                   "R2OpenBUGS",
                   "factoextra",
                   "mvoutlier",
                   "outliers",
                   "cluster",
                   "fpc",
                   "mclust",
                   "dbscan",
                   "readxl",
                   "psych",
                   "corrplot",
                   "mclust",
                   "gclus", 
                   "rrcov",
                   "tourr",
                   "aplpack",
                   "TeachingDemos",
                   "rgl",
                   "ape",
                   "DMwR",
                   "GGally",
                   "Hmisc",
                   "PerformanceAnalytics",
                   "e1071",
                   "class",
                   "sqldf",
                   "lubridate",
                   "aTSA",
                   "mFilter",
                   "seasonal",
                   "tseries",
                   "forecast",
                   "TSstudio",
                   "plotly"))
library(ggplot2)
library(caret)
library(DataExplorer)
library(VIM)
library(missForest)
library(ggplot2)
library(dummies)
library(lattice)
library(colorspace)
library(data.table)
library(randomForest)
library(foreach)
library(itertools)
library(MASS) 
library(pROC)
library(foreign)
library(gmodels)
library(InformationValue)
library(caTools)
library(MLmeTrics)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(scales)
library(Boruta)
library(BRugs)
library(R2OpenBUGS)
library(factoextra)
library(mvoutlier)
library(outliers)
library(cluster)
library(fpc)
library(mclust)
library(dbscan)
library(readxl)
library(psych)
library(corrplot)
library(mclust)
library(gclus) 
library(rrcov)
library(tourr)
library(aplpack)
library(TeachingDemos)
library(rgl)
library(ape)
library(DMwR)
library(GGally)
library(Hmisc)
library(PerformanceAnalytics)
library(e1071)  
library(class)  
library(sqldf)
library(lubridate)
library(aTSA)
library(mFilter)
library(seasonal)
library(tseries)
library(forecast)
library(TSstudio)
library(plotly)
library(readxl) 
library(tidyr)
library(dplyr)
library(readxl)



############################################################################
########################## CARGA DE BASE DE DATOS ##########################
############################################################################


setwd("E:/UNI/DIPLOMADO CIENCIA DE DATOS/FINAL/FORECASTING FOR DATA SCIENCE/BASE DATOS/") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo


#Lectura de datos
datos <- read.csv("DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
dplyr::glimpse(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
XNT <- dplyr::select(datos,XNT)

#Indicamos que sea una serie de tiempo
Serie_XNT <- ts(XNT, start = c(1994,1), end = c(2021,8), frequency = 12)
print(Serie_XNT)

Serie_XNT_1 <- ts(XNT, start = c(1994,1), end = c(2005,12), frequency = 12)
print(Serie_XNT_1)

Serie_XNT_2 <- ts(XNT, start = c(2006,1), end = c(2021,8), frequency = 12)
print(Serie_XNT_2)


####################################################################
########################## ESPECIFICACI?N ##########################
####################################################################


#Explorando datos
par(mfrow = c(2,1))
plot(Serie_XNT)
boxplot(Serie_XNT ~ cycle(Serie_XNT))
par(mfrow = c(1,1))


#Explorando datos
par(mfrow = c(3,1))
plot(Serie_XNT,xlab = "Tiempo",ylab = "Exportaciones")
plot(Serie_XNT_1,xlab = "Tiempo",ylab = "Exportaciones")
plot(Serie_XNT_2,xlab = "Tiempo",ylab = "Exportaciones")
par(mfrow = c(1,1))




####################################################################
########################## IDENTIFICACI?N ##########################
####################################################################


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)

adf.test(Serie_XNT, 
         alternative="stationary")

#Transfromaci?n de Box Cox
library(forecast)
lamb.x <- BoxCox.lambda(Serie_XNT)
lamb.x

##realiza la transformacion
tran_box <- function(xt,lambda){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1)/lamb.x
  }
}

Serie_XNT_boxCox <- tran_box(Serie_XNT, lamb.x)
plot(Serie_XNT_boxCox)
adf.test(Serie_XNT_boxCox, 
         alternative="stationary")

#Tomando la primera diferencia
D_Serie_XNT_boxCox = diff(Serie_XNT_boxCox, differences = 1)
plot(D_Serie_XNT_boxCox)

adf.test(D_Serie_XNT_boxCox, 
         alternative="stationary")

#Explorando datos
par(mfrow = c(3,1))
plot(Serie_XNT)
plot(Serie_XNT_boxCox)
plot(D_Serie_XNT_boxCox)
par(mfrow = c(1,1))


#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(Serie_XNT, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(Serie_XNT, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(Serie_XNT_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(Serie_XNT_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(D_Serie_XNT_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(D_Serie_XNT_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))



####################################################################
############################ ESTIMACI?N ############################
####################################################################

library(lmtest)
#especificaci?n del modelo
modelo_1 = arima(Serie_XNT_boxCox, order = c(1,1,1))
summary(modelo_1)
coeftest(modelo_1)

modelo_2 = arima(Serie_XNT_boxCox, order = c(1,1,2))
summary(modelo_2)
coeftest(modelo_2)

modelo_3 = arima(Serie_XNT_boxCox, order = c(2,1,1))
summary(modelo_3)
coeftest(modelo_3)

modelo_4 = arima(Serie_XNT_boxCox, order = c(2,1,2))
summary(modelo_4)
coeftest(modelo_4)


#Tomando en cuenta la componente estacional
modelo_5 = arima(Serie_XNT_boxCox, 
                 order = c(2,1,1),
                 seasonal = list(order = c(1,1,0), period = 12,method="ML"))
summary(modelo_5)
coeftest(modelo_5)

#modelo_6 = arima(Serie_XNT_boxCox, 
#                 order = c(2,1,1),
#                 seasonal = list(order = c(2,1,0), period = 12,method="ML"))
#summary(modelo_6)
#coeftest(modelo_6)

modelo_7 = arima(Serie_XNT_boxCox, 
                 order = c(0,1,0),
                 seasonal = list(order = c(3,1,0), period = 12,method="ML"))
summary(modelo_7)
coeftest(modelo_7)

modelo_8 = auto.arima(Serie_XNT_boxCox)
summary(modelo_8)
coeftest(modelo_8)

#Mejor modelo
modelo_9 = arima(Serie_XNT_boxCox, 
                 order = c(1,1,0),
                 seasonal = list(order = c(2,0,0), period = 12,method="ML"))
summary(modelo_9)
coeftest(modelo_9)




####################################################################
############################ VALIDACI?N ############################
####################################################################

#MODELO 1
#Analisis de residuales
#Normalidad
residuos_modelo5 = modelo_5$residuals
hist(residuos_modelo5)
boxplot(residuos_modelo5)
plot(residuos_modelo5)
residuos_modelo5 = residuos_modelo5[!is.na(residuos_modelo5)]
jarque.bera.test(residuos_modelo5)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo5, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo5, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))


#MODELO 2
#Analisis de residuales
#Normalidad
residuos_modelo7 = modelo_7$residuals
hist(residuos_modelo7)
boxplot(residuos_modelo7)
plot(residuos_modelo7)
residuos_modelo5 = residuos_modelo7[!is.na(residuos_modelo7)]
jarque.bera.test(residuos_modelo7)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo7, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo7, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))


#MODELO 3
#Analisis de residuales
#Normalidad
residuos_modelo3 = modelo_3$residuals
hist(residuos_modelo3)
boxplot(residuos_modelo3)
plot(residuos_modelo3)
residuos_modelo5 = residuos_modelo3[!is.na(residuos_modelo3)]
jarque.bera.test(residuos_modelo3)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo3, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo3, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))


#MODELO 4
#Analisis de residuales
#Normalidad
residuos_modelo9 = modelo_9$residuals
hist(residuos_modelo9)
boxplot(residuos_modelo9)
plot(residuos_modelo9)
residuos_modelo9 = residuos_modelo9[!is.na(residuos_modelo9)]
jarque.bera.test(residuos_modelo9)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo9, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo9, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))


