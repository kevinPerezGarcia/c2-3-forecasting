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
######################### INSTALACIÓN DE LIBRERIAS #########################
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
                   "plotly",
                   "fGarch",
                   "dynlm",
                   "quatmod",
                   "rugarch",
                   "rmgarch"))
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
library(fGarch)
library(dynlm)
library(quatmod)
library(rugarch)
library(rmgarch)

############################################################################
########################## CARGA DE BASE DE DATOS ##########################
############################################################################


setwd("E:/UNI/DIPLOMADO CIENCIA DE DATOS/FINAL/FORECASTING FOR DATA SCIENCE/") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo

#Lectura de datos
datos <- read.csv("DATOS_SERIES.csv", header = TRUE, sep = ";" ,dec = ".") %>% as.data.frame()
glimpse(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
XNT <- select(datos,XNT)

#Indicamos que sea una serie de tiempo
Serie_XNT <- ts(XNT, start = c(1994,1), end = c(2021,8), frequency = 12)
print(Serie_XNT)




####################################################################
########################## ESPECIFICACIÓN ##########################
####################################################################


#Explorando datos
par(mfrow = c(2,1))
plot(Serie_XNT)
boxplot(Serie_XNT ~ cycle(Serie_XNT))
par(mfrow = c(1,1))


####################################################################
########################## IDENTIFICACIÓN ##########################
####################################################################


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)

adf.test(Serie_XNT, 
         alternative="stationary")

#Transfromación de Box Cox
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

#Explorando datos
par(mfrow = c(2,1))
plot(Serie_XNT)
plot(Serie_XNT_boxCox)
par(mfrow = c(1,1))

#Test de raiz unitaria de la serie transformada
adf.test(Serie_XNT_boxCox, 
         alternative="stationary")

#Tomando la primera diferencia para quitar la tendencia
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
#par(mfrow = c(2,1))
#acf(Serie_XNT, lag.max=34, main = "Función de autocorrelación")
#pacf(Serie_XNT, lag.max=34, main = "Función de autocorrelación parcial")
#par(mfrow = c(1,1))

#par(mfrow = c(2,1))
#acf(Serie_XNT_boxCox, lag.max=34, main = "Función de autocorrelación")
#pacf(Serie_XNT_boxCox, lag.max=34, main = "Función de autocorrelación parcial")
#par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(D_Serie_XNT_boxCox, lag.max=34, main = "Función de autocorrelación")
pacf(D_Serie_XNT_boxCox, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

#Anbalizar la componenete estacional
par(mfrow = c(2,1))
acf(D_Serie_XNT_boxCox, lag.max=90, main = "Función de autocorrelación")
pacf(D_Serie_XNT_boxCox, lag.max=90, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))


####################################################################
############################ ESTIMACIÓN ############################
####################################################################


library(lmtest)
#especificación del modelo
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
############################ VALIDACIÓN ############################
####################################################################



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
acf(residuos_modelo9, lag.max=34, main = "Función de autocorrelación")
pacf(residuos_modelo9, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))



############################################################################
############################ HETEROCEDÁSTICIDAD ############################
############################################################################


###################
#1.- ESECIFICACIÓN
###################

#Analisis de residuales al cuadrado
checkresiduals(modelo_9)
Errores_cuadrado = resid(modelo_9)^2

par(mfrow = c(2,1))
plot(Errores_cuadrado) 
boxplot(Errores_cuadrado)
par(mfrow = c(1,1))


####################
#2.- IDENTIFICACIÓN
####################

#Funciones de autocorrelacion para los residuales al cudarado
par(mfrow = c(2,1))
acf(Errores_cuadrado, lag.max=34, main = "Función de autocorrelación")
pacf(Errores_cuadrado, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

#Prueba de componenete ARCH en el modelo
install.packages("FinTS")
library(FinTS)

ModeloARCH1 = ArchTest(D_Serie_XNT_boxCox, lags = 1, demean = TRUE)
ModeloARCH1

ModeloARCH2 = ArchTest(D_Serie_XNT_boxCox, lags = 2, demean = TRUE)
ModeloARCH2

ModeloARCH3 = ArchTest(D_Serie_XNT_boxCox, lags = 3, demean = TRUE)
ModeloARCH3

ModeloARCH4 = ArchTest(D_Serie_XNT_boxCox, lags = 4, demean = TRUE)
ModeloARCH4


####################
#3.- ESTIMACIÓN
####################

#Plnateamiento del modelo ARCH / GARCH
#Modelo ARMA (1,1)
library(rugarch)
ugarch0 = ugarchspec()
ugarch0



#Modelo GARCH (0,1)
ugarch01 = ugarchspec(mean.model = list(armaOrder = c(0,1)))
ugfit01 = ugarchfit(spec = ugarch01, data = D_Serie_XNT_boxCox)
ugfit01

#Modelo GARCH (0,2)
ugarch02 = ugarchspec(mean.model = list(armaOrder = c(0,2)))
ugfit02 = ugarchfit(spec = ugarch02, data = D_Serie_XNT_boxCox)
ugfit02

#Modelo GARCH (0,3)
ugarch03 = ugarchspec(mean.model = list(armaOrder = c(0,3)))
ugfit03 = ugarchfit(spec = ugarch03, data = D_Serie_XNT_boxCox)
ugfit03

#Modelo GARCH (0,4)
ugarch04 = ugarchspec(mean.model = list(armaOrder = c(0,4)))
ugfit04 = ugarchfit(spec = ugarch04, data = D_Serie_XNT_boxCox)
ugfit04



#Modelo GARCH (1,1)
ugarch11 = ugarchspec(mean.model = list(armaOrder = c(1,1)))
ugfit11 = ugarchfit(spec = ugarch11, data = D_Serie_XNT_boxCox)
ugfit11

#Modelo GARCH (1,2)
ugarch12 = ugarchspec(mean.model = list(armaOrder = c(1,2)))
ugfit12 = ugarchfit(spec = ugarch12, data = D_Serie_XNT_boxCox)
ugfit12

#Modelo GARCH (1,3)
ugarch13 = ugarchspec(mean.model = list(armaOrder = c(1,3)))
ugfit13 = ugarchfit(spec = ugarch13, data = D_Serie_XNT_boxCox)
ugfit13

#Modelo GARCH (1,4)
ugarch14 = ugarchspec(mean.model = list(armaOrder = c(1,4)))
ugfit14 = ugarchfit(spec = ugarch14, data = D_Serie_XNT_boxCox)
ugfit14



#Modelo GARCH (2,1)
ugarch21 = ugarchspec(mean.model = list(armaOrder = c(2,1)))
ugfit21 = ugarchfit(spec = ugarch21, data = D_Serie_XNT_boxCox)
ugfit21

#Modelo GARCH (2,2)
ugarch22 = ugarchspec(mean.model = list(armaOrder = c(2,2)))
ugfit22 = ugarchfit(spec = ugarch22, data = D_Serie_XNT_boxCox)
ugfit22

#Modelo GARCH (2,3)
ugarch23 = ugarchspec(mean.model = list(armaOrder = c(2,3)))
ugfit23 = ugarchfit(spec = ugarch23, data = D_Serie_XNT_boxCox)
ugfit23

#Modelo GARCH (2,4)
ugarch24 = ugarchspec(mean.model = list(armaOrder = c(2,4)))
ugfit24 = ugarchfit(spec = ugarch24, data = D_Serie_XNT_boxCox)
ugfit24


#Modelo GARCH (3,1)
ugarch31 = ugarchspec(mean.model = list(armaOrder = c(3,1)))
ugfit31 = ugarchfit(spec = ugarch31, data = D_Serie_XNT_boxCox)
ugfit31

#Modelo GARCH (3,2)
ugarch32 = ugarchspec(mean.model = list(armaOrder = c(3,2)))
ugfit32 = ugarchfit(spec = ugarch32, data = D_Serie_XNT_boxCox)
ugfit32

#Modelo GARCH (3,3)
ugarch33 = ugarchspec(mean.model = list(armaOrder = c(3,3)))
ugfit33 = ugarchfit(spec = ugarch33, data = D_Serie_XNT_boxCox)
ugfit33

#Modelo GARCH (3,4)
ugarch34 = ugarchspec(mean.model = list(armaOrder = c(3,4)))
ugfit34 = ugarchfit(spec = ugarch34, data = D_Serie_XNT_boxCox)
ugfit34




####################
#4.- VALIDACIÓN
####################



#validaciones al modelo garch (3,3)

ugfit33@fit$coef #Coeficiientes

ug_var = ugfit33@fit$var
ug_var

autoplot(ts(ug_var)) + 
  geom_line(color = "green") + labs(title = "Varianza de nuestro modelo ARMA (3,3)")

#Analisis de los cuadrados de los residuales de la nueva serie
ug_resid = (ugfit3@fit$residuals)^2
autoplot(ts(ug_resid)) + autolayer(ts(ug_resid), series = "Residuales al cuadrado") + autolayer(ts(ug_var), series = "Varianza") + labs(title = "Residuales al cuadrado y Varianza de nuestro modelo ARMA (2,1)") + ylab("") + xlab("Tiempo")

#Correlogramas de los residuales al cuadrado de la nueva serie
par(mfrow = c(2,1))
acf(ug_resid, lag.max=34, main = "Función de autocorrelación")
pacf(ug_resid, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

checkresiduals(ugfit3@fit$residuals)


