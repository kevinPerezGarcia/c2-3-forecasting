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
IMP <- select(datos,IMP)

#Indicamos que sea una serie de tiempo
Serie_IMP <- ts(IMP, start = c(1994,1), end = c(2021,8), frequency = 12)
print(Serie_IMP)




####################################################################
########################## ESPECIFICACI?N ##########################
####################################################################


#Explorando datos
par(mfrow = c(2,1))
plot(Serie_IMP)
boxplot(Serie_IMP ~ cycle(Serie_IMP))
par(mfrow = c(1,1))


####################################################################
########################## IDENTIFICACI?N ##########################
####################################################################


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)

adf.test(Serie_IMP, 
         alternative="stationary")

#Transfromaci?n de Box Cox
library(forecast)
lamb.x <- BoxCox.lambda(Serie_IMP)
lamb.x

##realiza la transformacion
tran_box <- function(xt,lambda){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1)/lamb.x
  }
}

Serie_IMP_boxCox <- tran_box(Serie_IMP, lamb.x)
plot(Serie_IMP_boxCox)

#Explorando datos
par(mfrow = c(2,1))
plot(Serie_IMP)
plot(Serie_IMP_boxCox)
par(mfrow = c(1,1))

#Test de raiz unitaria de la serie transformada
adf.test(Serie_IMP_boxCox, 
         alternative="stationary")

#Tomando la primera diferencia para quitar la tendencia
D_Serie_IMP_boxCox = diff(Serie_IMP_boxCox, differences = 1)
plot(D_Serie_IMP_boxCox)

adf.test(D_Serie_IMP_boxCox, 
         alternative="stationary")

#Explorando datos
par(mfrow = c(3,1))
plot(Serie_IMP)
plot(Serie_IMP_boxCox)
plot(D_Serie_IMP_boxCox)
par(mfrow = c(1,1))


#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(D_Serie_IMP_boxCox, lag.max=90, main = "Funci?n de autocorrelaci?n")
pacf(D_Serie_IMP_boxCox, lag.max=90, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))


####################################################################
############################ ESTIMACI?N ############################
####################################################################


library(lmtest)
#especificaci?n del modelo
modelo = auto.arima(Serie_IMP_boxCox)
summary(modelo)
coeftest(modelo)


####################################################################
############################ VALIDACI?N ############################
####################################################################



#Analisis de residuales
#Normalidad
residuos_modelo = modelo$residuals
res_2 <- residuos_modelo*residuos_modelo
hist(residuos_modelo)
boxplot(residuos_modelo)
plot(residuos_modelo)
residuos_modelo = residuos_modelo[!is.na(residuos_modelo)]
jarque.bera.test(residuos_modelo)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(residuos_modelo, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(residuos_modelo, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

checkresiduals(residuos_modelo)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(res_2, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(res_2, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

checkresiduals(res_2)



####################################################################
############################ PRONOSTICOS ############################
####################################################################


prediccion <- forecast(modelo, h=4) #nivel confianza 95%, h = periodos
autoplot(prediccion)
