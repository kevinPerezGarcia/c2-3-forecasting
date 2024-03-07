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
                   "forescast",
                   "fpp2",
                   "foreign",
                   "ggfortify",
                   "TSA",
                   "ggplot2",
                   "gridExtra",
                   "seasonal",
                   "lattice",
                   "zoo",
                   "urca",
                   "dynlm",
                   "lubridate"))
library(ggplot2)
library(caret)
library(DataExplorer)
library(VIM)
library(missForest)
library(ggplot2)
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
library(forescast)
library(fpp2)
library(lubridate)
library(foreign)
library(ggfortify)
library(TSA)
library(ggplot2)
library(gridExtra)
library(seasonal)
library(lattice)
library(zoo)
library(urca)
library(dynlm)

############################################################################
########################## CARGA DE BASE DE DATOS ##########################
############################################################################

setwd("E:/UNI/DIPLOMADO CIENCIA DE DATOS/FINAL/FORECASTING FOR DATA SCIENCE/BASE DATOS") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo

#Lectura de datos
datos <- read.csv("DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
str(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
Exportaciones <- dplyr::select(datos,XNT)
Importaciones <- dplyr::select(datos,IMP)

#Indicamos que sea una serie de tiempo
Serie_Exportaciones <- ts(Exportaciones, start = c(1994,1), end = c(2021,8), frequency = 12)
Serie_Importaciones <- ts(Importaciones, start = c(1994,1), end = c(2021,8), frequency = 12)
print(Serie_Exportaciones)
print(Serie_Importaciones)

#Explorando datos
par(mfrow = c(2,1))
plot(Serie_Exportaciones)
boxplot(Serie_Exportaciones ~ cycle(Serie_Exportaciones))
par(mfrow = c(1,1))

par(mfrow = c(2,1))
plot(Serie_Importaciones)
boxplot(Serie_Importaciones ~ cycle(Serie_Importaciones))
par(mfrow = c(1,1))


#Descomponiendo la serie
Desc_Exportaciones <- decompose(Serie_Exportaciones)
plot(Desc_Exportaciones, xlab='Año')





######################################################################
################### SUAVIZACION POR MEDIAS MOVILES ###################
######################################################################


library(foreign)
library(forecast)
library(ggplot2)
library(gridExtra)
library(seasonal)
library(lattice)
library(zoo)
library(urca)
library(dynlm)

install.packages("smooth")
library(smooth)

ggseasonplot(Serie_Exportaciones,
             year.labels = TRUE,
             year.labels.left = TRUE) +
ylab("Millones de soles") + 
xlab("Meses") + 
ggtitle("EXportaciones del Per?")




######################################################################
################### SUAVIZACION POR MEDIAS MOVILES ###################
######################################################################



plot(Serie_Exportaciones)
Mediamovil1 <- sma(Serie_Exportaciones, order = 3)
pronostico1 <- forecast(Mediamovil1, h=10)
plot(forecast(pronostico1))

plot(Serie_Exportaciones)
Mediamovil2 <- sma(Serie_Exportaciones, order = 4)
pronostico2 <- forecast(Mediamovil2, h=10)
plot(forecast(pronostico2))


plot(Serie_Exportaciones)
Mediamovil3 <- sma(Serie_Exportaciones, order = 5)
pronostico3 <- forecast(Mediamovil3, h=10)
plot(forecast(pronostico3))



###############################################################
################### SUAVIZACION EXPONENCIAL ###################
###############################################################


##SIN TENDENCIAS

#Tomar la primera diferencia
D_Serie_Exportaciones <- diff(Serie_Exportaciones)
plot(D_Serie_Exportaciones)



#Exportaciones
#Suavizacion exponencial
Fit_Ses_1 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.1)
Fit_Ses_1
summary(Fit_Ses_1)
Pronos1 <- forecast::forecast(Fit_Ses_1,h=1)
plot(Pronos1)

Fit_Ses_2 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.5)
Fit_Ses_2
summary(Fit_Ses_2)
Pronos2 <- forecast::forecast(Fit_Ses_2,h=1)
plot(Pronos2)

Fit_Ses_3 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.9)
Fit_Ses_3
summary(Fit_Ses_3)
Pronos3 <- forecast::forecast(Fit_Ses_3,h=1)
plot(Pronos3)

#Calculo de alfa optimo
Fit_Ses_4 <- forecast::ses(Serie_Exportaciones, h = 1, initial = "simple")
Fit_Ses_4
summary(Fit_Ses_4)
Pronos4 <- forecast::forecast(Fit_Ses_4,h=1)
plot(Pronos4)

Fit_Ses_5 <- forecast::ses(D_Serie_Exportaciones, h = 1,alpha = 0.6898)
Fit_Ses_5
summary(Fit_Ses_5)
Pronos5 <- forecast::forecast(Fit_Ses_5,h=1)
plot(Pronos5)

#COMPARACI?N DE ERROR CUADR?TICO MEDIO
summary(Fit_Ses_1)
summary(Fit_Ses_2)
summary(Fit_Ses_3)
summary(Fit_Ses_4)



#CON TENDENCIAS

#Suavizaci?n exponancial considerando varios facotores
FitHolt <- HoltWinters(Serie_Exportaciones, alpha = 0.6928873, beta = TRUE, gamma = TRUE)
summary(FitHolt)
Pronos_FitHolt <- forecast::forecast(FitHolt,h=1)
hist(Pronos_FitHolt$residuals)
Pronos_FitHolt$model

forecast::

Fit_Ses_1_ets <- forecast::ets(Serie_Exportaciones)
Fit_Ses_1_ets
summary(Fit_Ses_1_ets)
Pronos1_ets <- forecast::forecast(Fit_Ses_1_ets,h=1)
Pronos1_ets
plot(Pronos1_ets)

#####################################################################################################

#### MODELADO DATA COVID 19


#Lectura de datos
datos <- read.csv("Data_Covid.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
dplyr::glimpse(datos) # Ver la estructura de la data

CantCovid19 <- table(datos$FECHA_RESULTADO) |> as.data.frame()
colnames(CantCovid19) <- c("Periodo","CantCovid19")

Cant_Covd_19 <- dplyr::select(CantCovid19,CantCovid19)

Serie_CantCovid19 <- ts(Cant_Covd_19, start = c(2020,1,6), end = c(2021,11,11), frequency = 360)
print(Serie_CantCovid19)

plot(Serie_CantCovid19)

install.packages("TTR")
library(TTR)

library(smooth)
library(forecast)

#medias moviles
Mediamovil_Covid <- sma(Serie_CantCovid19, order = 5)
pronostico_covid <- forecast::forecast(Mediamovil_Covid, h=10)
plot(forecast(pronostico_covid))

#SuavizAci?n exponencial
#Simple
#Tomar la primera diferencia
D_Serie_CantCovid19 <- diff(Serie_CantCovid19)
plot(D_Serie_Exportaciones)

#Exponencial simple
Fit_Ses_Covid19 <- forecast::ses(D_Serie_CantCovid19, h = 1, initial = "simple")
Fit_Ses_Covid19 
summary(Fit_Ses_Covid19)
Pronos_Covid <- forecast::forecast(Fit_Ses_Covid19,h=1)
plot(Pronos_Covid)

#Exponencia doble
FitHolt <- HoltWinters(Serie_CantCovid19, alpha = 0.1, beta = TRUE, gamma = FALSE)
summary(FitHolt)
Pronos_FitHolt <- forecast(FitHolt,h=1)
Pronos_FitHolt


