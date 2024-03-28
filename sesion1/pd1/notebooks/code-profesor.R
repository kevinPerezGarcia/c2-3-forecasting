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
library(dplyr)
library(tidyverse)
library(kableExtra)
library(scales)
library(Boruta)
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
library(GGally)
library(Hmisc)
library(e1071)  
library(class)  
library(sqldf)



############################################################################
########################## CARGA DE BASE DE DATOS ##########################
############################################################################


setwd("E:/UNI/DIPLOMADO CIENCIA DE DATOS/FINAL/FORECASTING FOR DATA SCIENCE/") #Fija el directorio de trabajo
getwd() # Ver el directorio de trabajo

#Lectura de datos
library(foreign)
datos <-read.spss("BASE SERIES TEMPORAL.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)

glimpse(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
Desem <- select(datos,Desem)

#Indicamos que sea una serie de tiempo
DesemSerie <- ts(Desem, start = c(2007,5), frequency = 12)
print(DesemSerie)

#Explorando datos
par(mfrow = c(2,1))
plot(DesemSerie)
boxplot(DesemSerie ~ cycle(DesemSerie))
par(mfrow = c(1,1))

#Descomponiendo la serie
DesemSerie.desc <- decompose(DesemSerie)
plot(DesemSerie.desc, xlab='A?o')

#Estabilizacion de la variabiliad
plot(log(DesemSerie))
x <- log(DesemSerie)

#Tomar la primera diferencia
dif1.x <- diff(x)
plot(dif1.x)

#Eliminar la estacionalidad de la serie
tsstationary <- diff(dif1.x, lag=12)
plot(tsstationary)

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)
par(mfrow = c(1,1))

