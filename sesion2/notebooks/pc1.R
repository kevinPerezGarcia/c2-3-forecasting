library(foreign)
library(dplyr)
library(ggplot2)
library(lattice)
library(data.table)
library(stats)

#Lectura de datos
datos <- read.csv("./data/raw/DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
dplyr::glimpse(datos) # Ver la estructura de la data

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

#Estabilizacion de la variabiliad
plot(log(Serie_Exportaciones))
x <- log(Serie_Exportaciones)

plot(log(Serie_Importaciones))
y <- log(Serie_Importaciones)

#Tomar la primera diferencia
dif1.x <- diff(x)
plot(dif1.x)

dif1.y <- diff(y)
plot(dif1.y)


par(mfrow = c(3,1))
plot(Serie_Exportaciones)
plot(log(Serie_Exportaciones))
plot(diff(log(Serie_Exportaciones)))
par(mfrow = c(1,1))



#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(diff(log(Serie_Exportaciones)), lag.max=34)
pacf(diff(log(Serie_Exportaciones)), lag.max=34)
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(diff(log(Serie_Importaciones)), lag.max=34)
pacf(diff(log(Serie_Importaciones)), lag.max=34)
par(mfrow = c(1,1))


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)

adf.test(diff(log(Serie_Exportaciones)), 
         alternative="stationary", 
         k=0)

adf.test(diff(log(Serie_Importaciones)), 
         alternative="stationary", 
         k=0)


#Prueba Phillips - Perron (PP)
pp.test(diff(log(Serie_Exportaciones), alternative="stationary"))
pp.test(diff(log(Serie_Importaciones), alternative="stationary"))


#################################################################################


#Lectura de datos
datos <- read.csv("Data_Covid.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
dplyr::glimpse(datos) # Ver la estructura de la data

CantCovid19 <- table(datos$FECHA_RESULTADO) |> as.data.frame()
colnames(CantCovid19) <- c("Periodo","CantCovid19")

Cant_Covd_19 <- dplyr::select(CantCovid19,CantCovid19)

Serie_CantCovid19 <- ts(Cant_Covd_19, start = c(2020,1,6), end = c(2021,11,11), frequency = 360)
print(Serie_CantCovid19)

plot(Serie_CantCovid19)

plot(log(Serie_CantCovid19))
plot(diff(log(Serie_CantCovid19)))

#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(diff(log(Serie_CantCovid19)), lag.max=34)
pacf(diff(log(Serie_CantCovid19)), lag.max=34)
par(mfrow = c(1,1))






