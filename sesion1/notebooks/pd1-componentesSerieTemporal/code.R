library(foreign)
library(stats)
library(graphics)
library(dplyr)

#Lectura de datos
datos <-read.spss("./data/raw/BASE SERIES TEMPORAL.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)

# Ver la estructura de la data
glimpse(datos)

#Seleccionamos variables monetarias de la serie
Desem <- select(datos, Desem)

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
DesemSerie.desc
plot(DesemSerie.desc, xlab='Año')

#Estabilizacion de la variabiliad
plot(log(DesemSerie))
x <- log(DesemSerie)

#Explorando datos
par(mfrow = c(2,1))
plot(DesemSerie)
plot(x)
par(mfrow = c(1,1))

#Tomar la primera diferencia
x <- DesemSerie
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

par(mfrow = c(2,1))
acf(DesemSerie, lag.max=34)
pacf(DesemSerie, lag.max=34)
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(dif1.x, lag.max=34)
pacf(dif1.x, lag.max=34)
par(mfrow = c(1,1))
