library(foreign)
library(dplyr)
library(ggplot2)
library(lattice)
library(data.table)
library(stats)

datos <-read.spss("./data/raw/BASE SERIES TEMPORAL.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)

dplyr::glimpse(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
Desem <- dplyr::select(datos,Desem)

#Indicamos que sea una serie de tiempo
DesemSerie <- ts(Desem, start = c(2007,5), frequency = 12)
print(DesemSerie)

#Explorando datos
par(mfrow = c(2,1))
plot(DesemSerie)
boxplot(DesemSerie ~ cycle(DesemSerie))
par(mfrow = c(1,1))

#Estabilizacion de la variabiliad (Varianza cte de los residuales)

par(mfrow = c(2,1))
plot(DesemSerie)
plot(log(DesemSerie))
par(mfrow = c(1,1))

par(mfrow = c(2,1))
plot(log(DesemSerie), main = "Serie - Estacionariedad en varianza")
plot(DesemSerie, main = "Serie - Original")
par(mfrow = c(1,1))

x <- log(DesemSerie)

#Tomar la primera diferencia
dif1.x <- diff(x)
plot(dif1.x)

#Funciones de autocorrelacion
# sin tomar primeras diferencias: en presencia de tendencia
par(mfrow = c(2,1))
acf(DesemSerie, main = "Función de autocorrelación", lag.max=34)
pacf(DesemSerie,main = "Función de autocorellación parcial", lag.max=34)
par(mfrow = c(1,1))

#Funciones de autocorrelacion
# Con primeras diferencias
par(mfrow = c(2,1))
acf(diff(DesemSerie), main = "Función de autocorrelación", lag.max=34)
pacf(diff(DesemSerie),main = "Función de autocorellación parcial", lag.max=34)
par(mfrow = c(1,1))

# Pruebas estadísticas de tendencia

#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
# H0: serie no estacionaria: con tendencias
library(tseries)

adf.test(DesemSerie, 
         alternative="stationary", 
         k=0)
# Aceptamos la hipótesis nula

#Prueba Phillips - Perron (PP)
pp.test(DesemSerie, alternative="stationary")
