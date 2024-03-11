library(forecast)
library(tseries)

#Lectura de datos
datos <- read.csv("./data/raw/DATOS_XNT_PROD_TRAD_1.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
dplyr::glimpse(datos) # Ver la estructura de la data

#Seleccionamos variables monetarias de la serie
XNT_Cereales <- dplyr::select(datos,XNT05)

#Indicamos que sea una serie de tiempo
Serie_XNT_Cereales <- ts(XNT_Cereales, start = c(1999,1), end = c(2021,8), frequency = 12)
print(Serie_XNT_Cereales)


####################################################################
########################## ESPECIFICACI?N ##########################
####################################################################


#Explorando datos
par(mfrow = c(2,1))
plot(Serie_XNT_Cereales)
boxplot(Serie_XNT_Cereales ~ cycle(Serie_XNT_Cereales))
par(mfrow = c(1,1))




####################################################################
########################## IDENTIFICACI?N ##########################
####################################################################


#test de raices unitaria
#Prueba de Dicker - Fuller Aumentado (ADF)
library(tseries)

adf.test(Serie_XNT_Cereales, 
         alternative="stationary", 
         k=0)

#Prueba Phillips - Perron (PP)
pp.test(Serie_XNT_Cereales, alternative="stationary")

#Transfromaci?n de Box Cox
library(forecast)
lamb.x <- BoxCox.lambda(Serie_XNT_Cereales)
lamb.x

##realiza la transformacion
tran_box <- function(xt,lambda){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1)/lamb.x
  }
}

Serie_XNT_Cereales_boxCox <- tran_box(Serie_XNT_Cereales, lamb.x)

#Explorando datos
par(mfrow = c(2,1))
plot(Serie_XNT_Cereales)
plot(Serie_XNT_Cereales_boxCox)
par(mfrow = c(1,1))


#Funciones de autocorrelacion
par(mfrow = c(2,1))
acf(Serie_XNT_Cereales, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(Serie_XNT_Cereales, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(Serie_XNT_Cereales_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n")
pacf(Serie_XNT_Cereales_boxCox, lag.max=34, main = "Funci?n de autocorrelaci?n parcial")
par(mfrow = c(1,1))