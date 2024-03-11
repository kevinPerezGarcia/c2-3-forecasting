# Manipulacón de datos y preparación de series tiemporales

## Carga de librerías
library(dplyr)
library(forecast)
library(tseries)

## Lectura de datos
datos <- read.csv("./data/raw/DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()

## Ver la estructura de la data
dplyr::glimpse(datos)

## Selecci´no de variables relevanets para el análisis de series temporales
XNT <- dplyr::select(datos, XNT)

## Trasnformación de datos en una serie temporal con la función `ts()`
Serie_XNT <- ts(XNT, start = c(1994,1), end = c(2021,8), frequency = 12)
print(Serie_XNT)

# Exploración inicial de los datos 

## Visualización de la serie temporal
par(mfrow = c(2,1))
plot(Serie_XNT)
boxplot(Serie_XNT ~ cycle(Serie_XNT))
par(mfrow = c(1,1))

# Identificación de la estacionariedad y autocorrelación

## Prueba de raíces unitarias con el Test de Dickey-Fuller Aumentado (ADF)
adf.test(Serie_XNT, alternative = "stationary")

## Análisis de las funciones de autocorrelación (ACF) y autocorrelación parcial (PACF)
par(mfrow = c(2,1))
acf(Serie_XNT, lag.max = 34)
pacf(Serie_XNT, lag.max = 34)
par(mfrow = c(1,1))

## Transformación de Box Cox para estabilizar la varianza de la serie

### 1ro) Identificar el valor óptimo de lambda
lamb.x <- BoxCox.lambda(Serie_XNT)
lamb.x

### 2do) Aplicar la transformación respectiva para dicho lambda óptimo

#### Definir la transformación
tran_box <- function(xt, lambda){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1) / lamb.x
  }
}

#### Aplicar la transformación
Serie_XNT_boxCox <- tran_box(Serie_XNT, lamb.x)

##### Gráfica
plot(Serie_XNT_boxCox)

##### Prueba de raíces unitarias con el Test de Dickey-Fuller Aumentado (ADF)
adf.test(Serie_XNT_boxCox, alternative = "stationary")

###### Tomando la primera diferencia
D_Serie_XNT_boxCox <- diff(Serie_XNT_boxCox, differences = 1)

####### Gráfica
plot(D_Serie_XNT_boxCox)

####### Prueba de raíces unitarias con el Test de Dickey-Fuller Aumentado (ADF)
adf.test(D_Serie_XNT_boxCox, alternative = "stationary")

# Explorando datos

## De las tres series
par(mfrow = c(3,1))
plot(Serie_XNT)
plot(Serie_XNT_boxCox)
plot(D_Serie_XNT_boxCox)
par(mfrow = c(1,1))

## Funciones de autocorrelación
par(mfrow = c(2,1))
acf(Serie_XNT, lag.max = 34, main = "Función de autocorrelación")
pacf(Serie_XNT, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(Serie_XNT_boxCox, lag.max = 34, main = "Función de autocorrelación")
pacf(Serie_XNT_boxCox, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(D_Serie_XNT_boxCox, lag.max = 34, main = "Función de autocorrelación")
pacf(D_Serie_XNT_boxCox, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

# Estimación de modelos ARIMA
library(lmtest)

## Especificación de modelos ARIMA con diferentes órdenes
modelo_1 <- arima(Serie_XNT_boxCox, order = c(1,1,1))
summary(modelo_1)
coeftest(modelo_1)

modelo_2 <- arima(Serie_XNT_boxCox, order = c(1,1,2))
summary(modelo_2)
coeftest(modelo_2)

modelo_3 <- arima(Serie_XNT_boxCox, order = c(2,1,1))
summary(modelo_3)
coeftest(modelo_3)

modelo_4 <- arima(Serie_XNT_boxCox, order = c(2,1,2))
summary(modelo_4)
coeftest(modelo_4)

## Selección de modelos considerando la componente estacional
modelo_5 <- arima(Serie_XNT_boxCox, 
                  order = c(2,1,1),
                  seasonal = list(order = c(1,1,0), period = 12, method = "ML"))
summary(modelo_5)
coeftest(modelo_5)

modelo_7 <- arima(Serie_XNT_boxCox, 
                  order = c(0,1,0),
                  seasonal = list(order = c(3,1,0), period = 12, method = "ML"))
summary(modelo_7)
coeftest(modelo_7)

## Uso de la función `auto.arima()` para identificar automáticamente el mejor modelo
modelo_8 <- auto.arima(Serie_XNT_boxCox)
summary(modelo_8)
coeftest(modelo_8)

## Mejor modelo
modelo_9 <- arima(Serie_XNT_boxCox, 
                  order = c(1,1,0),
                  seasonal = list(order = c(2,0,0),
                  period = 12,
                  method = "ML")
                  )
summary(modelo_9)
coeftest(modelo_9)

# Validación de modelos

## MODELO 5

### Prueba de bondad de ajuste (por ejemplo, Prueba de Jarque-Bera) para evaluar la normalidad de los residuos
residuos_modelo5 <- modelo_5$residuals
hist(residuos_modelo5)
boxplot(residuos_modelo5)
plot(residuos_modelo5)
residuos_modelo5 <- residuos_modelo5[!is.na(residuos_modelo5)]
jarque.bera.test(residuos_modelo5)

# Visualización de las funciones de autocorrelación de los residuos
par(mfrow = c(2,1))
acf(residuos_modelo5, lag.max = 34, main = "Función de autocorrelación")
pacf(residuos_modelo5, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

## MODELO 7

### Prueba de bondad de ajuste (por ejemplo, Prueba de Jarque-Bera) para evaluar la normalidad de los residuos
residuos_modelo7 <- modelo_7$residuals
hist(residuos_modelo7)
boxplot(residuos_modelo7)
plot(residuos_modelo7)
residuos_modelo7 <- residuos_modelo7[!is.na(residuos_modelo7)]
jarque.bera.test(residuos_modelo7)

### Visualización de las funciones de autocorrelación de los residuos
par(mfrow = c(2,1))
acf(residuos_modelo7, lag.max = 34, main = "Función de autocorrelación")
pacf(residuos_modelo7, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

## MODELO 3

### Prueba de bondad de ajuste (por ejemplo, Prueba de Jarque-Bera) para evaluar la normalidad de los residuos
residuos_modelo3 <- modelo_3$residuals
hist(residuos_modelo3)
boxplot(residuos_modelo3)
plot(residuos_modelo3)
residuos_modelo3 <- residuos_modelo3[!is.na(residuos_modelo3)]
jarque.bera.test(residuos_modelo3)

# Visualización de las funciones de autocorrelación de los residuos
par(mfrow = c(2,1))
acf(residuos_modelo3, lag.max = 34, main = "Función de autocorrelación")
pacf(residuos_modelo3, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

## MODELO 9

### Prueba de bondad de ajuste (por ejemplo, Prueba de Jarque-Bera) para evaluar la normalidad de los residuos
residuos_modelo9 <- modelo_9$residuals
hist(residuos_modelo9)
boxplot(residuos_modelo9)
plot(residuos_modelo9)
residuos_modelo9 <- residuos_modelo9[!is.na(residuos_modelo9)]
jarque.bera.test(residuos_modelo9)

### Visualización de las funciones de autocorrelación de los residuos
par(mfrow = c(2,1))
acf(residuos_modelo9, lag.max = 34, main = "Función de autocorrelación")
pacf(residuos_modelo9, lag.max = 34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))
