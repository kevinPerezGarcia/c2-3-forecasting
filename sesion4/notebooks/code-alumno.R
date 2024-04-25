setwd("P:/maestriaDS/cursos/c2-3-forecasting/sesion4/pd1")

library(forecast)

datos <- read.csv("data/raw/DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
XNT <- dplyr::select(datos,XNT)
Serie_Exportaciones <- ts(XNT, start = c(1994,1), end = c(2021,8), frequency = 12)

########################## ESPECIFICACION ##########################

#Explorando datos
par(mfrow = c(2,1))
plot(Serie_Exportaciones)
boxplot(Serie_Exportaciones ~ cycle(Serie_Exportaciones))
par(mfrow = c(1,1))

#Explorando datos
par(mfrow = c(3,1))
plot(Serie_Exportaciones,xlab = "Tiempo",ylab = "Exportaciones")
plot(Serie_Exportaciones_1,xlab = "Tiempo",ylab = "Exportaciones")
plot(Serie_Exportaciones_2,xlab = "Tiempo",ylab = "Exportaciones")
par(mfrow = c(1,1))

########################## IDENTIFICACION ##########################

# Pruebas de raíces unitarias
# Permite evaluar la estacionariedad en media

# 1) Prueba de Dicker-Fuller Aumentado (ADF)
library(tseries)

adf.test(Serie_Exportaciones,
         alternative="stationary"
         )
# Discusión: p-value calculado > 0.05 => H0 (no estacionariedad) aceptada => tomar primeras diferencias

# 2) Funciones de autocorrelación
# Permite evaluar la estacionariedad en media, vía fac, y en varianza, vía facp.
# Si se salen de las bandas de confianza no hay estacionariedad
par(mfrow = c(2,1))
acf(Serie_Exportaciones, lag.max=34, main = "Función de autocorrelación")
pacf(Serie_Exportaciones, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))


# Transformación Box-Cox
# Permite corregir la no estacionariedad (presencia de tendencia) en varianza
library(forecast)
lamb.x <- BoxCox.lambda(Serie_Exportaciones)
lamb.x # De acuerdo a este valor, realizamos la transformación Box-Cox

# Realizando la transformación
tran_box <- function(xt,lambda){
  if (lamb.x == 0) {
    xt_box <- log(xt)
  } else {
    xt_box <- (xt^lamb.x - 1)/lamb.x
  }
}

Serie_Exportaciones_boxCox <- tran_box(Serie_Exportaciones, lamb.x)
plot(Serie_Exportaciones_boxCox)

# Si no se suaviza la serie en varianza, entonces puede estar afectado por la varianza del error.
# Teniendo el modelo, evaluamos si es un modelo heterocedástico.

# Tomando la primera diferencia para estabilizar la media
D_Serie_Exportaciones_boxCox = diff(Serie_Exportaciones_boxCox, differences = 1)
plot(D_Serie_Exportaciones_boxCox)

adf.test(
  D_Serie_Exportaciones_boxCox,
  alternative="stationary" # Concluímos que la serie es estacionaria en media
  )

# Comparación visual de la serie original y sus transformaciones
par(mfrow = c(3,1))
plot(Serie_Exportaciones)
plot(Serie_Exportaciones_boxCox)
plot(D_Serie_Exportaciones_boxCox)
par(mfrow = c(1,1))

# Determinación de los valores "p" y "q" del modelo
# vía funciones de autocorrelación
par(mfrow = c(2,1))
acf(
  Serie_Exportaciones,
  lag.max=50, # Permite evaluar visualmente estacionalidad
  main = "Función de autocorrelación"
  )
pacf(Serie_Exportaciones, lag.max=50, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

############################ ESTIMACION ############################
# Especificación del modelo vía probando varios valores de "p" y "q"
# Comparamos p-value significativos y RMSE
library(lmtest)
modelo_1 = arima(Serie_Exportaciones_boxCox, order = c(1,1,0))
summary(modelo_1)
coeftest(modelo_1)

modelo_1a = arima(D_Serie_Exportaciones_boxCox, order = c(1,0,0))
summary(modelo_1a)
coeftest(modelo_1a)

modelo_2 = arima(Serie_Exportaciones_boxCox, order = c(2,1,0))
summary(modelo_2)
coeftest(modelo_2)

modelo_3 = arima(Serie_Exportaciones_boxCox, order = c(3,1,0))
summary(modelo_3)
coeftest(modelo_3)

modelo_4 = arima(Serie_Exportaciones_boxCox, order = c(4,1,0))
summary(modelo_4)
coeftest(modelo_4) # Mejor modelo resultante, concuerda con el gráfico de las funciones de autocorrelación

# El modelo sería: Z_t = -0.1024*Z_(t-1) -0.1166*Z_(t-4) + error_t

# Otras pruebas
modelo_5 = arima(Serie_Exportaciones_boxCox, order = c(1,1,1))
summary(modelo_5)
coeftest(modelo_5)

# Inspección visual para determinar lo valores de "P" y "Q" para la componente estacional del modelo
par(mfrow = c(2,1))
acf(Serie_Exportaciones, lag.max=34, main = "Función de autocorrelación")
pacf(Serie_Exportaciones, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

# Tomando en cuenta la componente estacional
modelo_6 = arima(Serie_Exportaciones_boxCox, 
                  order = c(4,1,0),
                  seasonal = list(order = c(1,0,1),
                  period = 12,
                  method="ML")
                 )
summary(modelo_6)
coeftest(modelo_6)

modelo_7 = arima(Serie_Exportaciones_boxCox, 
                 order = c(4,1,0),
                 seasonal = list(order = c(1,0,2), period = 12,method="ML"))
summary(modelo_7)
coeftest(modelo_7)

modelo_8 = arima(Serie_Exportaciones_boxCox, 
                 order = c(4,1,0),
                 seasonal = list(order = c(1,0,3), period = 12,method="ML"))
summary(modelo_8)
coeftest(modelo_8)

modelo_9 = arima(Serie_Exportaciones_boxCox, 
                 order = c(4,1,0),
                 seasonal = list(order = c(2,0,1), period = 12,method="ML"))
summary(modelo_9)
coeftest(modelo_9)

modelo_10 = arima(Serie_Exportaciones_boxCox, 
                 order = c(4,1,0),
                 seasonal = list(order = c(2,0,2), period = 12,method="ML"))
summary(modelo_10)
coeftest(modelo_10)

modelo_11 = arima(Serie_Exportaciones_boxCox, 
                 order = c(4,1,0),
                 seasonal = list(order = c(2,0,3), period = 12,method="ML"))
summary(modelo_11)
coeftest(modelo_11)

#Mejor modelo
modelo_6 = arima(Serie_Exportaciones_boxCox, 
                  order = c(4,1,0),
                  seasonal = list(order = c(1,0,1),
                  period = 12,
                  method="ML")
                  )
summary(modelo_6)
coeftest(modelo_6)

# El modelo sería: SARIMA(4,1,0)(1,0,1)

############################ VALIDACION ############################
# Análisis de normalidad de residuales

# MODELO 1

# Calculando residuos
residuos_modelo_6 = modelo_6$residuals

# Graficando
par(mfrow = c(1,3))
hist(residuos_modelo_6)
boxplot(residuos_modelo_6)
plot(residuos_modelo_6)
par(mfrow = c(1,1))

# Excluyendo valores faltantes de los residuos
residuos_modelo_6 = residuos_modelo_6[!is.na(residuos_modelo_6)]

# Prueba estadística de normalidad (H0: no normalidad) de Jarque-Bera
jarque.bera.test(residuos_modelo_6)

# Ante no normalidad de los residuos, verificar presencia de datos atípicos.

# Determinanción de modelos heterocedásticos vía la variabilidad de los residuales al cuadrado
par(mfrow = c(2,1))
acf(residuos_modelo_6^2, lag.max=34, main = "Función de autocorrelación")
pacf(residuos_modelo_6^2, lag.max=34, main = "Función de autocorrelación parcial")
par(mfrow = c(1,1))

# Si se respetan las bandas de confianza, no hay comportamiento heterocedástico.
# Sino en estos modelos heterocedásticos, tendríamos que modelar los residuales.

# MODELO AUTO-ARIMA
# Debe ejecutarse con el modelo transformado (el que estabiliza la varianza)
modelo_autoarima = auto.arima(
  Serie_Exportaciones
)
summary(modelo_autoarima)
coeftest(modelo_autoarima)