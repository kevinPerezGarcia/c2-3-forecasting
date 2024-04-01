# Carga de librerías necesarias
library(foreign)  # Para leer datos externos
library(stats)    # Funciones estadísticas básicas
library(graphics) # Funciones gráficas básicas
library(forecast) # Para realizar pronósticos
library(dplyr)    # Para manipulación de datos
library(smooth)   # Para suavizado de series temporales
library(TTR)      # Para cálculo de indicadores técnicos
library(ggplot2)  # Para graficar

# Lectura de datos desde un archivo CSV
datos <- read.csv("./sesion1/pd2/data/raw/DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()

# Muestra la estructura de los datos
str(datos)

# Selecciona la columna de Exportaciones
Exportaciones <- select(datos, XNT)

# Convierte las series de tiempo en objetos 'ts' con frecuencia mensual
Serie_Exportaciones <- ts(Exportaciones, start = c(1994, 1), end = c(2021, 8), frequency = 12)

# Muestra las series de tiempo
print(Serie_Exportaciones)

# Gráficos de Exportaciones
par(mfrow = c(2, 1)) # Organiza los gráficos en 2 filas y 1 columna
plot(Serie_Exportaciones)
boxplot(Serie_Exportaciones ~ cycle(Serie_Exportaciones))
par(mfrow = c(1, 1))

# Gráfico estacional de Exportaciones utilizando ggplot2
ggseasonplot(Serie_Exportaciones,
             year.labels = TRUE,
             year.labels.left = TRUE) +
  ylab("Millones de soles") +
  xlab("Meses") +
  ggtitle("Exportaciones del Perú")

######################################################################
################### SUAVIZACION POR MEDIAS MOVILES ###################
######################################################################

# Cálculo del pronóstico utilizando media móvil con diferentes órdenes
plot(Serie_Exportaciones)
Mediamovil1 <- sma(Serie_Exportaciones, order = 3)

plot(Serie_Exportaciones)
Mediamovil2 <- sma(Serie_Exportaciones, order = 4)

plot(Serie_Exportaciones)
Mediamovil3 <- sma(Serie_Exportaciones, order = 5)

###############################################################
################### SUAVIZACION EXPONENCIAL ###################
###############################################################

##SIN TENDENCIAS

# Se quita la tendencia vía Diferenciación de la serie de tiempo de Exportaciones
D_Serie_Exportaciones <- diff(Serie_Exportaciones)
plot(D_Serie_Exportaciones)

# Ajuste utilizando el método Simple Exponential Smoothing (SES)
Fit_Ses_1 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.1)
Fit_Ses_1
summary(Fit_Ses_1)

Fit_Ses_2 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.5)
Fit_Ses_2
summary(Fit_Ses_2)

Fit_Ses_3 <- forecast::ses(D_Serie_Exportaciones, h = 1, initial = "simple", alpha = 0.9)
Fit_Ses_3
summary(Fit_Ses_3)

Fit_Ses_4 <- forecast::ses(Serie_Exportaciones, h = 1, initial = "simple")
Fit_Ses_4
summary(Fit_Ses_4)

Fit_Ses_5 <- forecast::ses(D_Serie_Exportaciones, h = 1, alpha = 0.6898)
Fit_Ses_5
summary(Fit_Ses_5)

# Resúmenes de los ajustes SES
summary(Fit_Ses_1)
summary(Fit_Ses_2)
summary(Fit_Ses_3)
summary(Fit_Ses_4)

#CON TENDENCIAS

# Ajuste y pronóstico utilizando el método Holt-Winters
FitHolt <- HoltWinters(Serie_Exportaciones, alpha = 0.6928873, beta = TRUE, gamma = TRUE)
summary(FitHolt)

# Ajuste y pronóstico utilizando el método Exponential Smoothing (ETS)
Fit_Ses_1_ets <- forecast::ets(Serie_Exportaciones)
Fit_Ses_1_ets
summary(Fit_Ses_1_ets)