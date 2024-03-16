# Carga de librerías necesarias
library(foreign)  # Para leer datos externos
library(stats)    # Funciones estadísticas básicas
library(graphics) # Funciones gráficas básicas
library(forecast) # Para realizar pronósticos
library(dplyr)    # Para manipulación de datos
library(smooth)   # Para suavizado de series temporales
library(TTR)      # Para cálculo de indicadores técnicos

# Lectura de datos desde un archivo CSV
datos <- read.csv("./data/raw/DATOS_SERIES_FINALES.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()

# Muestra la estructura de los datos
str(datos)

# Selecciona las columnas de Exportaciones e Importaciones
Exportaciones <- select(datos, XNT)
Importaciones <- select(datos, IMP)

# Convierte las series de tiempo en objetos 'ts' con frecuencia mensual
Serie_Exportaciones <- ts(Exportaciones, start = c(1994, 1), end = c(2021, 8), frequency = 12)
Serie_Importaciones <- ts(Importaciones, start = c(1994, 1), end = c(2021, 8), frequency = 12)

# Muestra las series de tiempo
print(Serie_Exportaciones)
print(Serie_Importaciones)

# Gráficos de Exportaciones e Importaciones
par(mfrow = c(2, 1)) # Organiza los gráficos en 2 filas y 1 columna

# Gráfico de Exportaciones
plot(Serie_Exportaciones)
boxplot(Serie_Exportaciones ~ cycle(Serie_Exportaciones))

# Gráfico de Importaciones
plot(Serie_Importaciones)
boxplot(Serie_Importaciones ~ cycle(Serie_Importaciones))

# Restaurar la configuración original de los gráficos
par(mfrow = c(1, 1))

# Primero se estabiliza la variabilidad (varianza)
plot(log(Serie_Exportaciones))
x <- log(Serie_Exportaciones)

plot(log(Serie_Importaciones))
x <- log(Serie_Importaciones)

# Tomar la primera diferencia para estabilizarlo
dif1.x <- diff(x)
plot(dif1.x)

dif1.y <- diff(y)
plot(dif1.y)

par(mfrow = c(3, 1))
plot(Serie_Exportaciones)
plot(log(Serie_Exportaciones))
plot(diff(log(Serie_Exportaciones)))
par(mfrow = c(1, 1))

# Funciones de autocorrelación
# Para determinar si hay tendencia, trabajar sobre las series temporales originales

par(mfrow = c(2, 1))
acf(Serie_Exportaciones, lag.max=34)
pacf(Serie_Exportaciones, lag.max=34)
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
acf(diff(log(Serie_Importaciones)), lag.max=34)
pacf(diff(log(Serie_Importaciones)), lag.max=34)
par(mfrow = c(1,1))

# para determinar los valores de p y q
par(mfrow = c(2, 1))
acf(diff(log(Serie_Exportaciones)), lag.max=34)
pacf(diff(log(Serie_Exportaciones)), lag.max=34)
par(mfrow = c(1,1))

par(mfrow = c(2, 1))
acf(diff(log(Serie_Importaciones)), lag.max=34)
pacf(diff(log(Serie_Importaciones)), lag.max=34)
par(mfrow = c(1,1))

# Test de raíces unitarias
library(tseries)

adf.test(Serie_Exportaciones,
         alternative="stationary",
         k=0)

adf.test(Serie_Importaciones,
         alternative="stationary",
         k=0)

# Prueba de Phillips-Perron (PP)
pp.test(Serie_Exportaciones, alternative="stationary")
pp.test(Serie_Importaciones, alternative="stationary")

