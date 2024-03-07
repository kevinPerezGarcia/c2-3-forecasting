#### MODELADO DATA COVID 19

# Cargando librerías necesarias
library(foreign)
library(stats)
library(graphics)
library(forecast)
library(dplyr)
library(smooth)
library(TTR)

# Lectura de datos relacionados con COVID-19
datos <- read.csv("./data/raw/Data_Covid.csv", header = TRUE, sep = ";" ,dec = ".") |> as.data.frame()
glimpse(datos) # Ver la estructura de la data

# Conteo de casos de COVID-19 por fecha
CantCovid19 <- table(datos$FECHA_RESULTADO) |> as.data.frame()
colnames(CantCovid19) <- c("Periodo","CantCovid19")

# Selección de la columna con los datos de casos de COVID-19
Cant_Covd_19 <- dplyr::select(CantCovid19,CantCovid19)

# Creación de una serie de tiempo para los casos de COVID-19
Serie_CantCovid19 <- ts(Cant_Covd_19, start = c(2020,1,6), end = c(2021,11,11), frequency = 360)
print(Serie_CantCovid19)

# Visualización de la serie de tiempo de casos de COVID-19
plot(Serie_CantCovid19)

# Instalación y carga del paquete "TTR" para realizar cálculos técnicos de análisis financiero
install.packages("TTR")
library(TTR)

# Suavización por medias móviles de casos de COVID-19 y generación de pronósticos
Mediamovil_Covid <- sma(Serie_CantCovid19, order = 5)
pronostico_covid <- forecast::forecast(Mediamovil_Covid, h=10)
plot(forecast(pronostico_covid))

# Suavización exponencial simple de casos de COVID-19 y generación de pronósticos
D_Serie_CantCovid19 <- diff(Serie_CantCovid19)
plot(D_Serie_Exportaciones)

Fit_Ses_Covid19 <- forecast::ses(D_Serie_CantCovid19, h = 1, initial = "simple")
Fit_Ses_Covid19 
summary(Fit_Ses_Covid19)
Pronos_Covid <- forecast::forecast(Fit_Ses_Covid19,h=1)
plot(Pronos_Covid)

# Suavización exponencial doble de casos de COVID-19 y generación de pronósticos
FitHolt <- HoltWinters(Serie_CantCovid19, alpha = 0.1, beta = TRUE, gamma = FALSE)
summary(FitHolt)
Pronos_FitHolt <- forecast(FitHolt,h=1)
Pronos_FitHolt
