![logo](https://github.com/kevinPerezGarcia/kevinPerezGarcia/blob/main/logo.png)

<p>
AÑo 2024 <br>
UNIVERSIDAD NACIONAL DE INGENIERÍA <br>
FACULTAD DE INGENIERÍA ECONÓMICA, ESTADÍSTICA Y CIENCIAS SOCIALES <br>
MAESTRÍA EN DATA SCIENCE <br>
CICLO 2 <br>
FORECASTING
</p>

<h1>TRABAJO FINAL</h1>

# 👥 Alumno

[@Kevin Perez Garcia](https://www.linkedin.com/in/kevinperezgarcia)

# 🤝 Contribución

¡Las observaciones, las recomendaciones y las contribuciones son bienvenidos!

# 📞 Contacto

Para más consultas o colaboraciones, comuníquese a `econ.perez.garcia.k@gmail.com`.

# 📌 Tabla de contenido
- [👥 Alumno](#-alumno)
- [🤝 Contribución](#-contribución)
- [📞 Contacto](#-contacto)
- [📌 Tabla de contenido](#-tabla-de-contenido)
- [Partes del trabajo final](#partes-del-trabajo-final)
  - [Parte 1. Referida a métodos de suavización](#parte-1-referida-a-métodos-de-suavización)
  - [Parte 2. Referida al método Box Jenkins](#parte-2-referida-al-método-box-jenkins)
- [Información sobre el trabajo final](#información-sobre-el-trabajo-final)

# Partes del trabajo final

El trabajo final está conformado por 2 partes.

## Parte 1. Referida a métodos de suavización

Para cada uno de los siguientes métodos de suavización, calcular el RMSE. Luego, realizar las comparaciones e interpretaciones de estos.
  
Métodos de suavización a desarrollar:
  1. Método de suavización de medias móviles para $q=3$, $q=4$ y $q=5$.
  2. Método de suavización exponencial.

## Parte 2. Referida al método Box Jenkins

Para cada uno de los siguientes modelos, calcular el RMSE. Luego, realizar las comparaciones e interpretaciones de estos.

Modelos a desarrollar:
  1. Uno a partir del auto ARIMA y 
  2. otro a partir del correlograma.

*Nota.* Tenga en cuenta la metodología de Box-Jenkins descrita a continuación:
  
  1. Especificación: Analice (análisis situacional y estadístico) la gráfica de la serie e identifique tendencias y volatilidades.
  2. identificación: Analice la estacionariedad en varianza y en media; use test de raíz unitaria; Transformación de Box-Cox; use las FAC y FCP para identificar posibles modelos.
  3. Estimación: Estime los parámetros del modelo cno métodos estadístciso; analice la significancia de los parámetros estimados.
  4. Verificación: Analice los residuales del modelo (aleatoriedad y normalidad); valide los supuestos del modelo.<br>
   *Nota*. Las etapas de identificación, estimación y verificación es cíclica.
  5. Pronósticos

# Información sobre el trabajo final

* Fecha de entrega:
  - 22 de marzo, la parte 1.
  - 05 de abril, la parte 2.
* Softwares a usar: Puede usar uno de los dos siguientes lenguajes de programación.
  - Lenguaje de programación R. El código está facilitado por el docente durante el desarrollo del curso.
    - [pd2.R](../sesion1/notebooks/pd2-componentes_suavizado_serieTemporal/code.R) de la sesión 1 para la Parte 1 del trabajo final.
    - [pd4.R](../sesion2/notebooks/pd4.R) para la Parte 2 del trabajo final.
  - Lenguaje de programación Python. El código lo debe desarrollar el alumno.<br>
  *Nota.* Este trabajo se desarrolló con el lenguaje de programación Python.
* Entregables:
  * Reportes en Quarto si se usó R.
  * Reportes en Notebooks si se usó Python.
* ¿Individual o grupal? Individual
* Data: puede conseguir una serie temporal de su preferencia o seleccionar otra de la base de datos [seriesMacroeconomicas.xlsx](https://www.kaggle.com/datasets/kevinprezgarca/peru-balanza-comercial-series-macroeconomicas/data) disponible en Kaggle .<br>
  *Nota.*
  - Las series temporales elegidas entre alumnos deben ser distintas.
  - La serie temporal escogida será utilizada para todo este trabajo.
  - Serie temporal escogida: por definir.