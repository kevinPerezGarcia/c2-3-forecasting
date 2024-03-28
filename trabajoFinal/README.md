![logo](https://github.com/kevinPerezGarcia/kevinPerezGarcia/blob/main/logo.png)

<p>
AÑO 2024 <br>
UNIVERSIDAD NACIONAL DE INGENIERÍA <br>
FACULTAD DE INGENIERÍA ECONÓMICA, ESTADÍSTICA Y CIENCIAS SOCIALES <br>
MAESTRÍA EN DATA SCIENCE <br>
CICLO 2 <br>
FORECASTING
</p>

<h1>TRABAJO FINAL. METODOLOGÍA DE BOX-JENKINS</h1>

# 👥 Autor

[@Kevin Perez Garcia](https://www.linkedin.com/in/kevinperezgarcia)

🤝 ¡Las observaciones, las recomendaciones y las contribuciones son bienvenidas!

📞 Para más consultas o colaboraciones, comuníquese a `econ.perez.garcia.k@gmail.com`.

# Información del trabajo

* Fecha de entrega: 05 de abril.
* Softwares a usar: Puede usar uno de los dos siguientes lenguajes de programación.
  - Lenguaje de programación R. Código facilitado por el docente ([pd4.R de la sesión 2](../sesion2/notebooks/pd4.R)).
  - Lenguaje de programación Python. El código lo debe desarrollar el alumno.<br>
  *Nota.* Este trabajo se desarrolló con el lenguaje de programación Python.
* Entregables:
  * Reportes en Quarto si se usó R.
  * Reportes en Notebooks si se usó Python.
* ¿Individual o grupal? Individual
* Data: La misma serie temporal que eligió para el trabajo inicial.
* Trabajo: Para cada uno de los siguientes modelos, calcular el RMSE. Luego, realizar las comparaciones e interpretaciones de estos. Modelos a desarrollar:
  1. Uno a partir del auto ARIMA y 
  2. otro a partir del correlograma.

*Nota.* Tenga en cuenta la metodología de Box-Jenkins descrita a continuación:
  
  1. Especificación: Analice (análisis situacional y estadístico) la gráfica de la serie e identifique tendencias y volatilidades.
  2. identificación: Analice la estacionariedad en varianza y en media; use test de raíz unitaria; Transformación de Box-Cox; use las FAC y FCP para identificar posibles modelos.
  3. Estimación: Estime los parámetros del modelo cno métodos estadístciso; analice la significancia de los parámetros estimados.
  4. Verificación: Analice los residuales del modelo (aleatoriedad y normalidad); valide los supuestos del modelo.<br>
   *Nota*. Las etapas de identificación, estimación y verificación es cíclica.
  5. Pronósticos