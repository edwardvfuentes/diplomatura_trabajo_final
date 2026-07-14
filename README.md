# Trabajo final de la diplomatura de Economía y Política China: Repositorio

## Introducción

Este repositorio pretende guardar de manera abierta todo el código relacionado con la estimación de modelos econométricos para el trabajo final de la mencionada diplomatura. La estimación se hará mediante R.

## Metodología

Se pretende estimar una serie de modelos econométricos para medir la posible influencia de la reforma del Hukou de 2014 sobre el consumo y otras variables de las familias chinas.

## Estructura

El proyecto cuenta con una serie de carpetas y scripts, que detallamos a continuación:

- data_source: Carpeta el conjunto de datos descargado y su documentación relacionad, en este caso del CFPS
- input: Carpeta con los scripts de R necesarios. Cada sript tiene una finalidad:

  * load_source_data.R: Carga los datos en bruto en el entorno de R. Nótese que en el repositorio remoto esta carpeta no se incluye por normas de uso de los datos de CFPS de parte de Peking University (para más información: https://cfpsdata.pku.edu.cn/#/home).
  * clean_datasets.R: A partir de los datos en bruto, realiza una serie de limpiezas para tener los dos dataframes completos del proyecto, que son __famecon_family_df__ y __famecon_grouped_df__. El primero contiene los datos consolidados a nivel familia de todas las muestras, desde 2010 hasta 2022, mientras que el segundo se compone de promedios por año y provincia, pensado para las estimaciones de panel.
  * udf.R: Fichero con funciones de usuario definidas que han sido utilizadas en el proyecto.
  * household_type.R: Script que se encarga de clasificar a las familias según rural, urbano o migrante, mediante la metodología que detallamos en el trabajo.
  * cfps_eda.R: Script exploratorio de datos. Aquí se generan las tablas y las gráficas del trabajo.
  * estimaciones.R: Donde se estiman los modelos econométricos propuestos en el trabajo.

## Instalación

Para ejecutar los scripts, se necesitan los paquetes indicados en el fichero _requirements.txt_. Se pueden instalar mediante el comando install.packages(), en donde el argumento puede ser un vector de caracteres con el nombre de cada paquete.
  
## Cómo usar

Si se desea obtener los conjuntos de datos ya listos para su exploración o cálculo de modelos econométricos, basta con ejecutar source("./input/clean_datasets") desde el directorio de trabajo.

## Datos

Los datos provienen del *China Family Panel Studies* (CFPS), y no se pueden publicar sin permiso de la institución, por lo que este repositorio no subirá los datos de manera pública.