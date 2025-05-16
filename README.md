# Indicadores-censo-escolar
Funciones para analizar diferentes indicadores del censo escolar

## Descargar bases

La función "descargar_bases" permite descargar las bases del Censo Escolar para los años 2023 y/o 2024 de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```

# Llamar las bases del año 2023
descargar_bases(c("2023"))

#Llamar las bases del año 2023 y 2024 simultáneamente
descargar_bases(c("2023","2024"))

```
