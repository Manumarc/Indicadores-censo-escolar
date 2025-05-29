# Indicadores del censo escolar

Funciones para analizar diferentes indicadores del censo escolar.

## Llamar a las funciones 

Para llamar a las funciones desarrolladas se debe usar el siguiente código:

```
devtools::source_url("https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/refs/heads/main/funcionesce.R")
```

## Descargar bases

La función "descargar_bases" permite descargar las bases del Censo Escolar para los años 2023 y/o 2024 de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```
# Llamar las bases del año 2023
descargar_bases(c("2023"))

# Llamar las bases del año 2023 y 2024 simultáneamente
descargar_bases(c("2023","2024"))

```
## Cálculo de indicadores

La función "cal_indicador" permite calcular diferentes indicadores para diferentes años del censo escolar de acuerdo con los años descargados. A continuación se detallan los indicadores considerados dentro de la función.

### Situación de saneamiento legal

Este indicador da cuenta de la condición de saneamiento físico legal que presentan los locales de la escuela. La pregunta considera es "El terreno, ¿cuenta con saneamiento Físico Legal?" y considera dos tipos de respuestas: "Sí"; "No". Dado que la variable está a nivel de local escolar y que dentro de cada escuela se puede encontrar varios locales escolares, el indicador se construyó con tres categorías:

- Saneado: si todos los locales escolares se encuentran en terreno con saneamiento físico legal.
- Saneado parcialmente: si alguno de los locales escolares se encuentra en terreno que no cuenta con saneamiento físico legal.
- No saneado: si todos los locales escolares se encuentran en terreno que no cuenta con saneamiento físico legal.

```
# Calcular el indicador de situación de saneamiento del local educativo del año 2023
cal_indicador("Saneamiento","2023")

```
### Servicios básicos (agua, desagüe, luz)

```
# Calcular el indicador de abastecimiento de agua conectado a la red pública del local educativo del año 2024
cal_indicador("Agua","2024")

# Calcular el indicador de tipo de desagüe conectado a la red pública del local educativo del año 2024
cal_indicador("Desagüe","2024")

# Calcular el indicador fuente de luz conectada a la red pública del local educativo del año 2024
cal_indicador("Luz","2024")
```
