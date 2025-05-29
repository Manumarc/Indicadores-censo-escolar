# Indicadores del censo escolar

Funciones para analizar diferentes indicadores del censo escolar.

## Llamar a las funciones 

Para llamar a las funciones desarrolladas se debe usar el siguiente código:

```
devtools::source_url("https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/refs/heads/main/funcionesce.R")
```

## Descargar bases

La función "descargar_bases" permite descargar las bases del Censo Escolar para los años 2022, 2023 y/o 2024 de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```
# Llamar las bases del año 2023 #
#-------------------------------#

descargar_bases(c("2023"))

# Llamar las bases del año 2023 y 2024 simultáneamente #
#------------------------------------------------------#

descargar_bases(c("2023","2024"))

# Llamar las bases del año 2022, 2023 y 2024 simultáneamente #
#------------------------------------------------------------#

descargar_bases(c("2022","2023","2024"))

```

## Cálculo de indicadores

La función "cal_indicador" permite calcular diferentes indicadores para diferentes años del censo escolar de acuerdo con los años descargados. A continuación se detallan los indicadores considerados dentro de la función.

### Situación de saneamiento legal

Este indicador da cuenta de la condición de saneamiento físico legal que presentan los locales de la escuela. La pregunta que considera es:

  - *El terreno, ¿cuenta con saneamiento Físico Legal?*

Dado que la variable está a nivel de local escolar y que dentro de cada local se pueden encontrar varios terrenos, el indicador se construyó con tres categorías:

- **Saneado**: si dentro del local escolar todos los terrenos cuentan con saneamiento físico legal.
- **Saneado parcialmente**: si dentro del local escolar uno o varios de los terrenos no cuentan con saneamiento físico legal.
- **No saneado**: si dentro del local escolar ninguno de los terrenos cuentan con saneamiento físico legal.

A continuación se muestra un ejemplo para el uso de dicho indicador. 

```
# Calcular el indicador de situación de saneamiento del local educativo del año 2023 #
#------------------------------------------------------------------------------------#

cal_indicador("Saneamiento","2023")

```

### Acceso a servicios básicos (agua, desagüe, luz)

Este indicador da cuenta si el servicio de luz, agua y desagüe que tiene la escuela presenta conexión con la red pública. Se ha construido con las siguientes preguntas del censo escolar:

- *Procedencia de abastecimiento de agua*
- *Tipo de conexión de desagüe*
- *Procedencia de abastecimiento de energía eléctrica*

Para todos estos indicadores se realiza una recodificación de las diferentes opciones de respuesta de cada uno y se construye un indicador dicotómico de "Conexión a red pública" y "Sin conexión a red pública". A continuación se muestra un ejemplo para el uso de dicho indicador. 

```
# Calcular el indicador de abastecimiento de agua conectado a la red pública del local educativo del año 2024 #
#-------------------------------------------------------------------------------------------------------------#

cal_indicador("Agua","2024")

# Calcular el indicador de tipo de desagüe conectado a la red pública del local educativo del año 2023 #
#------------------------------------------------------------------------------------------------------#

cal_indicador("Desagüe","2023")

# Calcular el indicador fuente de luz conectada a la red pública del local educativo del año 2022 #
#-------------------------------------------------------------------------------------------------#

cal_indicador("Luz","2022")

```

### Conexión a servicio de internet

Este indicador informa si la escuela cuenta con servicio de internet. Se ha construido con la siguiente pregunta del censo escolar:

- *El Local Educativo ¿Cuenta con servicio de internet?*

A continuación se muestra un ejemplo para el uso de dicho indicador. 

```
# Calcular el indicador de conexión a internet dentro del local educativo del año 2024 #
#--------------------------------------------------------------------------------------#

cal_indicador("Internet","2024")

```

### Condición de la infraestructura de las aulas

Este indicador da cuenta de la condición en la que se encuentran las paredes, pisos, techos, ventanas y puertas de las aulas en su conjunto. Es decir, el indicador señala el porcentaje de aulas que cuentan con el elemento de infraestructura en buen estado. Se ha construido con las siguientes preguntas del censo escolar:

- *¿El ambiente se encuentra en uso?*
- *Puertas: cantidad*
- *Puertas: estado de conservación*
- *Ventanas: cantidad*
- *Marco de las ventanas: estado de conservación*
- *Vidrios de las ventanas: estado de conservación*
- *Paredes: estado de conservación*
- *Techo: estado de conservación*
- *Piso: estado de conservación*

El estado de conservación de los elementos de infraestructura que se han considerado toman los valores siguientes dependiendo el año que se esté analizando:

| Código 2022 | Descripción 2022              | Código 2023 en adelante | Descripción 2023 en adelante           |
|-------------|-------------------------------|--------------|-----------------------------|
| 01          | Buen estado                   | 01           | Buen estado                 |
| 02          | Regular estado                | 02           | Regular estado              |
| 03          | Mal estado                    | 03           | Mal estado                  |
| 04          | No tiene, pero lo requiere    | 04           | No aplica                   |
| 05          | No tiene y no lo requiere     |              |                             |

Dado esto, sólo se considera el código "01"
