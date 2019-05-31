# spring19-group-assignment-team1kenobi

# Package dds.base

<!-- badges: start -->
<!-- badges: end -->

El objetivo del Package es el análisis de los datasets HTTP y HTTPS de Rapid7 para determinar en que paises a nivel mundial se realizan conexiones seguras y en que paises no.

## Instalación

Se puede encontrar el package en el siguiente enlace: [Github](https://github.com/DDS-MCSM/spring19-group-assignment-team1kenobi) y podemos instalarlo con RStudio mediante el siguiente comando:

``` r
devtools::install_github("DDS-MCSM/spring19-group-assignment-team1kenobi")
```


## Obtención de datos

### Conexiones HTTP/HTTPS con información sobre su geolocalización 
La obtención y normalización de los datasets para realizar el estudio se realiza mediante las siguientes funciones:

#### createPath
Función que permite la creación de un path dentro de nuestro proyecto, donde almacenaremos el dataset descargado y el posterior dataframe analizado.
- @param savepath Define el nombre de la carpeta donde guardaremos el dataframe. 
- Si no indicamos ningun parámetro Default = data
- @return Devuelve el directorio donde guardaremos el dataframe.

```{r}
createPath <- function(savepath="data")
```


#### downloadScanIO
Función que descarga, descomprime y abre un dataset de  scansio y lo almacena en la carpeta especificada.
Si no se especifica carpeta, esta será default=data.
- @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
- @param savepath Introducir el nombre de la carpeta donde guardaremos el dataset
- @param filename Introducir el nombre del fichero que contendrá el dataset
- @return Devuelve el dataframe descargado con los datos en crudo

```{r}
downloadScanIO <- function(data.url, savepath="data", filename)
```


#### downloadMaxmind (savepath="data")
Función que descarga, descomprime y abre el dataset Maxmind en la carpeta especificada.
Si no se especifica carpeta, default=data.
- @param savepath Introducir el nombre de la carpeta donde guardaremos el dataset
- @return Devuelve el dataframe descargado con los datos en crudo.

```{r}
downloadMaxmind <- function(savepath="data")
```


#### generate.dfScan (nrows, scope=500, df.port)
Función que genera una muestra del dataset especificado.
- @param nrows Introducimos el número de filas que queremos que genere el dataframe.
- @param scope Introducimos el número de filas que tendrá la muestra. Si no se introduce el valor default=500.(Consideramos que una muestra debe tener como mínimo 500 resultados)
Importante remarcar que **el valor de nrows debe ser mayor o igual que scope.**
- @param df.port Introducimos el dataset de scans.io previamente descargado.

```{r}
generate.dfScan <- function(nrows, scope=500, df.port)
```


#### generate.dfMaxmind(df.maxmind)
Función que genera una muestra del dataframe Maxmind, cambiando el rango ip de cada una de sus filas por la IP max y IP min de dicho rango.
- @param df.maxmind Introducimos el dataframe de maxmind previamente descargado.
- @return Genera una muestra del dataframe.

```{r}
generate.dfMaxmind <- function(df.maxmind)
```


#### cleanAndJoin.df(df.scans, df.maxmind)
Función que une el dataframe a analizar con el dataframe Maxmind. Dicha unión permite mostrar la longitud y latitud que corresponde a cada una de las IP del dataframe a analizar.
- @param df.scans Introducimos la muestra del dataframe de scansio generado.
- @param df.maxmind Introducimos la muestra del dataframe maxmind generado.
- @return Devuelve el Dataframe resultante de hacer Join de ambas muestras.

```{r}
cleanAndJoin.df <- function(df.scans, df.maxmind)
```


#### addCountry <-(df)
Función que añade una columna al dataframe con el pais al que corresponde la longitud y latitud de origen de cada fila del dataframe.
- @param df Introducimos el dataframe generado mediante la unión de las dos muestras.
- @return Devuelve el df con la columna Country incluida.

```{r}
addCountry <- function(df)
```


#### printWorldMap(df)
Función que devuelve un mapa mundo coloreado, donde cuanto más oscuro es el color, implica que en ese pais hay más conexiones.
- @param df Introducimos el dataframe una vez incluida la columna Country.

```{r}
printWorldMap <- function(df)
```


#### printBarplot(df)
Función que devuelve una gráfica de los 10 paises con más conexiones.
- @param df Introducimos el dataframe una vez incluida la columna Country.
- @examples

```{r}
printBarplot <- function (df)
```


