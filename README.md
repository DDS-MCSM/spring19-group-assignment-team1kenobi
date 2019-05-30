# spring19-group-assignment-team1kenobi

# Paquete dds.base

<!-- badges: start -->
<!-- badges: end -->

El objetivo del paquete es la obtencion de datasets relacionados con tipos de conexiones para posteriormente graficarlos 
y representar el numero de coneiones en un grafio de barras.

## Instalacion

POdemos instalar el paquete desde: [Github](https://github.com/DDS-MCSM/spring19-group-assignment-team1kenobi) with:

``` r
devtools::install_github("DDS-MCSM/spring19-group-assignment-team1kenobi")
```

## Obteniendo los datos 

### Conexiones HTTP/HTTPS con información sobre su geolocalización 
La obtencion de los datasets para en formato muestra para realizar el estudio se realiza a traves de la ejecucion consecutiva de las 
siguientes funciones:

#### createPath ()
Función que permite la creación de un path donde almacenaremos nuestro dataframe.
- @param savepath Define el nombre de la carpeta donde guardaremos el dataframe. 
- Si no indicamos ningun parámetro Default = data
- @return Devuelve el directorio donde guardaremos el dataframe.


```{r}
createPath <- function(savepath="data") {
  verbose <- TRUE

  set.seed(666)
  dir.data <- file.path(getwd(), savepath)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio Creado.")
    dir.create(dir.data)
  }
  return (dir.data)
}
```
#### downloadScanIO (data.url, savepath="data", filename)
Funcion que descarga, descomprime y abre un dataset de  scansio y lo almacena en la carpeta especificada.
Si no se especifica carpeta, esta será default=data.
- @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
- @param savepath Introducir el nombre de la carpeta donde guardaremos el dataset
- @param filename Introducir el nombre del fichero que contendrá el dataset
- @return Devuelve el dataframe descargado con los datos en crudo

```{r}
downloadScanIO <- function(data.url, savepath="data", filename) {
  verbose <- TRUE
  scansio.url <- data.url

  # Revisa si la carpeta fue creada por createPath y sino la crea
  dir.data <- file.path(getwd(), savepath)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio Ok.")
    dir.create(dir.data)
  }

  # scans.io - Obtenemos los datos del dataset
  scansio.source <- file.path(getwd(), savepath ,filename)
  scansio.source.csv <- paste(scansio.source, ".csv" , sep = "")
  scansio.file.gz <- paste(scansio.source.csv, ".gz", sep = "")
  download.file(url = scansio.url, destfile = scansio.file.gz)
  R.utils::gunzip(scansio.file.gz)
  df.port <- read.csv(scansio.source.csv, stringsAsFactors = FALSE)
  rm(scansio.file.gz)
  return (df.port)
}
```
#### downloadMaxmind (savepath="data")
Funcion que descarga, descomprime y abre el dataframe de Maxmind en la carpeta especificada.
Recordar que la carpeta debe crearse previamente con createPath(). Si no se especifica carpeta, default=data.
- @param savepath Introducir el nombre de la carpeta creada mediante la función createPath()
- @return Devuelve el dataframe descargado con los datos en crudo.
```{r}
downloadMaxmind <- function(savepath="data") {
  verbose <- TRUE
  maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"

  dir.data <- file.path(getwd(), savepath)
  if (!dir.exists(dir.data)) {
    if (verbose) print("Directorio Creado.")
    dir.create(dir.data)
  }
  # Maxmind - Obtener datos en crudo (city)
  maxmind.file <- file.path(getwd(), savepath, "maxmind.zip")
  download.file(url = maxmind.url, destfile = maxmind.file)
  zipfiles <- unzip(zipfile = maxmind.file, list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  unzip(zipfile = maxmind.file, exdir = dir.data, files = maxmind.source)
  maxmind.source <- file.path(getwd(), savepath, maxmind.source)
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  rm(maxmind.file, zipfiles)
  return (df.maxmind)
}
```
#### generate.dfScan (nrows, scope=500, df.port)
Funcion que genera un dataframe del dataset especificado. Extrae una muestra limitada por el parametro scope.
- @param nrows Introducimos el número de filas que queremos que genere el dataframe.
- @param scope Introducimos el scope, que será el número de filas que tendrá la muestra. Si no se introduce el valor default=500 el valor de nrows debe ser < que scope.
- @param df.port Introducimos el dataset de scans.io previamente descargado.

```{r}
generate.dfScan <- function(nrows, scope=500, df.port){
verbose <- TRUE
# Seleccionamos una muestra del dataset
if(nrows<scope)
  return(print("el valor de nrows debe ser mayor o igual que scope, vuelva a introducir los datos"))
df.port$saddr.num <- iptools::ip_to_numeric(df.port$saddr)
df.port$daddr.num <- iptools::ip_to_numeric(df.port$daddr)
muestra <- sample(1:nrows, scope)
df.scans <- df.port[muestra,]
rm(muestra)
return (df.scans)
}
```
#### generate.dfMaxmind(df.maxmind)
Funcion que genera una muestra del dataframe Maxmind.
- @param df.maxmind Introducimos el dataframe de maxmind previamente descargado.
- @return Genera una muestra del dataframe.
```{r}
generate.dfMaxmind <- function(df.maxmind){
    verbose <- TRUE
df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
df.maxmind$rowname <- as.integer(row.names(df.maxmind))
return (df.maxmind)
}
```
#### cleanAndJoin.df(df.scans, df.maxmind)
Funcion que limpia y junta los dataframes. Conexiones (df.scans) y geolocalizacion de las IP (df.maxmind)
- @param df.scans Introducimos la muestra del dataframe scans generado.
- @param df.maxmind Introducimos la muestra del dataframe maxmind generado.
- @return Devuelve el Dataframe resultante de hacer Join de ambas muestras.
```{r}
cleanAndJoin.df <- function(df.scans, df.maxmind){
    verbose <- TRUE
# Usamos multiples cpu's para geolocalizar IPs en rangos
if (verbose) print("[*] Foreach IP (source and destination) identify network range using parallel computing")
no_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(no_cores)
parallel::clusterExport(cl, "df.maxmind")
df.scans$sloc <- sapply(df.scans$saddr.num,
                           function(ip)
                             which((ip >= df.maxmind$min_numeric) &
                                   (ip <= df.maxmind$max_numeric)))
df.scans$dloc <- sapply(df.scans$daddr.num,
                        function(ip)
                          which((ip >= df.maxmind$min_numeric) &
                                (ip <= df.maxmind$max_numeric)))
parallel::stopCluster(cl)
rm(cl, no_cores)


# Join and tidy data frame (source address)
if (verbose) print("[*] Joining source IP's with geolocation data")
df <- dplyr::left_join(df.scans, df.maxmind, by = c("sloc" = "rowname"))
df <- dplyr::select(df, saddr, latitude, longitude, accuracy_radius)
names(df) <- c("IP_ORIGEN", "LATITUD_O", "LONGITUD_O",
               "PRECISION")

# Join and tidy data frame (destination address)
if (verbose) print("[*] Joining destination IP's with geolocation data")
suppressMessages(library(dplyr))
df.dst <- df.scans %>%
  left_join(df.maxmind, by = c("dloc" = "rowname")) %>%
  select(daddr, latitude, longitude)
names(df.dst) <- c("IP_DESTINO", "LATITUD_D", "LONGITUD_D")
df <- dplyr::bind_cols(df, df.dst)
rm(df.dst, df.scans)

return (df)
}
```
#### addCountry <-(df)
Función que añade una columna al dataframe con el pais al que corresponde la longitud y latitud de origen de cara fila del dataframe.
- @param df Introducimos el dataframe generado mediante las dos muestras.
- @return Devuelve el df con la columna Country incluida.

```{r}
addCountry <- function(df){

  ## The df that is used in this function must have columms names slongitude and slatitude for the GPS coordinates.
  df$COUNTRY<-map.where(database = "world",df$LONGITUD_O,df$LATITUD_O)
  return(df)
}
```
#### printWorldMap(df)
Función que devuelve un mapa mundo coloreado, donde cuanto más oscuro es el color, implica que en ese pais hay más conexiones.
- @param df Introducimos el dataframe una vez incluida la columna Country.

```{r}
printWorldMap <- function(df) {

  colorPalette <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
  df.count.sort  <-  dplyr::count(df, COUNTRY, sort=TRUE)
  df.withoutna  <- df.count.sort[!is.na(df.count.sort$COUNTRY),]
  rworld  = rworldmap::joinCountryData2Map(dF = df.withoutna,
                                           joinCode = "NAME",
                                           nameJoinColumn = "COUNTRY")

  rworldmap::mapCountryData(rworld,
                            nameColumnToPlot = "n",
                            catMethod = "categorical",
                            mapTitle = "Comparativa",
                            addLegend = F,
                            colourPalette = colorPalette)
}
```
#### printBarplot(df)
Función que devuelve una gráfica de los 10 paises con más conexiones.
- @param df Introducimos el dataframe una vez incluida la columna Country.
- @examples

```{r}
printBarplot <- function (df) {

  a  <-  dplyr::count(df, COUNTRY, sort = TRUE)
  a <- a[!is.na(a$COUNTRY),]
  a2<-dplyr::top_n(a, 10)
  par(mar=c(6,5,3,1))
  barplot(a2$n,
          main = "HTTPS",
          ylim = c(0,900),
          names.arg = a2$COUNTRY,
          col = "darkred",
          cex.names =.7, las=2)
}
```


