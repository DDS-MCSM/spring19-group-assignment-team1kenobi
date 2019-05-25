#' Función que permite la creación de un path donde almacenaremos nuestro dataframe
#'
#' @param savepath Define el nombre de la carpeta donde guardaremos el dataframe. Si no indicamos ningun parámetro Default = data
#' @return Devuelve el directorio donde guardaremos el dataframe.
#' @examples
#' createPath("test")
#' \dontrun {
#' createPath()
#'}
#'
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

#' Funcion que descarga, descomprime y abre un dataset de  scansio y lo almacena en la carpeta especificada.
#' Si no se especifica carpeta, esta será default=data.
#' @param data.url Introducir la url con el dataset en formato csv que queremos descargar.
#' @param savepath Introducir el nombre de la carpeta donde guardaremos el dataset
#' @param filename Introducir el nombre del fichero que contendrá el dataset
#' @return Devuelve el dataframe descargado con los datos en crudo
#' @examples
#' url=https://opendata.rapid7.com/sonar.tcp/2019-04-20-1555731287-http_get_5000.csv.gz
#' downloadScanIO(url,"http","scansio.http")
#' dataset=downloadScanIO(url,"http","scansio.http") #guarda el dataframe resultante en la variable indicada
#' \dontrun {
#' downloadScanIO(url,"http","scansio.http")
#'}
#' \dontrun {
#' dataset=downloadScanIO(url,"http","scansio.http")
#' }
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

#' Funcion que descarga, descomprime y abre el dataframe de Maxmind en la carpeta especificada.
#' Recordar que la carpeta debe crearse previamente con createPath(). Si no se especifica carpeta, default=data.
#' @param savepath Introducir el nombre de la carpeta creada mediante la función createPath()
#' @return Devuelve el dataframe descargado con los datos en crudo.
#' @examples
#' downloadMaxmind()
#' maxmind=downloadMaxmind() #guarda el dataframe resultante en la variable indicada
#' \dontrun {
#' downloadMaxmind()
#'}
#' \dontrun {
#' maxmind=downloadMaxmind()
#'}
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

#' Funcion que genera un dataframe del dataset especificado.
#' @param nrows Introducimos el número de filas que queremos que genere el dataframe.
#' @param scope Introducimos el scope, si no se introduce valor default=500
#' El valor de nrows debe ser < que scope.
#' @param df.port Introducimos el dataset de scans.io previamente descargado.
#' @return Genera un dataframe del dataset introducido.
#' @examples
#' generate.dfScan(600,500,df.port)
#' df.scans=generate.dfScan(600,500,df.port) #guarda la muestra resultante en la variable indicada
#' \dontrun {
#' generate.dfScan()
#'}
#' \dontrun {
#' df.scans=generate.dfScan()
#'}

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

#' Funcion que genera una muestra del dataframe Maxmind.
#' @param df.maxmind Introducimos el dataframe de maxmind previamente descargado.
#' @return Genera una muestra del dataframe.
#' @examples
#' generate.dfMaxmind(maxmind)
#' df.maxmind=generate.dfMaxmind() #guarda la muestra resultante en la variable indicada
#' \dontrun {
#' generate.dfMaxmind()
#'}
#' \dontrun {
#' df.maxmind=generate.dfMaxmind()
#'}
generate.dfMaxmind <- function(df.maxmind){
    verbose <- TRUE
df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
df.maxmind$rowname <- as.integer(row.names(df.maxmind))
return (df.maxmind)
}

#' Funcion que limpia y junta los dataframes.
#' @param df.scans Introducimos la muestra del dataframe scans generado.
#' @param df.maxmind Introducimos la muestra del dataframe maxmind generado.
#' @return Devuelve el Dataframe resultante de hacer Join de ambas muestras.
#' @examples
#' cleanAndJoin.df(df.scans,df.maxmind)
#' df=cleanAndJoin.df(df.scans,df.maxmind) #Guardamos el dataframe resultante para utilizarlo posteriormente.
#' \dontrun {
#' cleanAndJoin.df()
#'}
#'\dontrun {
#' df=cleanAndJoin.df()
#'}
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
df <- dplyr::select(df, timestamp_ts, saddr, latitude, longitude, accuracy_radius,
                    is_anonymous_proxy, is_satellite_provider)
names(df) <- c("timestamp_ts", "saddr", "slatitude", "slongitude",
               "accuracy_radius", "is_anonymous_proxy", "is_satellite_provider")

# Join and tidy data frame (destination address)
if (verbose) print("[*] Joining destination IP's with geolocation data")
suppressMessages(library(dplyr))
df.dst <- df.scans %>%
  left_join(df.maxmind, by = c("dloc" = "rowname")) %>%
  select(daddr, latitude, longitude)
names(df.dst) <- c("daddr", "dlatitude", "dlongitude")
df <- dplyr::bind_cols(df, df.dst)
rm(df.dst, df.scans)
return (df)
}

#' Función que guarda y muestra un summary del Dataframe.
#' @param df Introducimos el dataframe generado mediante las dos muestras.
#' @return Devuelve summary del dataframe.
#' @examples
#' summaryDF(df)
#' \dontrun {
#' summaryDF()
#' }
summaryDf <- function(df, savepath="data"){
    verbose <- TRUE
    output.file <- "geoftps.rds"
    tini <- Sys.time()

if (verbose) print("[*] Tidy data and save it")
df$is_anonymous_proxy <- as.factor(df$is_anonymous_proxy)
df$is_satellite_provider <- as.factor(df$is_satellite_provider)
saveRDS(object = df, file = file.path(getwd(), savepath, output.file))
fini <- Sys.time()

# Summary
fini - tini
summary(df)
}
