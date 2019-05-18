# Gestión datos Dataframes
_En el siguiente Proyecto podemos visualizar como tratar los datos de los dataframes scansio y maxmind_

## Paso 1
_Accedemos al package dds.base y abrimos el fichero Código.R almacenado en la carpeta R_

### Paso 2
_Inicializamos cada una de las funciones creadas mediante los comandos [CNTRL+INTRO]_

### Paso 3
_Una vez inicializadas las funciones procederemos a ejecutarlas una a una para probar su correcto funcionamiento. Guardamos su return en una variable para aquellas que sea necesario_

#### Paso 4

_Tras finalizar el proceso podremos visualizar el summary del dataframe resultante_

##### Ejemplo

_Inicializamos las funciones creadas mediante [CNTRL+INTRO].
Ejecutamos por consola los siguiente comandos:
createPath()
descarga1=downloadScanIO()
descarga2=downloadMaxmind()
df.scans=generate.dfScan(5000,descarga1)
df.maxmind=generate.dfMaxmind(descarga2)
df=cleanAndJoin.df(df.scans,df.maxmind)
summaryDf(df)
Tras finalizar este proceso visualizaremos el summary del dataframe resultante_