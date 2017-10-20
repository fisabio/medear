
medear
======

El propósito del paquete `medear` es facilitar el trabajo con los datos utilizados en el proyecto MEDEA3, y proporcionar las funciones con las que se obtuvieron dichos datos.

Concretamente, se incluyen los datos de población por sexo y grupos de edad así como la cartografía para las ciudades implicadas en el proyecto a nivel de sección censal. Los datos para algunos años están encriptados por tratarse de consultas específicas realizadas al INE y solo son accesibles mediante contraseña. Los datos desde 2006 hasta 2017 son de libre acceso siguiendo la licencia del INE, y se pueden obtener usando las funciones contenidas en este paquete.

Instalación
-----------

Se puede instalar `medear` desde GitHub con:

``` r
# install.packages("devtools")
devtools::install_github("fisabio/medear")
```

Carga y preparación de los datos
--------------------------------

El uso habitual del paquete será la carga de datos y de la cartografia, incluyendo su preparación para unir las secciones según el rango de años que se desee (funciones `une_poblaciones` y `une_cartografia`):

``` r
library(medear)
cartografia <- une_cartografia(cartografia = cartografia, cambios = cambios, years = 2008:2016)
poblacion   <- une_poblaciones(poblaciones = poblaciones, cambios = cambios, years = 2008:2016)
```

Consultas específicas
---------------------

Un uso potencial puede ser la prepararación de consultas personalizadas para ciudades o provincias completas, descargando los datos y la cartografía directamente desde el INE y trabajarlos para obtener el producto final. Imaginemos que se desease trabajar con las poblaciones desde 2011 hasta 2014 para la provincia de Valencia:

``` r
library(medear)
carto_valencia <- descarga_cartografia()
pob_valencia   <- descarga_poblaciones(cod_provincia = "46", years = 2011:2014, descarga = TRUE)
tram_valencia  <- descarga_trameros(cod_provincia = "46", years = 2011:2014, descarga = TRUE)
camb_valencia  <- detecta_cambios(datos = tram_valencia, years = 2011:2014)
resultado      <- une_secciones(camb_valencia, pob_valencia, carto_valencia, 2011:2014, 85)
opar <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
plot(sf::st_geometry(cartografia[, "CUSEC"]), main = "SC's sin unir")
plot(sf::st_geometry(resultado$cartografia[, "CUSEC"]), main = "SC's unidas")
title(unique(cartografia$NMUN), outer = T)
df <- data.frame(
    ciudad  = unique(cartografia$NMUN),
    antes   = nrow(cartografia),
    despues = nrow(resultado$cartografia)
  )
par(opar)
df$dif <- df$antes - df$despues
df
```

Participación: dudas y consultas
--------------------------------

Para cualquier comentario, duda o consulta que se desee realizar, se puede abrir un *Issue* en este repositorio de GitHub.

Si deseases aportar algo al proyecto, puedes modificar el código a tu antojo y abrir un *Pull Request*.
