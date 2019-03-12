
#' @title Descarga los callejeros del INE
#'
#' @description Descarga los callejeros (trameros) que ofrece al público el INE
#'   para el año 2001 y desde 2004 en adelante.
#'
#' @param cod_provincia Cadena de carácteres de longitud >= 1 con el código de
#'   la/s provincia/s en las que se desee obtener el listado de cambios de
#'   seccionado.
#' @param years Vector numérico de longitud >= 2 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#' @param conservar Valor lógico: ¿se desea conservar los archivos descargados
#'   en el directorio oculto \code{./.trameros/} dentro del directorio de
#'   trabajo?
#' @param ntries Valor numérico: número de intentos en caso de mala conexión.
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primero
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos tres dígitos
#'   la sección censal.
#'
#' @usage descarga_trameros(cod_provincia = c(paste0("0", 1:9), 10:52), years =
#'   c(2001, 2004:2015), conservar = TRUE, ntries = 10)
#'
#' @return Un objeto de clase \code{tramero_ine} con 11 columnas:
#'   \item{CPRO}{Código de la provincia.} \item{CMUM}{Código del municipio.}
#'   \item{DIST}{Código del distrito.} \item{SECC}{Código de la sección censal
#'   reducido.} \item{CVIA}{Código de la vía reducido.} \item{EIN}{Primer portal
#'   del tramo de vía (incorpora decimales en caso de tener letra).}
#'   \item{ESN}{Último portal del tramo de vía (incorpora decimales en caso de
#'   tener letra).} \item{cod_upob}{Código de la entidad poblacional}
#'   \item{ent_colectiva}{Nombre de la entidad colectiva.}
#'   \item{ent_singular}{Nombre de la entidad singular.}
#'   \item{diseminado}{Nombre del núcleo diseminado.}
#'   \item{NVIAC}{Nombre de la vía.} \item{seccion}{Código de la sección censal
#'   completo.} \item{year}{Año del tramero.} \item{via}{Código de la vía
#'   completo (el último dígito hace referencia a si el tramo de vía es sin
#'   numeración, impar o par (0, 1, o 2, respectivamente).}
#'
#'   Cada fila representa un tramo de vía, puediendo repetirse la misma vía en
#'   varias ocasiones en función de si su recorrido recae en varias secciones
#'   censales, o si se trata de tramos de la vía con numeración par, impar o sin
#'   numeración alguna.
#'
#' @examples
#' \dontrun{
#'   library(medear)
#'   trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#'   trameros
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{descarga_cartografia}} y
#'   \code{\link{descarga_poblaciones}}.
#'
descarga_trameros <- function(cod_provincia = c(paste0("0", 1:9), 10:52),
                              years = c(2001, 2004:2015), conservar = TRUE, ntries = 10) {

  stopifnot(is.character(cod_provincia))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 0 & years %in% c(2001, 2004:(as.numeric(format(Sys.time(), "%Y")) - 1)))
  stopifnot(is.logical(conservar))
  n_cores <- data.table::getDTthreads(verbose = FALSE)
  data.table::setDTthreads(threads = 4L, restore_after_fork = FALSE)
  on.exit(data.table::setDTthreads(threads = n_cores, restore_after_fork = FALSE))

  trameros <- list()
  dir_dest <- normalizePath(
    path     = paste0(getwd(), "/.trameros/prov_", cod_provincia),
    winslash = "/",
    mustWork = FALSE
  )
  estructura <- list(
    start     = c(1, 3, 6, 8, 21, 43, 49, 54, 79, 86, 111, 136, 166),
    end       = c(2, 5, 7, 10, 25, 47, 52, 57, 85, 110, 135, 160, 190),
    col_names = c("CPRO", "CMUM", "DIST", "SECC", "CVIA", "CPOS", "EIN", "ESN",
                  "cod_upob", "ent_colectiva", "ent_singular", "diseminado", "NVIAC")
  )
  y_2001 <- FALSE
  if (2001 %in% years) {
    y_2001 <- TRUE
    years  <- years[-(years == 2001)]
  }

  descarga <- TRUE
  for (i in seq_along(dir_dest)) {
    if (!dir.exists(dir_dest[i])) {
      dir.create(dir_dest[i], recursive = TRUE)
    }
  }
  rutas <- paste0(dir_dest, "/year_", rep(substr(years, 3, 4), each = length(dir_dest)))
  if (y_2001) {
    rutas <- c(rutas, file.path(unique(dirname(dir_dest)), "TRAMG010831"))
    rutas <- rutas[-grep("_$", rutas)]
  }
  if (all(file.exists(rutas))) {
    descarga <- FALSE
  }

  if (descarga) {
    if (y_2001) {
      file_down <- paste0(unique(dirname(dir_dest)), "/nacional_2001.zip")
      descarga_segura(
        x        = "http://www.ine.es/prodyser/callejero/caj_esp/caj_esp_072001.zip",
        destfile = file_down,
        tries    = ntries
      )
      file_zip <- utils::unzip(zipfile = file_down, list = TRUE)
      file_zip <- file_zip[grep("^TRAM", file_zip[,1]), 1]
      utils::unzip(file_down, files = file_zip,
                   overwrite = TRUE, exdir = dirname(file_down))
      utils::unzip(paste0(dirname(file_down), "/", file_zip),
                   overwrite = TRUE, exdir = dirname(file_down))
    }
    for (i in seq_along(dir_dest)) {
      for (j in seq_along(years)) {
        file_down <- paste0(dir_dest[i], "/", substr(years, 3, 4)[j], ".zip")
        descarga_segura(
          x        = paste0("http://www.ine.es/prodyser/callejero/caj1",
                            substr(years, 3, 4)[j], "/call_p", cod_provincia[i], "_1",
                            substr(years, 3, 4)[j] ,".zip"),
          destfile = file_down,
          tries    = ntries
        )
        file_zip <- utils::unzip(zipfile = file_down, list = TRUE)[, 1]
        file_zip <- file_zip[grep("TRAM|t$", file_zip, ignore.case = TRUE)]

        if (grepl("\\.zip$", file_zip)) {
          utils::unzip(file_down, files = file_zip,
                       overwrite = TRUE, exdir = dir_dest[i])
          file_zip1 <- paste0(dirname(file_down), "/", file_zip)
          file_zip2 <- utils::unzip(zipfile = file_zip1, list = TRUE)[, 1]
          file_zip2 <- file_zip2[grep("TRAM|t$", file_zip2, ignore.case = TRUE)]
          utils::unzip(paste0(dirname(file_down), "/", file_zip),
                       files = file_zip2, overwrite = TRUE,
                       exdir = dir_dest[i])
          file.rename(paste0(dir_dest[i], "/", file_zip2),
                      paste0(dir_dest[i], "/", "year_", substr(years, 3, 4)[j]))
        } else {
          utils::unzip(file_down, files = file_zip,
                       overwrite = TRUE, exdir = dir_dest[i])
          file.rename(paste0(dir_dest[i], "/", file_zip),
                      paste0(dir_dest[i], "/", "year_", substr(years, 3, 4)[j]))
        }
        Sys.sleep(1)
      }
    }
  }

  ruta_tra <- matrix(NA, nrow = length(cod_provincia), ncol = length(years))
  for (i in seq_along(dir_dest)) {
    for (j in seq_along(years)) {
      ruta_tra[i, j] <- paste0(dir_dest[i], "/year_", substr(years[j], 3, 4))
      if (!file.exists(ruta_tra[i, j])) {
        stop("No existe el archivo ", ruta_tra[i, j])
      }

      tmp <- data.table(
        iconv(readLines(ruta_tra[i, j], skipNul = TRUE), "latin1", "utf8")
      )[, lapply(seq_along(estructura$start),
                 function(x)
                   trimws(stringi::stri_sub(V1, estructura$start[x], estructura$end[x])))
        ]
      colnames(tmp) <- estructura$col_names
      tmp[, `:=`(
        year          = years[j],
        seccion       = paste0(CPRO, CMUM, DIST, SECC),
        via           = paste0(CPRO, CMUM, CVIA, as.numeric(EIN) %% 2),
        ent_colectiva = gsub("", NA_character_, trimws(tmp$ent_colectiva))
      )][]
      trameros[[paste0("p", i, j)]] <- copy(tmp)
    }
  }
  if (y_2001) {
    ruta_2001 <- list.files(
      unique(dirname(dir_dest)), pattern = "TRAM.*[^\\.zip]$", full.names = TRUE
    )
    tmp <- data.table(
      iconv(readLines(ruta_2001, skipNul = TRUE), "Windows-1252", "utf8")
    )[, lapply(seq_along(estructura$start),
               function(x) trimws(stringi::stri_sub(V1, estructura$start[x], estructura$end[x])))
      ]
    colnames(tmp) <- estructura$col_names
    tmp <- tmp[, `:=`(
      year          = 2001,
      seccion       = paste0(CPRO, CMUM, DIST, SECC),
      via           = paste0(CPRO, CMUM, CVIA, as.numeric(EIN) %% 2),
      ent_colectiva = gsub("", NA_character_, trimws(ent_colectiva))
    )][CPRO %in% cod_provincia]
    trameros[["n_2001"]] <- copy(tmp)
  }
  if (!conservar)
    unlink(dirname(dir_dest), recursive = TRUE)

  trameros <- rbindlist(trameros)[order(year, seccion)]
  setkeyv(trameros, c("via", "CPOS", "seccion", "year", "CMUM"))
  setattr(trameros, "fuente", "Fuente: Sitio web del INE: www.ine.es")
  class(trameros)             <- c(class(trameros), "tramero_ine")

  return(trameros)
}


#' @title Descarga la cartografía con el seccionado del INE para 2011
#'
#' @description Descarga la cartografía del seccionado censal ofrecida
#'   públicamente por el INE para el año 2011.
#'
#' @param epsg Vector numérico de longitud uno con el código EPSG del sistema de
#'   referencia de coordenadas (CRS) empleado (por defecto se usa el 4326 con
#'   datum WGS84).
#' @param conservar Valor lógico: ¿se desea conservar los archivos descargados
#'   en el directorio oculto \code{./.cartografia/} dentro del directorio de
#'   trabajo?
#' @param ntries Valor numérico: número de intentos en caso de mala conexión.
#'
#' @usage descarga_cartografia(epsg = 4326, conservar = TRUE, ntries = 10)
#'
#' @details Aunque el INE emplea otro CRS, se recomienda utlizar el CRS 4326
#'   como elemento normalizado.
#'
#' @return Un objeto de clase \code{\link[sp]{SpatialPolygons}}, donde
#'   cada fila es una sección censal y que cuenta con 7 columnas:
#'   \item{seccion}{Cadena de 10 carácteres con el código de sección censal
#'   (incluye provincia, municipio y distrito).} \item{CUMUN}{Cadena de 5
#'   carácteres con el código del municipio (incluye provincia).}
#'   \item{CCA}{Cadena de 2 carácteres con el código de comunidad autónoma.}
#'   \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre de la comunidad
#'   autónoma.} \item{NMUN}{Nombre del municipio.}
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   library(sp)
#'   carto_ine    <- descarga_cartografia()
#'   carto_ine_46 <- carto[substr(carto$seccion, 3, 4) == "46", ]
#'   plot(carto_ine_valencia)
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{descarga_trameros}} y
#'   \code{\link{descarga_poblaciones}}.
#'
descarga_cartografia <- function(epsg = 4326, conservar = TRUE, ntries = 10) {
  stopifnot(is.logical(conservar))
  stopifnot(is.numeric(epsg))
  stopifnot(nchar(epsg) == 4)

  dir_dest <- normalizePath(
    path     = paste0(getwd(), "/.cartografia"),
    winslash = "/",
    mustWork = FALSE
  )
  if (!dir.exists(dir_dest))
    dir.create(dir_dest, recursive = TRUE)
  if (!file.exists(paste0(dir_dest, "/SECC_CPV_E_20111101_01_R_INE.shp"))) {
    descarga_segura(
      x        = "http://www.ine.es/censos2011_datos/cartografia_censo2011_nacional.zip",
      destfile = paste0(dir_dest, "/carto_2011.zip"),
      tries    = ntries
    )
    utils::unzip(
      zipfile = paste0(dir_dest, "/carto_2011.zip"),
      exdir = dir_dest
    )
  }

  carto <- rgdal::readOGR(
    dsn              = paste0(dir_dest, "/SECC_CPV_E_20111101_01_R_INE.shp"),
    verbose          = FALSE,
    stringsAsFactors = FALSE
  )
  if (!conservar)
    unlink(x = dir_dest, recursive = TRUE, force = TRUE)
  carto <- carto[, -grep("^Shape|^CNUT|CLAU2|^OB|^CSEC|^CDIS|^CMUN|^CPRO|^CUDIS",
                         colnames(carto@data))]
  names(carto)[names(carto) == "CUSEC"] <- "seccion"
  carto <- sp::spTransform(carto, CRSobj = sp::CRS(paste0("+init=epsg:", epsg)))

  attributes(carto@data)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  attributes(carto@data)$class  <-  c(attributes(carto@data)$class, "cartografia_ine")
  return(carto)
}


#' @title Descarga poblaciones del INE por sección censal, sexo, edad y año
#'
#' @description Descarga o carga las poblaciones anuales del INE por sección
#'   censal, sexo y edad por grupos quinquenales (datos desde 2006 en adelante).
#'
#' @param cod_provincia Cadena de carácteres de longitud >= 1 con el código de
#'   la/s provincia/s en las que se desee obtener el listado de cambios de
#'   seccionado.
#' @param years Vector numérico de longitud >= 1 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#' @param conservar Valor lógico: ¿se desea conservar los archivos descargados
#'   en el directorio oculto \code{./.poblaciones/} dentro del directorio de
#'   trabajo?
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primeros
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos tres a la
#'   sección censal.
#'
#'   Hasta el año 2011 el INE agrupa la última categoría de edad como 85 y más,
#'   mientras que desde el año siguiente llega hasta 100 y más.
#'
#'   Si se desea acceder a las poblaciones desde 1996 (datos adquiridos al INE),
#'   se debe utilizar la función \code{\link{carga_datos}}.
#'
#' @usage descarga_poblaciones(cod_provincia = c(paste0("0", 1:9), 10:52), years
#'   = 2004:2015, conservar = TRUE)
#'
#' @return Un objeto de clase \code{poblaciones_ine} donde las filas representan
#'   las distintas secciones censales. Las tres primeras columnas son:
#'   \item{seccion}{Código de la sección censal.} \item{sexo}{Codificado como 0
#'   para hombres y 1 para mujeres.} \item{year}{Año al que se hace referencia.}
#'   El resto de columnas representan los distintos grupos de edad.
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   poblaciones <- descarga_poblaciones(cod_provincia = "46")
#'   poblaciones
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{carga_datos}}, \code{\link{descarga_trameros}} y
#'   \code{\link{descarga_cartografia}}.
#'
descarga_poblaciones <- function(cod_provincia = c(paste0("0", 1:9), 10:52),
                                 years = 2004:2015, conservar = TRUE) {

  stopifnot(is.character(cod_provincia))
  stopifnot(is.numeric(years))
  stopifnot(length(years) >= 1 & years %in% 2004:(as.numeric(format(Sys.time(), "%Y")) - 1))
  stopifnot(is.logical(conservar))
  n_cores <- data.table::getDTthreads(verbose = FALSE)
  data.table::setDTthreads(threads = 4L, restore_after_fork = FALSE)
  on.exit(data.table::setDTthreads(threads = n_cores, restore_after_fork = FALSE))

  poblaciones <- vector("list", length(cod_provincia))
  dir_dest <- normalizePath(
    path     = paste0(getwd(), "/.poblaciones/prov_", cod_provincia),
    winslash = "/",
    mustWork = FALSE
  )

  for (i in seq_along(cod_provincia)) {
    descarga <- TRUE
    if (!dir.exists(dir_dest[i])) {
      if (conservar)
        dir.create(dir_dest[i], recursive = TRUE)
    } else {
      rutas <- paste0(dir_dest[i], "/", years, ".csv")
      if (all(file.exists(rutas))) {
        descarga    <- FALSE
        poblaciones[[i]] <- lapply(rutas, fread, colClasses = "character")
        columnas         <- sapply(poblaciones[[i]], ncol)
        for (j in seq_along(columnas)) {
          poblaciones[[i]][[j]] <- fread(
            file = rutas[j],
            colClasses = c("character", rep("numeric", 2), rep("integer", columnas[j] - 3))
          )
        }
      }
    }
    if (descarga) {
      poblaciones[[i]] <- vector("list", length(years))

      for (j in seq_along(years)) {
        ruta_des <- paste0(
          "http://www.ine.es/pcaxisdl/t20/e245/p07/a",
          years[j],
          "/l0/",
          ifelse(
            years[j] < 2006,
            paste0("02_.", cod_provincia[i]),
            ifelse(
              years[j] < 2008,
              paste0("02", cod_provincia[i]),
              ifelse(
                years[j] < 2011,
                paste0("01", cod_provincia[i]),
                paste0(cod_provincia[i], "01")
              )
            )
          ),
          ".px"
        )
        tmp <- tempfile()
        utils::download.file(ruta_des, tmp, quiet = TRUE)
        bruto <- paste0(iconv(readLines(tmp, encoding = "latin1"), "latin1", to = "ascii//translit"), collapse = "\n")
        bruto <- unlist(strsplit(bruto, ";"))
        nombres <- sapply(strsplit(bruto, split = "="), `[`, 1)
        valores <- sapply(strsplit(bruto, split = "="), `[`, 2)
        corte_pars <- valores[grep("values", nombres, ignore.case = T)]
        params <- lapply(corte_pars, function(x) trimws(strsplit(trimws(gsub("\\\"|\n", "", x)), ",")[[1]]))
        datos <- valores[grep("data", nombres, ignore.case = T)]
        datos <- as.integer(strsplit(trimws(gsub("\n|\\s+", " ", datos)), " ")[[1]])
        conjunto    <- data.table(do.call(expand.grid, params[c(3:1)]))
        conjunto <- conjunto[, lapply(.SD, as.character)]
        datos <- data.table(conjunto, datos)
        colnames(datos) <- c("edad", "seccion", "sexo", "pob")
        datos$year    <- as.numeric(years[j])
        datos <- datos[grep("\\d+", seccion)]
        datos <- datos[grep("hombr|masc|varo|mujer|feme", sexo, ignore.case = TRUE)]
        datos <- datos[grep("\\d+", edad)]
        datos$edad <- paste0("q-", datos$edad)
        datos$edad <- gsub(" y m.s", "-plus", datos$edad)
        datos$edad <- gsub("05-09", "5-9", datos$edad)
        datos$edad <- gsub("-", "_", datos$edad)
        datos      <- dcast(datos, seccion + sexo + year ~ edad, value.var = "pob")
        nombres    <- colnames(datos)
        nombres    <- c(nombres[1:3], nombres[grep("q_0", nombres)], nombres[grep("q_5_", nombres)],
                        nombres[grep("q_\\d{2}_[^p]", nombres)], nombres[grep("q_100|q_85_p", nombres)])
        setcolorder(datos, nombres)
        datos$sexo[grep("hombr|masc|varo", datos$sexo, ignore.case = TRUE)] <- "0"
        datos$sexo[grep("mujer|feme", datos$sexo, ignore.case = TRUE)] <- "1"
        datos$sexo <- as.numeric(datos$sexo)
        poblaciones[[i]][[paste0("p", i, j)]] <- datos
        invisible(file.remove(tmp))
        if (conservar) {
          fwrite(x = datos, file = paste0(dir_dest[i], "/", years[j], ".csv"))
        }
      }
    }
    poblaciones[[i]] <- rbindlist(poblaciones[[i]], fill = TRUE)
  }
  poblaciones <- rbindlist(poblaciones, fill = TRUE)
  poblaciones[, seccion := trimws(seccion)]
  setkey(poblaciones, seccion, sexo, year)
  setattr(poblaciones, "fuente", "Fuente: Sitio web del INE: www.ine.es")
  class(poblaciones) <- c(class(poblaciones), "poblaciones_ine")

  return(poblaciones)
}
