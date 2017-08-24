

#' @title Carga los trameros del INE
#'
#' @description Detecta cambios de sección censal para las provincias y el
#'   período marcados.
#'
#' @param cod_provincia Cadena de caracteres de longitud >= 1 con el código de
#'   la/s provincia/s en las que se desee obtener el listado de cambios de
#'   seccionado.
#' @param years Vector numérico de longitud >= 2 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#' @param descarga Valor lógico: ¿debe procederse a la descarga de los trameros?
#' @param ruta Cadena de caracteres indicando la ruta en la que se almacenan los
#'   archivos tal cual se descargaron desde el INE, en caso de escoger
#'   \code{descarga = FALSE}.
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primero
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos cuatro
#'   dígitos la sección censal.
#'
#' @usage descarga_trameros(cod_provincia = c(paste0("0", 1:9), 10:52), years =
#'   2004:2017, descarga = TRUE, ruta = NULL)
#'
#' @return Un objeto de clase \code{tramero_ine} con 11 columnas:
#'   \item{CPRO}{Código de la provincia.}
#'   \item{CMUM}{Código del municipio.}
#'   \item{DIST}{Código del distrito.}
#'   \item{SECC}{Código de la sección censal reducido.}
#'   \item{CVIA}{Código de la víareducido.}
#'   \item{EIN}{Primer portal del tramo de vía.}
#'   \item{ESN}{Último portal del tramo de vía.}
#'   \item{NVIAC}{Nombre de la vía.}
#'   \item{seccion}{Código de la sección censal completo.}
#'   \item{year}{Año del tramero.}
#'   \item{via}{Código de la vía completo.}
#'
#'   Cada fila representa un tramo de vía, puediendo repetirse la misma vía en
#'   varias ocasiones en función de si su recorrido recae en varias secciones
#'   censales, o si se trata de tramos de la vía con numeración par, impar o sin
#'   numeración alguna.
#'
#' @examples
#' \dontrun{
#' trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#' trameros
#' }
#'
#' @encoding UTF-8
#'
#' @export
descarga_trameros <- function(cod_provincia = c(paste0("0", 1:9), 10:52),
                              years = 2004:2017, descarga = TRUE, ruta = NULL) {

  stopifnot(is.character(cod_provincia))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & years %in% 2001:2017)
  stopifnot(is.logical(descarga))
  stopifnot(is.character(ruta) | is.null(ruta))

  if (descarga) {
    dir_dest <- paste0(tempdir(), "/prov_", cod_provincia, "/")

    for (i in seq_along(dir_dest)) {
      for (j in seq_along(years)) {
        if (!dir.exists(dir_dest[i]))
          dir.create(dir_dest[i], recursive = TRUE)

        file_down <- paste0(dir_dest[i], substr(years, 3, 4)[j], ".zip")
        utils::download.file(
          url = paste0("http://www.ine.es/prodyser/callejero/caj1",
                       substr(years, 3, 4)[j], "/call_p", cod_provincia[i], "_1",
                       substr(years, 3, 4)[j] ,".zip"),
          destfile = file_down,
          quiet    = TRUE
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
                       files = file_zip2, overwrite = TRUE, exdir = dir_dest[i])
          file.rename(paste0(dir_dest[i], file_zip2),
                      paste0(dir_dest[i], "year_", substr(years, 3, 4)[j]))
        } else {
          utils::unzip(file_down, files = file_zip,
                       overwrite = TRUE, exdir = dir_dest[i])
          file.rename(paste0(dir_dest[i], file_zip),
                      paste0(dir_dest[i], "year_", substr(years, 3, 4)[j]))
        }
        Sys.sleep(1)
      }
    }
  } else {
    if (!grepl("/$", ruta))
      ruta <- paste0(ruta, "/")
    dir_dest <- paste0(ruta, "prov_", cod_provincia)
  }

  estructura <- readr::fwf_positions(
    start     = c(1, 3, 6, 8, 21, 49, 54),
    end       = c(2, 5, 7, 10, 25, 52, 57),
    col_names = c("CPRO", "CMUM", "DIST", "SECC", "CVIA", "EIN", "ESN")
  )
  trameros <- list()
  ruta_tra <- matrix(NA, nrow = length(cod_provincia), ncol = length(years))

  for (i in seq_along(dir_dest)) {
    for (j in seq_along(years)) {
      ruta_tra[i, j] <- paste0(dir_dest[i], "/year_", substr(years[j], 3, 4))
      tramero <- readr::read_fwf(file          = ruta_tra[i, j],
                                 col_positions = estructura,
                                 col_types     = readr::cols(.default = "c"),
                                 progress      = FALSE)
      trameros[[paste0("p", i, j)]] <- as.data.table(tramero)[, `:=`(
        year    = years[j],
        seccion = paste0(CPRO, CMUM, DIST, SECC),
        via     = paste0(CPRO, CMUM, CVIA, as.numeric(EIN) %% 2)
      )]
    }
  }
  trameros <- rbindlist(trameros)
  setkey(trameros, via, seccion, year)
  attributes(trameros)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  class(trameros)             <- c(class(trameros), "tramero_ine")
  return(trameros)
}



#' @title Función para descargar la cartografía con el seccionado del INE para
#'   2011
#'
#' @description Descarga la cartografía ofrecida públicamente por el INE para el
#'   año 2011.
#'
#' @param crs Vector numérico de longitud uno con el código EPSG del sistema de
#'   referencia de coordenadas (CRS) empleado (por defecto se usa el 4326 con
#'   datum WGS84).
#'
#' @details Aunque el INE emplea otro CRS, se recomienda utlizar el CRS 4326.
#'
#' @return Un objeto de clase \code{cartografia_ine} y \code{sf}, donde cada
#'   fila es una sección censal y que cuenta con 13 columnas:
#'   \item{CUSEC}{Cádena de 10 caracteres con el código de sección censal
#'   (incluye provincia, municipio y distrito).}
#'   \item{CUMUN}{Cádena de 5 caracteres con el código del municipio (incluye
#'   provincia).}
#'   \item{CSEC}{Cádena de 3 caracteres con el código de sección censal.}
#'   \item{CDIS}{Cádena de 2 caracteres con el código de distrito.}
#'   \item{CPRO}{Cádena de 3 caracteres con el código de provincia.}
#'   \item{CCA}{Cádena de 2 caracteres con el código de comunidad autónoma.}
#'   \item{CUDIS}{Cádena de 7 caracteres con el código de distrito (incluye
#'   provincia y  municipio).}
#'   \item{OBS}{Observaciones por parte del proveedor de los datos.}
#'   \item{NPRO}{Nombre de la provincia.}
#'   \item{NCA}{Nombre de la comunidad autónoma.}
#'   \item{NMUN}{Nombre del municipio.}
#'   \item{geometry}{Columna de tipo lista con la geometría asociada a cada
#'   sección censal.}
#'
#' @examples
#' \dontrun{
#' library(sf)
#' carto_ine    <- descarga_cartografia()
#' carto_ine_46 <- carto[, "CPRO" == "46"]
#' plot(st_geometry(carto_ine_valencia))
#' }
#'
#' @export
descarga_cartografia <- function(crs = 4326) {
  dest_dir <- paste0(tempdir(), "/")
  utils::download.file(
    url = "http://www.ine.es/censos2011_datos/cartografia_censo2011_nacional.zip",
    destfile = paste0(dest_dir, "carto_2011.zip"), quiet = TRUE
  )
  utils::unzip(
    zipfile = paste0(dest_dir, "carto_2011.zip"),
    exdir = dest_dir
  )
  carto <- sf::read_sf(paste0(dest_dir, "SECC_CPV_E_20111101_01_R_INE.shp"))
  carto <- carto[, -grep("^Shape|^CNUT|CLAU2|^OBJ", colnames(carto))]
  carto <- sf::st_transform(carto, crs = crs)
  attributes(carto)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  class(carto) <- c("cartografia_ine", class(carto))
  return(carto)
}



#' @title Carga poblaciones del INE por sección censal, sexo, edad y año
#'
#' @description Descarga o carga las poblaciones anuales del INE por sección
#'   censal, sexo y edad por grupos quinquenales (datos desde 2006).
#'
#' @param cod_provincia Cadena de caracteres de longitud >= 1 con el código de
#'   la/s provincia/s en las que se desee obtener el listado de cambios de
#'   seccionado.
#' @param years Vector numérico de longitud >= 1 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#' @param descarga Valor lógico: ¿debe procederse a la descarga de los datos?
#' @param ruta Cadena de caracteres indicando la ruta en la que se almacenan los
#'   archivos tal cual se descargaron desde el INE, en caso de escoger
#'   \code{descarga = FALSE}.
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primeros
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos cuatro
#'   a la sección censal.
#'
#'   Hasta el año 2011 el INE agrupa la última categoría de edad como 85 y más,
#'   mientras que desde el año siguiente llega hasta 100 y más.
#'
#' @usage descarga_poblaciones(cod_provincia = c(paste0("0", 1:9), 10:52), years =
#'   2006:2016, descarga = TRUE, ruta = NULL)
#'
#' @return Un objeto de clase \code{poblaciones_ine} donde las filas representan
#'   las distintas secciones censales. Las tres primeras columnas son:
#'   \item{seccion}{Código de la sección censal en el primer año.}
#'   \item{sexo}{Código de la sección censal en el segundo año.}
#'   \item{year}{Primer año.}
#'   El resto de columnas representan los distintos grupos de edad.
#'
#' @examples
#' \dontrun{
#' poblaciones <- descarga_poblaciones(cod_provincia = "46")
#' poblaciones
#' }
#'
#' @encoding UTF-8
#'
#' @export
descarga_poblaciones <- function(cod_provincia = c(paste0("0", 1:9), 10:52),
                                 years = 2006:2016, descarga = TRUE, ruta = NULL) {

  stopifnot(is.character(cod_provincia))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & years %in% 2006:2016)
  stopifnot(is.logical(descarga))
  stopifnot(is.character(ruta) | is.null(ruta))

  file_down <- matrix(ncol = length(years), nrow = length(cod_provincia))

  if (descarga) {
    dir_dest <- paste0(tempdir(), "/prov_", cod_provincia, "/")

    for (i in seq_along(dir_dest)) {
      for (j in seq_along(years)) {
        if (!dir.exists(dir_dest[i]))
          dir.create(dir_dest[i], recursive = TRUE)

        file_down[i, j] <- paste0(dir_dest[i], years[j], ".csv")
        utils::download.file(
          url = paste0(
            "www.ine.es/jaxi/files/_px/es/csv_sc/t20/e245/p07/a",
            years[j],
            if (years[j] < 2011) {
              paste0(
                "/l0/0",
                if (years[j] < 2008) {
                  "2"
                } else {
                  "1"
                }, cod_provincia[i])
            } else {
              paste0("/", cod_provincia[i], "01")
            }, ".csv_sc?nocab=1"
          ),
          destfile = file_down[i, j],
          quiet    = TRUE
        )
        Sys.sleep(5)
      }
    }
  } else {
    dir_dest <- paste0(ruta, "/prov_", cod_provincia, "/")
  }

  poblaciones <- list()

  for (i in seq_along(dir_dest)) {
    for (j in seq_along(years)) {

      file_down[i, j] <- paste0(dir_dest[i], years[j], ".csv")
      aux_file <- suppressWarnings(
        readr::read_delim(
          file      = file_down[i, j],
          delim     = ";",
          col_types = readr::cols(.default = "c"),
          skip      = 5,
          progress  = FALSE
        )
      )
      anti_col    <- grep("^[X]|Total", colnames(aux_file))
      names_df    <- c("seccion", paste0("q-", colnames(aux_file)[-anti_col]))
      names_df    <- sub("05-09", "5-9", names_df)
      names_df    <- sub(" y m\u00E1s", "-plus", names_df)
      names_df    <- gsub("-", "_", names_df)
      locate_rows <- grep("TOTAL|Nota", aux_file[[1]])

      hombres <- readr::read_delim(
        file      = file_down[i, j],
        delim     = ";",
        col_types = readr::cols(X1 = "c", .default = "d"),
        skip      = locate_rows[2] + 6,
        col_names = FALSE,
        n_max     = locate_rows[3] - locate_rows[2] - 2
      )
      hombres <- as.data.table(hombres)
      hombres[, c(2, ncol(hombres)) := NULL]
      setnames(hombres, names_df)
      hombres[, `:=`(sexo = 0, year = years[j])]
      setcolorder(hombres, c("seccion", "sexo", "year",
                             colnames(hombres)[2:(ncol(hombres) - 2)]))

      mujeres <- readr::read_delim(
        file      = file_down[i, j],
        delim     = ";",
        col_types = readr::cols(X1 = "c", .default = "d"),
        skip      = locate_rows[3] + 6,
        col_names = FALSE,
        n_max     = locate_rows[4] - locate_rows[3] - 1
      )
      mujeres <- as.data.table(mujeres)
      mujeres[, c(2, ncol(mujeres)) := NULL]
      setnames(mujeres, names_df)
      mujeres[, `:=`(sexo = 1, year = years[j])]
      setcolorder(mujeres, c("seccion", "sexo", "year",
                             colnames(mujeres)[2:(ncol(mujeres) - 2)]))

      poblaciones[[paste0("p", i, j)]] <- rbindlist(list(hombres, mujeres))
    }
  }
  poblaciones <- rbindlist(poblaciones, fill = TRUE)
  poblaciones[, seccion := trimws(seccion)]
  setkey(poblaciones, seccion, sexo, year)
  attributes(poblaciones)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  class(poblaciones) <- c(class(poblaciones), "poblaciones_ine")
  return(poblaciones)
}
