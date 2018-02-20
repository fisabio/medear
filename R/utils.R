
filtrar_ein_esn <- function(datos) {
  col_list    <- datos[indice == FALSE]
  no_col_list <- datos[indice == TRUE]
  col_list[, `:=`(
    p1 = mapply(function(x, y) x >= y, ref_ein, new_ein, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p2 = mapply(function(x, y) x <= y, ref_ein, new_esn, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p3 = mapply(function(x, y) x >= y, ref_esn, new_ein, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p4 = mapply(function(x, y) x <= y, ref_esn, new_esn, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )][, `:=`(
    p5 = mapply(function(w, x, y, z) (w & x) | (y & z),
                p1, p2, p3, p4,
                USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )][, sc_new := mapply(function(x, y) x[y], sc_new, p5, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  ][, paste0("p", 1:5) := NULL]
  datos <- rbindlist(list(col_list, no_col_list))[
    , c("ref_ein", "ref_esn", "new_ein", "new_esn", "indice") := NULL
    ]
  datos <- datos[, unlist(sc_new), by = list(ref_via, sc_ref, year, year2)]
  setnames(datos, "V1", "sc_new")
  return(datos)
}


#' @title Funcion para detectar cambios de seccionado en trameros
#'
#' @description Detecta cambios de secci�n censal para las provincias y el
#'   per�odo marcados.
#'
#' @param datos Objeto de clase \code{tramero_ine} (devuelto por la funci�n
#'   \code{\link{descarga_trameros}}), incluyendo obligatoriamente al a�o 2011.
#' @param years Vector num�rico de longitud >= 2 con los a�os para los que se
#'   desee consultar las variaciones de seccionado.
#'
#' @usage detecta_cambios(datos, years = 1996:2016)
#'
#' @details El tiempo de ejecuci�n de la funci�n var�a seg�n el n�mero de
#'   provincias y el rango de a�os. La forma m�s sencilla de acelerar el proceso
#'   de computaci�n es mediante la ejecuci�n en paralelo de la funci�n.
#'
#'   Los c�digos de secci�n censal siguen un orden preestablecido: los primeros
#'   dos d�gitos identifican la provincia, los siguientes tres d�gitos el
#'   municipio, los pr�ximos dos d�gitos el distrito y los �ltimos tres d�gitos
#'   hacen referencia a la secci�n censal.
#'
#' @return Un objeto de clase \code{cambios_ine} con 5 columnas:
#'   \item{sc_ref}{C�digo de la secci�n censal en el primer a�o.}
#'   \item{sc_new}{C�digo de la secci�n censal en el segundo a�o.}
#'   \item{year}{Primer a�o.} \item{year2}{Segundo a�o.} \item{vias}{Lista con
#'   el c�digo de las v�as que provocan el cambio de secci�n (incorporando un
#'   d�gito al final de la cadena indicando si se trata de numeraci�n par (0) o
#'   impar(1))}
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#'   cambios  <- detecta_cambios(datos = trameros)
#'   cambios
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{une_secciones}} y \code{\link{descarga_trameros}}.
#'
detecta_cambios <- function(datos, years = 1996:2016) {

  stopifnot("tramero_ine" %in% class(datos))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & 2011 %in% years)
  stopifnot(2011 %in% unique(datos$year))
  stopifnot(unique(datos$year) %in% years)

  cambios <- list()
  for (i in unique(datos$CPRO)) {
    tramero  <- datos[CPRO == i]
    tram_ref <- tramero[year == 2011]
    muni     <- unique(tram_ref$CMUM)

    for (j in years[years != 2011]) {
      tram_new <- tramero[year == j]

      for (k in seq_along(muni)) {
        muni_new <- tram_new[CMUM == muni[k]]

        corres <- data.table(
          ref_via = tram_ref[CMUM == muni[k], via],
          sc_ref  = tram_ref[CMUM == muni[k], seccion],
          ref_ein = tram_ref[CMUM == muni[k], EIN],
          ref_esn = tram_ref[CMUM == muni[k], ESN],
          year    = 2011,
          year2   = j
        )[, `:=`(
          sc_new  = lapply(ref_via, function(x)
            muni_new[which(muni_new[, via] == x), seccion]),
          new_ein = lapply(ref_via, function(x)
            muni_new[which(muni_new$via == x), EIN]),
          new_esn = lapply(ref_via, function(x)
            muni_new[which(muni_new$via == x), ESN]),
          indice  = lapply(lapply(ref_via, function(x)
            which(muni_new[, via] == x)), length) == 1
        )]
        corres  <- filtrar_ein_esn(corres)[sc_ref != sc_new][sc_new != ""]
        fin_1 <- lapply(
          corres$ref_via,
          function(x)
            sort(as.numeric(tram_ref$EIN[which(tram_ref$via %in% x)]))
        )
        fin_2 <- lapply(
          corres$ref_via,
          function(x)
            sort(as.numeric(tram_ref$ESN[which(tram_ref$via %in% x)]))
        )
        indice <- !mapply(function(x, y) any(y[-length(y)] >= x[-1]),
                          fin_1, fin_2, SIMPLIFY = TRUE)
        if (length(indice) != 0)
          corres <- corres[indice]

        corres <- corres[, .(list(ref_via)), by = .(sc_ref, sc_new, year, year2)]
        setnames(corres, "V1", "vias")
        cambios[[paste0("p", i, k, j)]] <- corres
      }
    }
  }

  cambios <- rbindlist(cambios)
  class(cambios) <- c(class(cambios), "cambios_ine")
  return(cambios)
}


#' @title Carga los datos privados de poblacion para el proyecto MEDEA3
#'
#' @description Algunos datos del proyecto MEDEA3 est�n encriptados para poder
#'   cumplir con la licencia INE (poblaciones desde 1998 a 2003). Esta funci�n
#'   los desencripta y a�ade a los datos p�blicos (resto de a�os), adjuntando el
#'   resultado al entorno global.
#'
#' @details La contrase�a no se almacena en el historial.
#'
#' @param key Cadena de car�cteres con la contrase�a.
#' @return No se devuelve nada.
#'
#' @usage carga_datos(key)
#'
#' @seealso \code{\link{poblacion}} y \code{\link{descarga_poblaciones}}.
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#'   carga_datos(key = "contrase�a")
#' }
#'
#' @encoding UTF-8
#'
#' @export
carga_datos <- function(key) {

  key     <- sodium::sha256(charToRaw(key))
  cifrado <- system.file("data_encrypted", "poblacion.rds",
                         package = "medear", mustWork = TRUE)
  cifrado <- unserialize(
    sodium::data_decrypt(readRDS(cifrado), key)
  )
  utils::data("poblacion")
  poblacion <- data.table::rbindlist(
    list(poblacion, cifrado), fill = TRUE
  )[order(year, sexo, seccion)]
  attributes(poblacion)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  class(poblacion) <- c(class(poblacion), "poblaciones_ine")
  return(poblacion)
  on.exit(
    try(expr = {
      ruta <- list.files(getwd(), all.files = TRUE,
                         pattern = "*\\.Rhistory$", full.names = TRUE)
      if (length(ruta) > 0) {
        historial <- readLines(ruta)
        historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
        unlink(ruta, force = TRUE)
        writeLines(historial, ruta)
        utils::loadhistory(file = ruta)
        if (.Platform$OS.type == "unix") {
          ruta <- list.files("~/.rstudio-desktop/", all.files = TRUE,
                             pattern = "history_database", full.names = TRUE)
          for (i in ruta) {
            historial <- readLines(i)
            historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
            unlink(i, force = TRUE)
            writeLines(historial, i)
          }
        } else {
          ruta <- list.files(dirname(dirname(tempdir())),
                             all.files = TRUE,
                             pattern = "history_database",
                             full.names = TRUE,
                             recursive = TRUE)
          for (i in ruta) {
            historial <- readLines(i)
            historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
            unlink(i, force = TRUE)
            writeLines(historial, i)
          }
        }
      }
    },
    silent = TRUE)
  )
}


elige_corte <- function(datos, corte) {
  stopifnot(corte %in% c(85, 100))
  res <- copy(datos)
  if (corte == 100 & "q_85_plus" %in% colnames(res)) {
    res[, q_85_plus := NULL]
  } else {
    if (!"q_85_plus" %in% colnames(res))
      res[, q_85_plus := double(.N)]
    res[,
        q_85_plus := sum(
          q_85_89, q_90_94, q_95_99, q_100_plus , na.rm = TRUE),
        by = .(seccion, sexo, year)
        ][, c("q_85_89", "q_90_94", "q_95_99", "q_100_plus") := NULL]
  }
  return(res)
}


llama_google <- function(direc, tries) {
  withRestarts(
    tryCatch(
      suppressWarnings(
        suppressMessages(
          ggmap::geocode(direc, output = "all", override_limit = TRUE)
        )
      ),
      error = function(e) {invokeRestart("retry")}
    ),
    retry = function() {
      if (tries <= 0) {
        return(list(status = "OVERQUERY_LIMIT"))
      }
      message("Failing to connect with server: retrying...")
      Sys.sleep(5)
      llama_google(direc, tries - 1)
    }
  )
}


descarga_segura <- function(x, tries = 10, ...) {
  withRestarts(
    tryCatch(
      suppressWarnings(
        suppressMessages(
          utils::download.file(
            url   = x,
            quiet = TRUE,
            ...
          )
        )
      ),
      error = function(e) {invokeRestart("retry")}
    ),
    retry = function() {
      if (tries <= 0) {
        stop("Server error: try later")
      }
      message("Failing to connect with server: retrying...")
      Sys.sleep(5)
      descarga_segura(x, tries - 1)
    }
  )
}


utils::globalVariables(
  c("CPRO", "CMUM", "DIST", "SECC", "CVIA", "EIN", "ESN", "via", "seccion",
    "CUSEC", "idn", ".", "sc_unida", "geometry", "CUSEC2", "cluster_id",
    "indice", "new_ein", "new_esn", "ref_ein", "ref_esn", "ref_via",
    paste0("p", 1:5), "sc_new", "sc_ref", "year", "year2", "cluster", "id_cluster",
    "q_100_plus", "q_85_89", "q_85_plus", "q_90_94", "q_95_99", "sc", "sexo",
    "geocodificados", "parimp_o", "parimp_c", "codigos_ine", "nombre_provincia",
    "nombre_municipio", "cod_provincia", "cod_municipio", "tip_via", "portalNumber",
    "muni", "province", "postalCode", "secciones")
)
