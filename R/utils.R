
filtrar_ein_esn <- function(datos) {
  col_list    <- datos[indice == FALSE]
  no_col_list <- datos[indice == TRUE]
  col_list[, `:=`(
    p1 = mapply(function(x, y) x >= y, old_ein, new_ein, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p2 = mapply(function(x, y) x <= y, old_ein, new_esn, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p3 = mapply(function(x, y) x >= y, old_esn, new_ein, USE.NAMES = FALSE, SIMPLIFY = FALSE),
    p4 = mapply(function(x, y) x <= y, old_esn, new_esn, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )][, `:=`(
    p5 = mapply(function(w, x, y, z) (w & x) | (y & z),
                p1, p2, p3, p4,
                USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )][, `:=`(
    sc_new = mapply(function(x, y) x[y], sc_new, p5, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )][, paste0("p", 1:5) := NULL]
  datos <- rbindlist(list(col_list, no_col_list))[
    , c("old_ein", "old_esn", "new_ein", "new_esn", "indice") := NULL
    ]
  datos <- datos[, unlist(sc_new), by = list(old_via, sc_old, year, year2)]
  setnames(datos, "V1", "sc_new")
  return(datos)
}


#' @title Funcion para detectar cambios de seccionado en trameros
#'
#' @description Detecta cambios de sección censal para las provincias y el
#'   período marcados.
#'
#' @param datos Objeto de clase \code{tramero_ine} (devuelto por la función
#'   \code{\link{descarga_trameros}}).
#' @param years Vector numérico de longitud >= 2 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#'
#' @usage detecta_cambios(datos, years = 1996:2016)
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primeros
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos tres dígitos
#'   hacen referencia a la sección censal.
#'
#' @return Un objeto de clase \code{cambios_ine} con 4 columnas:
#'   \item{sc_old}{Código de la sección censal en el primer año.}
#'   \item{sc_new}{Código de la sección censal en el segundo año.}
#'   \item{year}{Primer año.} \item{year}{Segundo año.}
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
  stopifnot(length(years) > 1 & years %in% 1996:2016)
  cambios <- list()

  for (i in unique(datos$CPRO)) {
    tramero <- datos[CPRO == i]

    for (j in years[-length(years)]) {
      tram_old <- tramero[year == j]
      tram_new <- tramero[year == j + 1]
      muni     <- unique(tram_old[, CMUM])

      for (k in seq_along(muni)) {
        muni_old <- tram_old[CMUM == muni[k]]
        muni_new <- tram_new[CMUM == muni[k]]

        corres <- data.table(
          old_via = muni_old[, via],
          sc_old  = muni_old[, seccion],
          old_ein = muni_old[, EIN],
          old_esn = muni_old[, ESN],
          year    = muni_old[, year],
          year2   = muni_old[, year] + 1
        )[, `:=`(
          sc_new  = lapply(old_via, function(x)
            muni_new[which(muni_new[, via] == x), seccion]),
          new_ein = lapply(old_via, function(x)
            muni_new[which(muni_new$via == x), EIN]),
          new_esn = lapply(old_via, function(x)
            muni_new[which(muni_new$via == x), ESN]),
          indice  = lapply(lapply(old_via, function(x)
            which(muni_new[, via] == x)), length) == 1
        )]
        corres  <- filtrar_ein_esn(corres)[sc_old != sc_new][sc_new != ""]
        fin_1 <- lapply(
          corres[, old_via],
          function(x)
            sort(as.numeric(tram_old[which(tram_old[, via] %in% x), EIN]))
        )
        fin_2 <- lapply(
          corres[, old_via],
          function(x)
            sort(as.numeric(tram_old[which(tram_old[, via] %in% x), ESN]))
        )
        indice <- !mapply(function(x, y) any(y[-length(y)] >= x[-1]),
                          fin_1, fin_2, SIMPLIFY = TRUE)
        if (length(indice) != 0)
          corres <- corres[indice]
        corres <- corres[, old_via := NULL][!duplicated(corres)]
        setcolorder(corres, c(1, 4, 2:3))
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
#' @description Algunos datos del proyecto MEDEA3 están encriptados para poder
#'   cumplir con la licencia INE (poblaciones desde 1998 a 2003). Esta función
#'   los desencripta y añade a los datos públicos (resto de años), adjuntando el
#'   resultado al entorno global.
#'
#' @details La contraseña no se almacena en el historial.
#'
#' @param key Cadena de caracteres con la contraseña.
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
#'   carga_datos(key = "contraseña")
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
  return(poblacion)
  on.exit({
    ruta <- list.files(getwd(), all.files = TRUE,
                       pattern = "*\\.Rhistory$", full.names = TRUE)
    if (length(ruta) > 0) {
      historial <- readLines(ruta)
      historial <- historial[!grepl("carga_datos|key", historial)]
      writeLines(historial, ruta)
    }
  })
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
    "indice", "new_ein", "new_esn", "old_ein", "old_esn", "old_via",
    paste0("p", 1:5), "sc_new", "sc_old", "year", "year2", "cluster", "id_cluster",
    "q_100_plus", "q_85_89", "q_85_plus", "q_90_94", "q_95_99", "sc", "sexo",
    "geocodificados", "parimp_o", "parimp_c", "codigos_ine", "nombre_provincia",
    "nombre_municipio", "cod_provincia", "cod_municipio", "tip_via", "portalNumber",
    "muni", "province", "postalCode", "secciones")
)
