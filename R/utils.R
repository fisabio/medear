
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
#' @param catastro Car�cter. Argumento opcional (nulo por defecto): ruta hasta
#'   el archivo alfanum�rico con la informaci�n catastral completa de un
#'   municipio.
#'
#' @usage detecta_cambios(datos, years = c(1996, 2001, 2004:2016), catastro =
#'   NULL)
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
#'   Si se proporciona un archivo alfanum�rico con la informaci�n catastral de
#'   un municipio, la funci�n solo calcula y devuelve los cambios de ese
#'   municipio, incorporando el n�mero de viviendas y tramos detectados en la
#'   informaci�n catastral.
#'
#'   El archivo de catastro se debe descargar de la
#'   \href{https://www.sedecatastro.gob.es/OVCFrames.aspx?TIPO=TIT&a=masiv}{sede
#'   electronica del catastro} (apartado denominado "Descarga de informaci�n
#'   alfanum�rica (formato CAT)"), paso que requiere de un certificado digital.
#'
#'   En cualquier caso, y como la finalidad de esta funci�n es servir en la
#'   uni�n de secciones (funci�n \code{\link{une_secciones}}), aunque se trata
#'   de un argumento opcional es muy recomendable aportar la informaci�n
#'   catastral, pues resulta imprescindible a la hora de decidir si ha de
#'   realizarse la uni�n de dos o m�s secciones.
#'
#' @return Si no se proporciona un fichero con informaci�n catastral, la funcion
#'   devuelve un objeto de clase \code{cambios_ine} con 5 columnas:
#'   \item{sc_ref}{C�digo de la secci�n censal en el primer a�o.}
#'   \item{sc_new}{C�digo de la secci�n censal en el segundo a�o.}
#'   \item{year}{Primer a�o.} \item{year2}{Segundo a�o.} \item{vias}{Lista con
#'   el c�digo de las v�as que provocan el cambio de secci�n (incorporando un
#'   d�gito al final de la cadena indicando si se trata de numeraci�n par (0) o
#'   impar(1))}.
#'
#'   Si se proporciona un archivo con informaci�n catastral de un municipio, la
#'   funci�n solo calcula los cambios para ese municipio y devuelve, ademas de
#'   los campos del anterior supuesto, el n�mero de viviendas afectadas por cada
#'   cambio y el porcentaje de tramos identificados en el archivo catastral.
#'
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#'   # Sin informaci�n catastral
#'   cambios  <- detecta_cambios(datos = trameros)
#'   cambios
#'
#'   # Con informaci�n catastral para la ciudad de Castell�n de la Plana
#'   # (el nombre del archivo puede variar)
#'   trameros <- descarga_trameros(cod_provincia = "12")
#'   cambios  <- detecta_cambios(datos = trameros, catastro = "12_900_U_2018-01-19.CAT")
#'   cambios
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{une_secciones}} y \code{\link{descarga_trameros}}.
#'
detecta_cambios <- function(datos, years = c(1996, 2001, 2004:2016), catastro = NULL) {

  stopifnot("tramero_ine" %in% class(datos))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & 2011 %in% years)
  stopifnot(2011 %in% unique(datos$year))
  stopifnot(years %in% unique(datos$year))
  if (!is.null(catastro)) {
    stopifnot(file.exists(catastro))

    catastro_finca  <- lee_catastro(catastro)

    stopifnot(unique(catastro_finca$prov_ine) %in% datos$CPRO)
    stopifnot(unique(catastro_finca$muni_ine) %in% datos$CMUM)

    datos <- datos[
      CPRO == unique(catastro_finca$prov_ine) &
        CMUM == unique(catastro_finca$muni_ine)
    ]
  }

  cambios <- list()
  for (i in unique(datos$CPRO)) {
    tramero  <- datos[CPRO == i]

    for (j in years[years != 2011]) {
      if (j < 2011) {
        tram_ref <- tramero[year == j]
        tram_new <- tramero[year == 2011]
        year_ref <- j
        year_com <- 2011
      } else {
        tram_ref <- tramero[year == 2011]
        tram_new <- tramero[year == j]
        year_ref <- 2011
        year_com <- j
      }
      muni     <- unique(tram_ref$CMUM)

      for (k in seq_along(muni)) {
        muni_new <- tram_new[CMUM == muni[k]]

        corres <- data.table(
          ref_via = tram_ref[CMUM == muni[k], via],
          sc_ref  = tram_ref[CMUM == muni[k], seccion],
          ref_ein = tram_ref[CMUM == muni[k], EIN],
          ref_esn = tram_ref[CMUM == muni[k], ESN],
          year    = year_ref,
          year2   = year_com
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

  cambios <- lapply(cambios, function(x)
    rbindlist(list(x[year == 2011], x[year != 2011, c(2, 1, 4, 3, 5)]))
  )
  cambios <- rbindlist(cambios)
  class(cambios) <- c(class(cambios), "cambios_ine")

  if (!is.null(catastro)) {
    tramero_cambios <- filtra_tramero(datos, cambios)
    viviendas_def   <- calcula_viviendas(tramero_cambios, catastro_finca)
    tramos_cat <- mapply(
      function(x, y) x / nrow(datos[seccion == y & year == 2011]) * 100,
      viviendas_def$n_na, cambios$sc_ref
    )
    cambios[, viviendas := viviendas_def$n_viv]
    cambios <- cambios[, tramo_por := tramos_cat][]
    setcolorder(cambios, c(1:4, 6:7, 5))
  }

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
          q_85_plus, q_85_89, q_90_94, q_95_99, q_100_plus , na.rm = TRUE),
        by = .(seccion, sexo, year)
        ][, c("q_85_89", "q_90_94", "q_95_99", "q_100_plus") := NULL][]
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


lee_catastro <- function(archivo) {
  stopifnot(is.character(archivo))

  estructura_finca <- readr::fwf_positions(
    start     = c(1, 26, 31, 51, 81, 154, 334, 343),
    end       = c(2, 28, 44, 52, 83, 158, 342, 352),
    col_names = c("tipo_reg", "muni_dgc", "ref_cat", "prov_ine", "muni_ine",
                  "via_dgc", "lng", "lat")
  )
  catastro_finca <- readr::read_fwf(
    file          = archivo,
    col_positions = estructura_finca,
    col_types     = readr::cols(.default = "c"),
    locale        = readr::locale(encoding = "latin1"),
    progress      = FALSE
  )

  catastro_finca <- as.data.table(catastro_finca)[tipo_reg == "11"][, tipo_reg := NULL][]

  estructura_vivienda <- readr::fwf_positions(
    start     = c(1, 31, 201, 206, 231, 428),
    end       = c(2, 44, 205, 230, 234, 428),
    col_names = c("tipo_reg", "ref_cat", "tvias", "vias", "npolis", "clave")
  )
  catastro_vivienda <- as.data.table(
    readr::read_fwf(
      file          = archivo,
      col_positions = estructura_vivienda,
      col_types     = readr::cols(.default = "c"),
      locale        = readr::locale(encoding = "latin1"),
      progress      = FALSE
    )
  )[tipo_reg == "15" & clave == "V"][, c("tipo_reg", "clave") := NULL][order(ref_cat)]

  # Agregar informacion de cada finca en columnas tipo lista
  catastro_vivienda <- catastro_vivienda[, `:=`(
    tvia   = list(tvias),
    nvia   = list(vias),
    npoli  = list(as.numeric(npolis))
  ), by = ref_cat][
    , c("tvias", "vias", "npolis") := NULL
    ][!duplicated(ref_cat)][]

  # Agregar a la BBDD principal la informacion de viviendas
  catastro_finca     <- catastro_finca[
    ref_cat %in% catastro_vivienda$ref_cat
    ][order(ref_cat)][, `:=`(
      tvia  = catastro_vivienda$tvia,
      nvia  = catastro_vivienda$nvia,
      npoli = catastro_vivienda$npoli)][]
  class(catastro_finca) <- c(class(catastro_finca), "catastro")

  return(catastro_finca)
}


filtra_tramero <- function(tramero, cambios) {
  stopifnot("tramero_ine" %in% class(tramero))
  stopifnot("cambios_ine" %in% class(cambios))

  tramero_copia   <- copy(tramero)
  cambios_tramero <- copy(cambios)
  res             <- vector("list", nrow(cambios_tramero))
  message("Filtrando el tramero...\n")
  pb1             <- utils::txtProgressBar(min = 0, max = length(res), style = 3)

  for (fila in seq_len(nrow(cambios_tramero))) {
    # Filtrar tramero por secciones de interes (referencia)
    tram1 <- tramero_copia[
      via %in% unlist(cambios_tramero[fila, vias]) &
        year == cambios_tramero[fila, year] &
        seccion == cambios_tramero[fila, sc_ref]
      ][,
        `:=`(
          sc_ref   = seccion,
          year_ref = year,
          old_ein  = as.numeric(EIN),
          old_esn  = as.numeric(ESN),
          sc_new   = NA_character_,
          year_new = NA_real_,
          new_ein  = NA_real_,
          new_esn  = NA_real_
        )][, c("year", "seccion", "EIN", "ESN", "DIST", "SECC", "CVIA") := NULL][]

    # Filtrar tramero por secciones de interes (comparacion)
    tram2 <- tramero_copia[
      via %in% unlist(cambios_tramero[fila, vias]) &
        year == cambios_tramero[fila, year2] &
        seccion == cambios_tramero[fila, sc_new]
      ][,`:=`(
        sc_new   = seccion,
        year_new = year,
        new_ein  = as.numeric(EIN),
        new_esn  = as.numeric(ESN),
        sc_ref   = NA_character_,
        year_ref = NA_real_,
        old_ein  = NA_real_,
        old_esn  = NA_real_
      )][, c("year", "seccion", "EIN", "ESN", "DIST", "SECC", "CVIA") := NULL][]

    setcolorder(tram2, colnames(tram1))

    for (i in seq_len(nrow(tram1))) {
      for (j in seq_len(nrow(tram2))) {
        if ((tram2$new_ein[j] %in% seq(tram1$old_ein[i], tram1$old_esn[i], by = 2)) |
            (tram2$new_esn[j] %in% seq(tram1$old_ein[i], tram1$old_esn[i], by = 2))) {
          tram1[i, `:=`(
            sc_new   = tram2[j, sc_new],
            year_new = tram2[j, year_new],
            new_ein  = tram2[j, new_ein],
            new_esn  = tram2[j, new_esn]
          )]
        }
      }
    }
    for (i in seq_len(nrow(tram2))) {
      for (j in seq_len(nrow(tram1))) {

        if ((tram1$old_ein[j] %in% seq(tram2$new_ein[i], tram2$new_esn[i], by = 2)) |
            (tram1$old_esn[j] %in% seq(tram2$new_ein[i], tram2$new_esn[i], by = 2))) {
          tram2[i, `:=`(
            sc_ref   = tram1[j, sc_ref],
            year_ref = tram1[j, year_ref],
            old_ein  = tram1[j, old_ein],
            old_esn  = tram1[j, old_esn]
          )]
        }
      }
    }

    # Union de los dos trameros
    res[[fila]] <- unique(rbind(tram1, tram2))[
      !is.na(old_ein) & !is.na(new_ein)
      ][(old_ein >= new_esn | new_ein <= old_esn) &
          (new_ein >= old_esn | old_ein <= new_esn)][]
    utils::setTxtProgressBar(pb1, fila)
  }

  return(res)
}


calcula_viviendas <- function(tramero_cambios, catastro_finca) {
  stopifnot("catastro" %in% class(catastro_finca))
  stopifnot(is.list(tramero_cambios))

  fincas  <- copy(catastro_finca)
  message("\nCalculando las viviendas afectadas en cada cambio...\n")
  pb      <- utils::txtProgressBar(min = 0, max = length(tramero_cambios), style = 3)
  n_viv   <- vector("list", length(tramero_cambios))
  n_na    <- vector("list", length(tramero_cambios))
  fincas[, nvia2 := lapply(nvia, function(x) trimws(gsub("\\b\\w{1,2}\\b\\s?|\\(|\\)|\\bDEL\\b", "", gsub("[[:punct:]]", "", x))))]

  for (cambio in seq_along(tramero_cambios)) {
    tramero <- copy(tramero_cambios[[cambio]])

    if (nrow(tramero) != 0) {
      # Union de los inicios y finales de tramo
      tramero[, `:=`(xx = list(), yy = list(), zz = list())]
      tramero[
        old_ein != new_ein,
        xx := .(list(unique(unlist(as.vector(mapply(function(w, x, y, z) seq(max(c(w, x)), min(c(y, z)), by = 2), old_ein, new_ein, old_esn, new_esn)))))),
        by = via
        ]

      tramero[
        old_esn != new_esn,
        yy := .(list(unique(unlist(as.vector(mapply(function(w, x, y, z) seq(max(c(w, x)), min(c(y, z)), by = 2), old_ein, new_ein, old_esn, new_esn)))))),
        by = via
        ]
      tramero[
        old_ein == new_ein & old_esn == new_esn,
        zz := .(list(unique(unlist(as.vector(mapply(function(x, y) seq(x, y, by = 2), old_ein, old_esn)))))),
        by = via
        ]

      npks_list <- c(mapply(c, tramero$xx, tramero$yy, tramero$zz))
      if (is.list(npks_list)) {
        npks_list <- lapply(npks_list, function(x) x[x != 0])
        tramero[, npk := npks_list]
      } else {
        tramero[, npk := .(list(npks_list))]
      }
      tramero <- tramero[, .(list(npk)), by = .(NVIAC, sc_ref)][
        ,
        lapply(V1, function(x) list(sort(unlist(x)))), by = .(NVIAC, sc_ref)
        ][]
      tramero[, V1 := .(lapply(V1, unique))]

      # Conservar las v�as con portales asociados
      tramero <- tramero[which(sapply(V1, length) != 0)]

      # Retirar palabras de tres o menos caracteres
      tramero[, NVIAC2 := trimws(gsub("\\b\\w{1,2}\\b\\s?|\\bDEL\\b", "", gsub("[[:punct:]]", "", NVIAC)))]

      # Numero de viviendas en cada tramo de via afectado por el cambio de SC
      n_viv[[cambio]] <- vector("integer", nrow(tramero))
      n_na[[cambio]]  <- vector("integer", nrow(tramero))
      for (i in seq_along(tramero$NVIAC2)) {
        n_viv[[cambio]][i] <- tryCatch({
          indice_cat <- which(
            sapply(lapply(fincas$nvia2, grep, pattern = paste0("^", tramero$NVIAC2[i], "$")), length) != 0
          )
          if (length(indice_cat) == 0) n_na[[cambio]][i] <- 1
          indice_poli <- lapply(fincas$nvia2[indice_cat], grep, pattern = paste0("^", tramero$NVIAC2[i], "$"))
          fincas[
            indice_cat,
            sum(
              sapply(
                mapply(
                  function(x, y, z) x[y] %in% z,
                  npoli,
                  indice_poli,
                  tramero[i]$V1
                ),
                sum
              )
            )
            ][]
        },
        error = function(e) 0
        )
      }
    } else {
      n_viv[[cambio]] <- 0
      n_na[[cambio]]  <- 0
    }
    utils::setTxtProgressBar(pb, cambio)
  }

  return(list(n_viv = sapply(n_viv, sum), n_na = sapply(n_na, sum)))
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
    "muni", "province", "postalCode", "secciones", "cambio_ref", "camb_distrito",
    "n_viv", "viv_ref", "viviendas", "no_11", "colin", "NVIAC", "NVIAC2", "V1",
    "npk", "npoli", "nvia", "nvia2", "old_ein", "old_esn", "vias", "xx",
    "year_new", "year_ref", "yy", "zz", "CPOS", "clave", "npolis", "ref_cat",
    "tipo_reg", "tramo_por", "tvias")
)
