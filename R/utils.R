
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
#' @description Detecta cambios de sección censal para las provincias y el
#'   período marcados.
#'
#' @param datos Objeto de clase \code{tramero_ine} (devuelto por la función
#'   \code{\link{descarga_trameros}}), incluyendo obligatoriamente al año 2011.
#' @param years Vector numérico de longitud >= 2 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#' @param cod_postal Valor lógico: ¿Debe añadirse el código postal a la vía? Por
#'   defecto falso, aunque es útil en casos de ciudades con pedanías que tengan
#'   nombres de vía comunes (a su vez suelen compartir mismo código de vía).
#' @param catastro Carácter. Argumento opcional (nulo por defecto): ruta hasta
#'   el archivo alfanumérico con la información catastral completa de un
#'   municipio.
#'
#' @usage detecta_cambios(datos, years = c(1996, 2001, 2004:2016), cod_postal =
#'   FALSE, catastro = NULL)
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
#'   Si se proporciona un archivo alfanumérico con la información catastral de
#'   un municipio, la función solo calcula y devuelve los cambios de ese
#'   municipio, incorporando el número de viviendas y tramos detectados en la
#'   información catastral.
#'
#'   El archivo de catastro se debe descargar de la
#'   \href{https://www.sedecatastro.gob.es/OVCFrames.aspx?TIPO=TIT&a=masiv}{sede
#'   electronica del catastro} (apartado denominado "Descarga de información
#'   alfanumérica (formato CAT)"), paso que requiere de un certificado digital.
#'
#'   En cualquier caso, y como la finalidad de esta función es servir en la
#'   unión de secciones (función \code{\link{une_secciones}}), aunque se trata
#'   de un argumento opcional es muy recomendable aportar la información
#'   catastral, pues resulta imprescindible a la hora de decidir si ha de
#'   realizarse la unión de dos o más secciones.
#'
#' @return Si no se proporciona un fichero con información catastral, la funcion
#'   devuelve un objeto de clase \code{cambios_ine} con 5 columnas:
#'   \item{sc_ref}{Código de la sección censal en el primer año.}
#'   \item{sc_new}{Código de la sección censal en el segundo año.}
#'   \item{year}{Primer año.} \item{year2}{Segundo año.} \item{vias}{Lista con
#'   el código de las vías que provocan el cambio de sección (incorporando un
#'   dígito al final de la cadena indicando si se trata de numeración par (0) o
#'   impar(1))}.
#'
#'   Si se proporciona un archivo con información catastral de un municipio, la
#'   función solo calcula los cambios para ese municipio y devuelve, ademas de
#'   los campos del anterior supuesto, el número de viviendas afectadas por cada
#'   cambio y el porcentaje de tramos identificados en el archivo catastral.
#'
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#'   # Sin información catastral
#'   cambios  <- detecta_cambios(datos = trameros)
#'   cambios
#'
#'   # Con información catastral para la ciudad de Castellón de la Plana
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
detecta_cambios <- function(datos, years = c(1996, 2001, 2004:2016),
                            cod_postal = FALSE, catastro = NULL) {

  stopifnot("tramero_ine" %in% class(datos))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & 2011 %in% years)
  stopifnot(all(c("CPRO", "CMUM", "CVIA", "EIN", "ESN",
                  "DIST", "SECC", "CPOS", "year") %in% names(datos)))
  stopifnot(2011 %in% unique(datos$year))
  stopifnot(years %in% unique(datos$year))

  datos[, via := paste0(CPRO, CMUM, CVIA, as.numeric(EIN) %% 2)]

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
  if (cod_postal) datos[, via := paste0(via, CPOS)]

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

  cambios <- rbindlist(
    lapply(cambios, function(x)
      if (all(x$year < 2011)) {
        setcolorder(x, c(2, 1, 4, 3, 5))
        setnames(x, names(x)[c(2, 1, 4, 3, 5)])
      } else {
        x
      }
    )
  )
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
#' @description Algunos datos del proyecto MEDEA3 están encriptados para poder
#'   cumplir con la licencia INE (poblaciones desde 1998 a 2003). Esta función
#'   los desencripta y añade a los datos públicos (resto de años), adjuntando el
#'   resultado al entorno global.
#'
#' @details La contraseña no se almacena en el historial.
#'
#' @param key Cadena de carácteres con la contraseña.
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
  utils::data("poblacion", envir = environment())
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
    start     = c(1, 26, 31, 51, 81, 154, 334, 343, 672),
    end       = c(2, 28, 44, 52, 83, 158, 342, 352, 677),
    col_names = c("tipo_reg", "muni_dgc", "ref_cat", "prov_ine", "muni_ine",
                  "via_dgc", "lng", "lat", "epsg")
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
  catastro_finca[, c("lng", "lat") := lapply(.SD, function(x)
    as.numeric(paste0(substr(x, 1, 2), gsub("^(.{2})", ".", x)))
  ), .SDcols = c("lng", "lat")
  ]
  attributes(catastro_finca)$epsg <- max(unique(catastro_finca$epsg))
  catastro_finca[, c("epsg") := NULL]

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

      # Conservar las vias con portales asociados
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

#' @title Deteccion de agrupaciones de mortalidad a revisar manualmente
#'
#' @description Esta función es útil a la hora de comprobar la geocodificación
#'   de la mortalidad, pues devuelve aquellos puntos con un exceso de
#'   fallecimientos en relación a la media de sus vecinos más próximos. Del
#'   mismo modo, también identifica los centros residenciales, donde es de
#'   esperar una mayor aglomeración de defunciones.
#'
#' @usage detecta_cluster(datos, epsg = 4326, vecinos = 10, cartografia = NULL)
#'
#' @param datos Base de datos con las coordenadas que ubican cada uno de los
#'   fallecimientos. Debe contener, al menos, 9 columnas: \code{BOD.direccion},
#'   \code{lat}, \code{lng}, \code{province}, \code{muni}, \code{tip_via},
#'   \code{address}, \code{portalNumber} y \code{postalCode}), las cuales deben
#'   tener exactamente esos nombres (son los que resultan del protocolo de
#'   geocodificación, así que este aspecto no debería causar problema alguno).
#'   Si la base de datos tuviera otros nombres, seria trabajo del usuario
#'   cambiárselos como paso previo al uso de la función.
#' @param epsg Numérico con longitud igual a 1: código EPSG con la proyección de
#'   las coordenadas de la base de datos. Por defecto se usa el EPSG 4326
#'   (longlat WGS 84).
#' @param vecinos Numérico con longitud igual a 1: número de vecinos más
#'   próximos con los que comparar la mortalidad de cada punto. Por defecto 10.
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}}. Con
#'   ella se representa el seccionado oficial de 2011 a modo de referencia con
#'   la que juzgar la geocodificación de un conjunto de defunciones anómalas. En
#'   principio tiene valor nulo, lo que implica usar la cartografía asociada al
#'   paquete para las ciudades MEDEA3. Si el usuario desea consultar otros
#'   municipios puede hacer uso de este argumento para tener el seccionado de
#'   fondo.
#'
#' @details La función comienza calculando el número de fallecimientos en cada
#'   par único de coordenadas, identifica los \emph{n} vecinos más próximos a
#'   cada punto y calcula la media de fallecimientos en los mismos. Considerando
#'   que los fallecimientos en cada par de coordenadas siguen una distribución
#'   de Poisson cuya media es la media de los fallecimientos en los puntos más
#'   cercanos, se calcula la probabilidad de superar esa media. En la
#'   representación
#'
#' @return Un objeto de clase \code{\link[leaflet]{leaflet}} en el que se marcan
#'   los puntos a revisar. Los puntos se dividen en tres colores en función de
#'   la mortalidad acontecida en las coordenadas vecinas: verde si la
#'   probabilidad de obtener ese resultado es inferior a 1e-10, amarillo si es
#'   inferior a 1e-15 y rojo si es inferior a 1e-20 (el caso más evidente).
#'   Haciendo clic en cada uno de los puntos se puede consultar las direcciones,
#'   tanto del BOD como las obtenidas en la geocodificación (se puede cambiar
#'   entre una y otra en el menú del extremo superior derecho), asociadas a cada
#'   punto. Al comienzo de cada dirección se indica el número de fila al que
#'   hace referencia cada dirección en los datos que se facilitaron, de forma
#'   que pueda recuperarse fácilmente dicha información para explorar los datos
#'   en profundidad.
#'
#'   Respecto al menú anteriormente mencionado, también permite cambiar la capa
#'   de visualización de fondo, utilizando OpenStreetMap, Google Maps o Google
#'   Satellite (de forma que pueda disponerse de información extra sin salir de
#'   la aplicación).
#'
#'   Si se desea efectuar el cálculo de distancias o áreas, en la esquina
#'   inferior izquierda se dispone de un menú para realizarlas (en metros y en
#'   metros cuadrados).
#'
#' @examples
#' \dontrun{
#'   library(medear)
#'   library(sp)
#'   revisar <- detecta_cluster(datosmort)
#'   plot(revisar)
#' }
#'
#' @encoding UTF-8
#'
#' @export
detecta_cluster <- function(datos, epsg = 4326, vecinos = 10, cartografia = NULL) {

  vars <- c("BOD.direccion", "lat", "lng", "province", "muni",
            "tip_via", "address", "portalNumber", "postalCode")
  columnas = c("lng", "lat")
  if (!all(vars %in% names(datos))) {
    stop("\nAlguna de las variables necesarias est\u00e1n ausentes en los datos o ",
         "requieren un cambio de nombre.\nPor favor, revise la documentaci\u00f3n.")
  }
  stopifnot(is.numeric(vecinos) & length(vecinos) == 1)
  stopifnot(is.numeric(epsg) & length(epsg) == 1)
  if (!is.null(cartografia) && !"SpatialPolygonsDataFrame" %in% class(cartografia)) {
    stop("\nEl objeto 'cartografia' debe ser un 'SpatialPolygonsDataFrame'")
  }

  if (is.null(cartografia)) {
    utils::data("cartografia", envir = environment())
  }
  carto_cl <- cartografia
  limite <- c(1e-10, 1e-15, 1e-20)
  datos$id_n <- seq_len(nrow(datos))
  if (is.data.table(datos)) {
    bdd <- datos[stats::complete.cases(datos[, columnas, with = FALSE])]
  } else {
    datos <- as.data.table(datos)
    bdd   <- datos[stats::complete.cases(datos[, columnas])]
  }

  setkeyv(bdd, columnas)
  bdd[, geo_dir := paste0(tip_via, " ", address, " ", portalNumber, ", ", muni, ", ", province, ", ", postalCode)]
  grupo  <- bdd[, c(columnas), with = FALSE][, .N, by = c(columnas)]
  for (i in seq_along(columnas)) {
    set(grupo, j = columnas[i], value = as.numeric(grupo[[columnas[i]]]))
  }
  grupo_sp                  <- copy(grupo)
  sp::coordinates(grupo_sp) <- stats::as.formula(paste("~", paste(columnas, collapse = " + ")))
  sp::proj4string(grupo_sp) <- sp::CRS(paste0("+init=epsg:", epsg))
  knn10 <- nabor::knn(sp::coordinates(grupo_sp), k = vecinos + 1)[[1]][, -1]
  grupo[, pr := .(lapply(seq_len(nrow(knn10)), function(x) knn10[x, ]))]
  grupo[, tr := sapply(pr, function(x) as.integer(round(mean(grupo[x, N]))))]
  grupo[, pr := NULL]
  grupo[, prob := mapply(function(x, y) stats::ppois(x, y, lower.tail = FALSE), N, tr)]

  grupo_sp$limite                         <- NA_character_
  grupo_sp$bod_dir                        <- NA_character_
  grupo_sp$geo_dir                        <- NA_character_
  grupo_sp$limite[grupo$prob < limite[1]] <- "1e-10"
  grupo_sp$limite[grupo$prob < limite[2]] <- "1e-15"
  grupo_sp$limite[grupo$prob < limite[3]] <- "1e-20"
  grupo_sp                                <- grupo_sp[grupo$prob < limite[1], ]
  rownames(grupo_sp@data)                 <- seq_len(nrow(grupo_sp))
  coord_grupo                             <- sp::coordinates(grupo_sp)
  for (i in seq_len(nrow(coord_grupo))) {
    pegote <- datos[
      lng == coord_grupo[i, "lng"] & lat == coord_grupo[i, "lat"],
      c(paste0(tip_via, " ", address, " ", portalNumber, ", ",
               muni, ", ", province, ", ", postalCode),
        "BOD.direccion", "id_n")
    ]
    grupo_sp$geo_dir[i] <- paste("<p>", pegote[[3]], pegote[[1]], "</p>", collapse = "")
    grupo_sp$bod_dir[i] <- paste("<p>", pegote[[3]], pegote[[2]], "</p>", collapse = "")
  }

  xx <- suppressWarnings(rgeos::gWithin(grupo_sp, carto_cl, byid = T))
  yy <- apply(xx, 1, sum)
  zz <- unique(carto_cl$CUMUN[which(yy != 0)])

  carto_cl <- carto_cl[carto_cl$CUMUN %in% zz, ]
  icon_pop    <- leaflet.extras::pulseIcons(
    color     = ifelse(
      grupo_sp$limite == "1e-10", "green", ifelse(grupo_sp$limite == "1e-15", "orange", "red")
    ),
    iconSize  = 5,
    heartbeat = .5
  )


  mapa_cluster <- leaflet::leaflet()
  mapa_cluster <- leaflet::addPolygons(
    map              = mapa_cluster,
    data             = carto_cl,
    popup            = paste0("Secci\u00f3n (2011): ", carto_cl$seccion),
    color            = "#6890FF",
    weight           = 2,
    smoothFactor     = 0.5,
    opacity          = 1.0,
    fillOpacity      = 0,
    highlightOptions = leaflet::highlightOptions(color = "#53C853", weight = 1.5, bringToFront = TRUE),
    group            = "Secciones INE 2011"
  )
  mapa_cluster <- leaflet.extras::addPulseMarkers(
    map          = mapa_cluster,
    data         = grupo_sp,
    popup        = ~ grupo_sp$bod_dir,
    icon         = icon_pop,
    group        = "Agrupaci\u00f3n (direcciones BOD)",
    popupOptions = leaflet::popupOptions(maxHeight = 300, maxWidth = 500)
  )
  mapa_cluster <-   leaflet.extras::addPulseMarkers(
    map          = mapa_cluster,
    data         = grupo_sp,
    popup        = ~ grupo_sp$geo_dir,
    icon         = icon_pop,
    group        = "Agrupaci\u00f3n (direcciones GEO)",
    popupOptions = leaflet::popupOptions(maxHeight = 300, maxWidth = 500)
  )
  mapa_cluster <- leaflet::addTiles(
    map         = mapa_cluster,
    urlTemplate = "http://tile.openstreetmap.org/{z}/{x}/{y}.png",
    group       = "Callejero (OSM)",
    attribution = '<a href="https://www.openstreetmap.org/" title="OpenStreetMap">&copy; OpenStreetMap (OSM)'
  )
  mapa_cluster <- leaflet::addTiles(
    map         = mapa_cluster,
    urlTemplate = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}",
    group       = "Callejero (Google)",
    attribution = '<a href="https://www.google.com/maps" title="GoogleMaps">&copy; Google Maps'
  )
  mapa_cluster <- leaflet::addTiles(
    map         = mapa_cluster,
    urlTemplate = "http://www.google.es/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}",
    group       = "Sat\u00e9lite",
    attribution = '<a href="https://www.google.com/maps" title="GoogleSatellite">&copy; Google Satellite'
  )

  mapa_cluster <- leaflet::addLayersControl(
    map           = mapa_cluster,
    baseGroups    = c("Callejero (OSM)", "Callejero (Google)", "Sat\u00e9lite"),
    overlayGroups = c("Secciones INE 2011", "Agrupaci\u00f3n (direcciones BOD)", "Agrupaci\u00f3n (direcciones GEO)")
  )
  mapa_cluster <- leaflet::addMiniMap(map = mapa_cluster)
  mapa_cluster <- leaflet::addMeasure(
    map               = mapa_cluster,
    position          = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit   = "sqmeters",
    activeColor       = "#3D535D",
    completedColor    = "#7D4479"
  )
  mapa_cluster <- leaflet::hideGroup(map = mapa_cluster, "Agrupaci\u00f3n (direcciones GEO)")


  return(mapa_cluster)
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
    "tipo_reg", "tramo_por", "tvias", "tmp", "final", "distan_T", "dista",
    "umbral", "umbral_T", "incluido", "N", "geo_dir", "pr", "tr", "prob", "lng", "lat")
)
