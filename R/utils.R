
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
#' @usage detecta_cambios(datos, years = c(1996, 2001, 2004:2015), cod_postal =
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
detecta_cambios <- function(datos, years = c(1996, 2001, 2004:2015),
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
    cambios$viviendas <- viviendas_def$n_viv
    cambios$tramo_por <- tramos_cat
    setcolorder(cambios, c(1:4, 6:7, 5))
  }

  return(cambios)
}


#' @title Carga los datos privados del proyecto MEDEA3
#'
#' @description Algunos datos del proyecto MEDEA3 están encriptados para poder
#'   cumplir con la licencia INE (poblaciones desde 1998 a 2003 o datos de los
#'   censos 2001 y 2011). Esta función los desencripta y añade a los datos
#'   públicos (resto de años).
#'
#' @details La contraseña no se almacena en el historial.
#'
#' @param key Cadena de carácteres con la contraseña.
#' @param tipo Tipo de datos a cargar: población o censo.
#'
#' @return Un \code{data.frame} con los datos solicitados.
#'
#' @usage carga_datos(key, tipo = c("poblacion", "censo"))
#'
#' @seealso \code{\link{poblacion}} y \code{\link{descarga_poblaciones}}.
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#'   # Carga de datos de población
#'   carga_datos(key = "contraseña", tipo = "poblacion")
#' }
#'
#' @encoding UTF-8
#'
#' @export
carga_datos <- function(key, tipo = c("poblacion", "censo")) {

  stopifnot(is.character(key))
  tipo <- match.arg(tipo)
  key  <- sodium::sha256(charToRaw(key))
  if (tipo == "poblacion") {
    cifrado <- system.file("data_encrypted", "poblacion.rds",
                           package = "medear", mustWork = TRUE)
    cifrado <- unserialize(
      sodium::data_decrypt(readRDS(cifrado), key)
    )
    utils::data("poblacion", envir = environment(), package = "medear")
    datos <- data.table::rbindlist(
      list(poblacion, cifrado), fill = TRUE
    )[order(year, sexo, seccion)]
    attributes(datos)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
    class(datos) <- c(class(datos), "poblaciones_ine")
  } else {
    cifrado <- system.file("data_encrypted", "censo.rds",
                           package = "medear", mustWork = TRUE)
    datos <- unserialize(
      sodium::data_decrypt(readRDS(cifrado), key)
    )
    attributes(datos)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
    class(datos) <- c(class(datos), "censos_ine")
  }
  return(datos)

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
    res[, q_85_plus := NULL][]
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


llama_google <- function(map_url, api.args, tries) {
  withRestarts(
    tryCatch(
      suppressWarnings(
        suppressMessages(
          httr::GET(map_url, query = api.args)
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
      llama_google(map_url, api.args, tries - 1)
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
  pb1             <- utils::txtProgressBar(min = 0, max = max(1, length(res)), style = 3)

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
  pb      <- utils::txtProgressBar(min = 0, max = max(1, length(tramero_cambios)), style = 3)
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
#'   fallecimientos en relación a la media de sus vecinos más próximos. Con esta
#'   dinámica es fácil identificar los centros residenciales o errores
#'   sistemáticos en la geocodificación, donde es de esperar una mayor
#'   aglomeración de defunciones.
#'
#' @usage detecta_cluster(datos, epsg = 4326, vecinos = 10, cartografia = NULL,
#'   secciones = NULL, limite = c(1e-10, 1e-15, 1e-20), devuelve_datos = TRUE)
#'
#' @param datos Base de datos con las coordenadas que ubican cada uno de los
#'   fallecimientos. Debe contener, al menos, 3 columnas: \code{BOD.direccion},
#'   \code{lat} y \code{lng}, las cuales deben tener exactamente esos nombres.
#'   De forma paralela la función tratará de buscar otras seis columnas fruto
#'   del protocolo de geocodificación (\code{province}, \code{muni},
#'   \code{tip_via}, \code{address}, \code{portalNumber} y \code{postalCode})
#'   (nuevamente deben tener exactamente esos nombres, que son los que resultan
#'   del protocolo de geocodificación), con el propósito de mostrar, junto a las
#'   direcciones del BOD, las direcciones devueltas por los servicios de
#'   geocodificado. Si la base de datos tuviera otros nombres, seria trabajo del
#'   usuario cambiárselos como paso previo al uso de la función.
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
#' @param secciones Cadena de caracteres: secciones censales anómalas
#'   (información facilitada por el nodo coordinador), para resaltarlas en el
#'   mapa. Por defecto ninguna.
#' @param limite Numérico con longitud 1 <= x <= 9: límites de probabilidad con
#'   los que se identifican las agrupaciones sospechosas. Por defecto viene
#'   fijado a 1e-10, 1e-15 y 1e-20.
#' @param devuelve_datos Valor lógico, por defecto TRUE. ¿La función debe
#'   devolver un \code{data.frame} con los datos identificativos de los clústeres?
#'
#' @details La función comienza calculando el número de fallecimientos en cada
#'   par único de coordenadas, identifica los \emph{n} vecinos más próximos a
#'   cada punto y calcula la media de fallecimientos en los mismos. Considerando
#'   que los fallecimientos en cada par de coordenadas siguen una distribución
#'   de Poisson cuya media es la media de los fallecimientos en los puntos más
#'   cercanos, se calcula la probabilidad de superar esa media.
#'
#' @return Representa un objeto de clase \code{\link[leaflet]{leaflet}} en el
#'   que se marcan los puntos a revisar. Por defecto los puntos se dividen en
#'   tres colores en función de la mortalidad acontecida en las coordenadas
#'   vecinas: verde si la probabilidad de obtener ese resultado es inferior a
#'   1e-10, azul si es inferior a 1e-15 y rojo si es inferior a 1e-20 (el caso
#'   más evidente). Haciendo clic en cada uno de los puntos se puede consultar
#'   las direcciones, tanto del BOD como las obtenidas en la geocodificación (se
#'   puede cambiar entre una y otra en el menú del extremo superior derecho),
#'   asociadas a cada punto. Al comienzo de cada dirección se indica el número
#'   de fila al que hace referencia cada dirección en los datos que se
#'   facilitaron, de forma que pueda recuperarse fácilmente dicha información
#'   para explorar los datos en profundidad.
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
#'   La función también devuelve los datos con las cooredenadas (campos
#'   \code{lng} y \code{lat}), número de defunciones (campo \code{N}), límite
#'   dentro del cual recae (campo \code{limite}, útil para filtrar resultados) y
#'   direcciones asociadas a cada punto (campo \code{detalle}, de tipo lista
#'   donde cada elemento contiene un data.frame con la información detallada).
#'
#' @examples
#' \dontrun{
#'   library(medear)
#'   revisar <- detecta_cluster(datosmort)
#'   revisar
#' }
#'
#' @encoding UTF-8
#'
#' @export
detecta_cluster <- function(datos, epsg = 4326, vecinos = 10, cartografia = NULL,
                            secciones = NULL, limite = c(1e-10, 1e-15, 1e-20),
                            devuelve_datos = TRUE) {

  vars_ob <- c("BOD.direccion", "lng", "lat")
  vars_op <- c("province", "muni", "tip_via", "address", "portalNumber", "postalCode")
  if (!all(vars_ob %in% names(datos))) {
    stop("\nAlguna de las variables necesarias est\u00e1n ausentes en los datos o ",
         "requieren un cambio de nombre.\nPor favor, revise la documentaci\u00f3n.")
  }
  stopifnot(is.numeric(vecinos) & length(vecinos) == 1)
  stopifnot(is.numeric(epsg) & length(epsg) == 1)
  stopifnot(is.numeric(limite) & length(limite) >= 1 & length(limite) <= 9)
  if (!is.null(secciones))
    stopifnot(is.character(secciones))
  if (!is.null(cartografia)) {
    if (!"SpatialPolygonsDataFrame" %in% class(cartografia)) {
      stop("\nEl objeto 'cartografia' debe ser un 'SpatialPolygonsDataFrame'")
    }
    if (is.na(sp::proj4string(cartografia))) {
      stop("\nEl objeto 'cartografia' no tiene asignado un CRS.")
    }
  } else {
    utils::data("cartografia", envir = environment(), package = "medear")
  }
  cartografia <- sp::spTransform(cartografia, sp::CRS(paste0("+init=epsg:", epsg)))
  datos_c     <- copy(as.data.table(datos))
  limite      <- sort(limite, decreasing = TRUE)

  carto_cl <- cartografia
  datos_c$id_n <- seq_len(nrow(datos_c))
  if (!any(vars_op %in% names(datos_c))) {
    datos_c[, `:=`(
      tip_via      = NA_character_,
      address      = NA_character_,
      portalNumber = NA_character_,
      muni         = NA_character_,
      province     = NA_character_,
      postalCode   = NA_character_
    )]
  }
  datos_c[, geo_dir := paste0(tip_via, " ", address, " ", portalNumber, ", ", muni, ", ", province, ", ", postalCode)]
  if (is.data.table(datos_c)) {
    bdd <- datos_c[stats::complete.cases(datos_c[, vars_ob[2:3], with = FALSE])]
  } else {
    datos_c <- as.data.table(datos_c)
    bdd   <- datos_c[stats::complete.cases(datos_c[, vars_ob[2:3]])]
  }

  setkeyv(bdd, vars_ob[2:3])
  grupo  <- bdd[, c(vars_ob[2:3]), with = FALSE][, .N, by = c(vars_ob[2:3])]
  for (i in seq_along(vars_ob[2:3])) {
    set(grupo, j = vars_ob[2:3][i], value = as.numeric(grupo[[vars_ob[2:3][i]]]))
  }
  grupo_sp                  <- copy(grupo)
  sp::coordinates(grupo_sp) <- stats::as.formula(paste("~", paste(vars_ob[2:3], collapse = " + ")))
  sp::proj4string(grupo_sp) <- sp::CRS(paste0("+init=epsg:", epsg))
  knn10 <- nabor::knn(sp::coordinates(grupo_sp), k = vecinos + 1)[[1]][, -1]
  grupo[, pr := .(lapply(seq_len(nrow(knn10)), function(x) knn10[x, ]))]
  grupo[, tr := sapply(pr, function(x) as.integer(round(mean(grupo[x, N]))))]
  grupo[, pr := NULL]
  grupo[, prob := mapply(function(x, y) stats::ppois(x, y, lower.tail = FALSE), N, tr)]

  grupo_sp$bod_dir <- grupo_sp$geo_dir <- NA_character_
  grupo_sp$limite <- factor(NA, levels = seq_along(limite), labels = paste(limite))
  for (i in seq_along(limite)) {
    grupo_sp$limite[grupo$prob < limite[i]] <- paste(limite[i])
  }
  grupo_sp                <- grupo_sp[grupo$prob < limite[1], ]
  grupo_sp$id <- seq_len(nrow(grupo_sp))
  rownames(grupo_sp@data) <- seq_len(nrow(grupo_sp))
  coord_grupo             <- sp::coordinates(grupo_sp)
  pegote                  <- vector("list", nrow(coord_grupo))
  for (i in seq_len(nrow(coord_grupo))) {
    pegote[[i]] <- datos_c[
      lng == coord_grupo[i, "lng"] & lat == coord_grupo[i, "lat"],
      c("geo_dir", "BOD.direccion", "id_n")
    ]
    grupo_sp$geo_dir[i] <- paste0(
      "<center><h3>Punto ", grupo_sp$id[i], " (N = ", nrow(pegote[[i]]), ")</h3></center>",
      paste("<p>", "<b>", pegote[[i]][[3]], ".- </b>", "<i>",
            pegote[[i]][[1]], "</i>", "</p>", collapse = "")
    )
    grupo_sp$bod_dir[i] <- paste0(
      "<center><h3>Punto ", grupo_sp$id[i], " (N = ", nrow(pegote[[i]]), ")</h3></center>",
      paste0("<p>", "<b>", pegote[[i]][[3]], ".- </b>", "<i>",
             pegote[[i]][[2]], "</i>", "</p>", collapse = "")
    )
  }

  xx       <- suppressWarnings(rgeos::gWithin(grupo_sp, carto_cl, byid = T))
  yy       <- apply(xx, 1, sum)
  zz       <- unique(carto_cl$CUMUN[which(yy != 0)])
  carto_cl <- carto_cl[carto_cl$CUMUN %in% zz, ]

  paleta       <- leaflet::colorFactor(
    palette = "Set1", domain = grupo_sp$limite, ordered = TRUE, reverse = TRUE
  )
  icon_pop     <- leaflet.extras::pulseIcons(
    color = paleta(grupo_sp$limite), iconSize = 5, heartbeat = .5
  )
  mapa_cluster <- leaflet::leaflet()
  mapa_cluster <- leaflet::addPolygons(
    map              = mapa_cluster,
    data             = carto_cl,
    popup            = paste0("Secci\u00f3n: ", carto_cl$seccion),
    color            = "black",
    weight           = 1,
    smoothFactor     = .5,
    opacity          = .7,
    fillOpacity      = 0,
    highlightOptions = leaflet::highlightOptions(color = "white", weight = 1.5, bringToFront = TRUE),
    group            = "Seccionado de referencia"
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
  mapa_cluster <- leaflet::addProviderTiles(
    map         = mapa_cluster,
    provider    = "CartoDB",
    group       = "Callejero"
  )
  mapa_cluster <- leaflet::addLayersControl(
    map           = mapa_cluster,
    baseGroups    = c("Callejero", "Callejero (OSM)", "Callejero (Google)", "Sat\u00e9lite"),
    overlayGroups = c("Seccionado de referencia", "Agrupaci\u00f3n (direcciones BOD)", "Agrupaci\u00f3n (direcciones GEO)")
  )
  mapa_cluster <- leaflet::addMeasure(
    map               = mapa_cluster,
    position          = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit   = "sqmeters",
    activeColor       = "#3D535D",
    completedColor    = "#7D4479"
  )
  mapa_cluster <- leaflet::hideGroup(map = mapa_cluster, "Agrupaci\u00f3n (direcciones GEO)")
  mapa_cluster <- leaflet::addLegend(
    map       = mapa_cluster,
    position  = "bottomright",
    pal       = paleta,
    values    = grupo_sp$limite,
    labFormat = leaflet::labelFormat(prefix = "<="),
    title     = "Pr(Esp >= Obs)",
    opacity   = 1
  )
  if (!is.null(secciones)) {
    carto_raras  <- carto_cl[carto_cl$seccion %in% secciones, ]
    mapa_cluster <- leaflet::addPolygons(
      map              = mapa_cluster,
      data             = carto_raras,
      popup            = paste0("Secci\u00f3n: ", carto_raras$seccion),
      color            = "black",
      fillColor        = "#F03B20",
      weight           = 1.5,
      smoothFactor     = 0.5,
      opacity          = 0.5,
      fillOpacity      = 0.3,
      highlightOptions = leaflet::highlightOptions(
        color        = "white",
        weight       = 2,
        bringToFront = TRUE
      ),
      group            = "Seccionado de referencia"
    )
  }

  if (devuelve_datos) {
    print(mapa_cluster)
    grupo_sp         <- as.data.table(grupo_sp)
    grupo_sp$detalle <- pegote
    grupo_sp         <- grupo_sp[, c("lng", "lat", "N", "limite", "detalle")]
    return(grupo_sp[])
  } else {
    mapa_cluster
  }
}


#' @title Comprobacion de asignacion de distintas coordenadas a misma direccion
#'
#' @description Aunque no es frecuente, en algunas situaciones, y frente a una
#'   misma dirección, se obtienen distintas coordenadas. Esta función detecta
#'   estos casos para que pueda volver a lanzarse el algoritmo de geocodificado
#'   sobre ellos. Concretamente se modifican estos registros de los datos de
#'   mortalidad, sustituyendo los valores devueltos por el servicio de
#'   geocodificado por \code{NA}'s, y fijando
#'   \code{georef == "repetir_geo_google"}.
#'
#' @param mortalidad Datos con la mortalidad geocodificada. Objeto de clase
#'   \code{data.frame} que contenga, al menos, la siguiente información (los
#'   nombres que deben tener las variables están entre paréntesis): longitud
#'   (\code{lng}), latitud (\code{lat}), tipo de vía (\code{tip_via}),
#'   dirección (\code{address}), número de portal (\code{portalNumber}),
#'   municipio (\code{muni}) y provincia (\code{province}) devueltos por el
#'   servicio de geocodificado.
#'
#' @usage comprueba_geocodificado(mortalidad)
#'
#' @return Devuelve un \code{data.frame} con la mortalidad y los registros
#'   problemáticos modificados a \code{NA}, fijando el campo \code{georef} de
#'   esos registros al valor \code{repetir_geo_google}.
#'
#' @encoding UTF-8
#'
#' @export
#'
comprueba_geocodificado <- function(mortalidad) {

  vars <- c("lat", "lng", "portalNumber", "muni", "province", "address", "tip_via")
  if (!all(vars[1:2] %in% names(mortalidad))) {
    stop("\nEn los datos de mortalidad no est\u00e1n presentes las variables ",
         "'lng' y 'lat', o tienen otro nombre.\nPor favor, revise los datos ",
         "y vuelva a ejecutar la funci\u00f3n.")
  }
  if (!all(vars[-c(1:2)] %in% names(mortalidad))) {
    stop("\nAlgunas de las variables necesarias no est\u00e1n presentes en los ",
         "datos proporcionados.\nPor favor, revise los datos de mortalidad y ",
         "aseg\u00farese de que las variables 'address', 'portalNumber', ",
         "'muni', 'province' y 'tip_via' est\u00e1n presentes y tienen exactamente esos ",
         "nombres (todas ellas se crean tras aplicar el algoritmo de geocodificado).")
  }

  mortalidad_1 <- copy(as.data.table(mortalidad))[, id_mort := as.integer(seq_len(.N))]
  mortalidad_c <- mortalidad_1[!is.na(lng) & !is.na(lat)]
  not_in_dt    <- fsetdiff(mortalidad_1, mortalidad_c)
  mortalidad_c[, direccion := paste0(tip_via, " ", address, " ", portalNumber, ", ", muni, ", ", province)]
  setindexv(mortalidad_c, c("direccion", "lng", "lat"))
  repetir_dir <- mortalidad_c[
    !grep("google", georef, ignore.case = TRUE),
    length(unique(lng)),
    by = direccion
  ][V1 != 1][, direccion]
  repetir_ind <- which(mortalidad_c$direccion %in% repetir_dir)
  mortalidad_c$direccion <- NULL

  if (length(repetir_ind) != 0) {
    mortalidad_c$georef[repetir_ind] <- "repetir_geo_google"
    for (j in seq_along(vars)) {
      set(mortalidad_c, repetir_ind, vars[j], NA_character_)
    }
    mortalidad_c <- rbindlist(list(mortalidad_c, not_in_dt))[order(id_mort)][, id_mort := NULL][]

    return(mortalidad_c)
  } else {
    message("\nNo hay coordenadas discordantes ante una misma direcci\u00f3n.")
  }
}


calcula_edad <- function(datos) {
  datos$MESNAC[nchar(datos$MESNAC) == 1, ] <- paste0("0", datos$MESNAC[nchar(datos$MESNAC) == 1, ])
  datos$DIANAC[nchar(datos$DIANAC) == 1, ] <- paste0("0", datos$DIANAC[nchar(datos$DIANAC) == 1, ])
  datos$MESDEFUN[nchar(datos$MESDEFUN) == 1, ] <- paste0("0", datos$MESDEFUN[nchar(datos$MESDEFUN) == 1, ])
  datos$DIADEFUN[nchar(datos$DIADEFUN) == 1, ] <- paste0("0", datos$DIADEFUN[nchar(datos$DIADEFUN) == 1, ])
  datos$g_edad  <- paste0(datos$MESDEFUN, "-", datos$DIADEFUN) > paste0(datos$MESNAC, "-", datos$DIANAC)
  datos$edad <- as.integer(datos$ANODEFUN) - as.integer(datos$ANONAC) - ifelse(datos$g_edad, 0, 1)
  datos$EDAD <- edad
  datos$g_edad <- NULL
  return(datos)
}


#' @title Proyecta pares de coordenadas en un mapa
#'
#' @description Esta función proyecta las coordenadas (latitud y longitud) en
#'   un mapa, devolviendo la base de datos origina con una columna extra en la que
#'   se indica la sección censal en la que recae el punto.
#'
#' @param datos Objeto de clase \code{data.frame} con al menos dos columnas:
#'   lat (latitud) y lng (longitud).
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}}.
#' @param epsg Vector numérico de longitud uno con el código EPSG del sistema de
#'   referencia de coordenadas (CRS) empleado (por defecto se usa el 4326 con
#'   datum WGS84).
#'
#' @usage proyecta_secciones(datos, cartografia, epsg = 4326)
#'
#' @details Esta función es capaz de proyectar cualquier base de datos con
#'   información puntual (mortalidad o población) en la cartografía.
#'
#' @return Se devuelve el \code{data.frame} con la columna extra \code{seccion}.
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{une_secciones}}, \code{\link{crea_cubo_mortalidad}} y
#'   \code{\link{crea_cubo_poblacion}}.
#'
proyecta_secciones <- function(datos, cartografia, epsg = 4326) {

  if (!is.data.frame(datos)) {
    stop("\nLos datos deben ser de clase 'data.frame'.")
  }
  if (!all(c("lng", "lat") %in% names(datos))) {
    stop("\nEn los datos no est\u00e1n presentes las variables ",
         "'lng' y 'lat', o tienen otro nombre.\nPor favor, revise los datos ",
         "y vuelva a ejecutar la funci\u00f3n.")
  }
  comprueba_datos(cartografia, "cartografia")

  datos_c        <- copy(as.data.table(datos))
  names(datos_c) <- tolower(names(datos_c))
  datos_c        <- datos_c[!is.na(lng) & !is.na(lat)]

  for (i in seq_along(datos_c)) {
    set(datos_c, j = i, value = as.character(datos_c[[i]]))
  }
  cartografia <- sp::spTransform(cartografia, sp::CRS(paste0("+init=epsg:", epsg)))
  datos_c <- datos_c[, c("lng", "lat") := lapply(.SD, as.numeric), .SDcols = c("lng", "lat")]
  sp::coordinates(datos_c) <- ~ lng + lat
  sp::proj4string(datos_c) <- sp::CRS(paste0("+init=epsg:", epsg))
  datos_c         <- sp::spTransform(datos_c, sp::proj4string(cartografia))
  datos_c$seccion <- sp::over(datos_c, cartografia)$seccion

  return(as.data.table(datos_c)[])
}


#' @title Agrupa las causas de mortalidad
#'
#' @description Esta función realiza una agrupación de causas de mortalidad
#'   identificadas mediante códigos CIE 9 y CIE 10 en los 22 grupos usados en
#'   MEDEA (véase la viñeta sobre el formato de los datos en MEDEA3). De forma
#'   adicional, se pueden incorporar otras causas manualmente empleando el
#'   argumento \code{otras_causas}.
#'
#' @param datos Datos de mortalidad ajustados al formato requerido por el paquete.
#' @param medea3 ¿Se desea utilizar las 22 agrupaciones MEDEA3? Por defecto sí.
#' @param otras_causas Véctor de caracteres que indica los nombres de las
#'   columnas (columnas con valor 0-1) en la base de datos de mortalidad que
#'   identifican a dichas otras causas.
#'
#' @usage causas_defuncion(datos, medea3 = TRUE, otras_causas = NULL)
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link{une_secciones}} y \code{\link{crea_cubo_mortalidad}}.
#'
causas_defuncion <- function(datos, medea3 = TRUE, otras_causas = NULL) {

  stopifnot(is.logical(medea3))
  if (!is.null(otras_causas)) {
    stopifnot(is.character(otras_causas))
  }
  comprueba_datos(datos, "mortalidad")
  datos$causa_defuncion <- gsub("\\.|,|\\s", "",  datos$causa_defuncion)
  causas_def <- list()

  if (medea3) {
    causas_def[["01_sida"]]                    <- "^279[56]|^B2[0-4]"
    causas_def[["02_cancer_estomago"]]         <- "^151|^C16"
    causas_def[["03_cancer_colon"]]            <- "^153|^C18"
    causas_def[["04_cancer_recto"]]            <- "^154|^C(19|2[01])"
    causas_def[["05_cancer_colorectal"]]       <- "^153|^C18|^154|^C(19|2[01])"
    causas_def[["06_cancer_laringe"]]          <- "^161|^C32"
    causas_def[["07_cancer_pulmon"]]           <- "^162|^C3[3-4]"
    causas_def[["08_cancer_mama"]]             <- "^174|^C50"
    causas_def[["09_cancer_prostata"]]         <- "^185|^C61"
    causas_def[["10_cancer_vejiga"]]           <- "^188|^C67"
    causas_def[["11_cancer_hemato"]]           <- "^20[0-8]|^2733|^C(8[1-9]|9[0-6])"
    causas_def[["12_diabetes"]]                <- "^250|^E1[0-4]"
    causas_def[["13_ttos_mentales_organicos"]] <- "^290(?!1)|^F0[0-9]"
    causas_def[["14_alzheimer"]]               <- "^2901|^3310|^G30"
    causas_def[["15_demencia"]]                <- "^F0[0-9]|^290|3310|^G30"
    causas_def[["16_isquemica_corazon"]]       <- "^41[0-4]|^I2[0-5]"
    causas_def[["17_ictus"]]                   <- "^43[0-46-8]|^I6[0-9]"
    causas_def[["18_epoc"]]                    <- "^49[0-24-6]|^J4[0-47]"
    causas_def[["19_cirrosis"]]                <- "^571|^K7[034]|^K721|^K76[19]"
    causas_def[["20_suicidios"]]               <- "^E95[0-9]|^X([6-7][0-9]|8[0-4])"
    causas_def[["21_accidente_trafico"]]       <- paste0(
      "^E81[0-9]|^V(0[2-4][19]|09[23]|1[2-4][3-59]|[1-7]9[4-69]|2[0-8][3-59]|",
      "[3-7][0-8][4-79]|80[3-5]|8[12]1|8[3-6][0-3]|87[0-8]|89[29])"
    )
    causas_def[["22_todas_causas"]]            <- "[[:alnum:]]"
    causas_def <- lapply(causas_def, grep, x = datos$causa_defuncion, ignore.case = TRUE, perl = TRUE)
  }

  if (!is.null(otras_causas)) {
    for (i in seq_along(otras_causas)) {
      causas_def[[otras_causas[i]]] <-  which(datos[[otras_causas[i]]] != 0)
    }
  }

  if (!medea3 & is.null(otras_causas)) {
    warning("Se fij\u00f3 medea3 = FALSE y otras_causas = NULL: no se hace nada",
            " (se devuelven las mismas causas).")
  } else {
    return(causas_def)
  }
}


#' @title Comprobaciones de la clase de los datos
#'
#' @description Función para comprobar si los datos se ajustan al formato requerido.
#'
#' @param datos Datos sobre los que lanzar comprobaciones.
#' @param tipo Tipo de datos a comprobar.
#' @param periodo Período de estudio: solo aplicable en \code{tipo == "mortalidad"}
#'   o \code{tipo == "poblacion"}.
#'
#' @usage comprueba_datos(datos, tipo = c("cartografia", "mortalidad",
#' "poblacion"), periodo = NULL)
#'
#' @encoding UTF-8
#'
comprueba_datos <- function(datos, tipo = c("cartografia", "mortalidad", "poblacion"), periodo = NULL) {

  tipo <- match.arg(tipo)

  if (tipo == "cartografia") {
    if ("SpatialPolygonsDataFrame" != class(datos))
      stop("La cartograf\u00eda debe ser de clase 'SpatialPolygonsDataFrame'.")
    if (is.na(sp::proj4string(datos)))
      stop("\nLa cartograf\u00eda no tiene asignada una proyecci\u00f3n.")
  } else if (tipo == "mortalidad") {
    if (!is.data.frame(datos)) {
      stop("\nEn los datos de mortalidad deben ser de clase 'data.frame'.")
    }

    if (!all(c("lng", "lat") %in% names(datos))) {
      stop("\nEn los datos de mortalidad no est\u00e1n presentes las variables ",
           "'lng' y 'lat', o tienen otro nombre.\nPor favor, revise los datos ",
           "y vuelva a ejecutar la funci\u00f3n.")
    }
    mort_vars <- c("sexo", "year_defuncion", "edad", "causa_defuncion")
    if (!all(mort_vars %in% names(datos))) {
      stop("\nAlgunas de las variables necesarias para calcular el cubo de ",
           "mortalidad no est\u00e1n presentes en los datos proporcionados.\n",
           "Por favor, revise los datos de mortalidad y aseg\u00farese de que las",
           " variables 'sexo', 'year_defuncion', 'edad' y 'causa_defuncion' est\u00e1n ",
           "presentes y tienen exactamente esos nombres.")
    }
    if (!all(nchar(datos$causa_defuncion) >= 3)) {
      stop("\nTodas las causas de mortalidad (sin importar si se codificaron ",
           "siguiendo CIE-9 o CIE-10) deben tener un m\u00ednimo de tres caracteres.",
           "\nPor favor, revise que este aspecto se cumple en su base de datos.")
    }
    if (!all(unique(datos$sexo) %in% 0:1)) {
      stop("\nLa variable sexo debe codificarse como 0 (masculino) o 1 (femenino).",
           "\nRevise la base de datos proporcionada")
    }
    if (!all(periodo %in% unique(datos$year_defuncion))) {
      stop("\nNo hay datos de mortalidad para todos los a\u00f1os marcados en el ",
           "argumento 'periodo'.")
    }
  } else {
    if (!is.data.frame(datos)) {
      stop("\nEn los datos de  poblaci\u00f3n deben ser de clase 'data.frame'.")
    }
    pob_vars <- c(
      "seccion",
      "sexo",
      "year",
      paste("q", seq(0, 99, 5), seq(4, 100, 5), sep = "_"),
      "q_100_plus",
      "q_85_plus"
    )
    if (!all(names(datos) %in% pob_vars)) {
      stop("\nAlgunas de las variables de los datos de poblaci\u00f3n est\u00e1n ausentes",
           " o reciben un nombre no est\u00e1ndar.\n\nLos posibles nombres son: ",
           paste(pob_vars, collapse = ', '),
           ".\nAdapte los datos al formato correcto antes de ejecutar la funci\u00f3n")
    }
    if (!all(unique(datos$sexo) %in% 0:1)) {
      stop("\nLa variable sexo debe codificarse como 0 (masculino) o 1 (femenino).",
           "\nRevise la base de datos proporcionada")
    }
    if (!all(periodo %in% unique(datos$year))) {
      stop("\nNo hay datos de poblaci\u00f3n para todos los a\u00f1os marcados en el ",
           "argumento 'periodo'.")
    }
  }
}


#' @title Crear la matriz 5-dimensional de mortalidad
#'
#' @description Esta función crea la matriz 5-dimensional de mortalidad (año de
#'   defunción, sexo, grupo de edad quinquenal, sección censal y causa de
#'   defunción).
#'
#' @param datos Objeto de clase \code{data.frame} con la mortalidad
#'   geocodificada. Este objeto debe contener las variables \code{sexo},
#'   \code{year_defuncion}, \code{edad}, \code{causa_defuncion} \code{lng} y
#'   \code{lat}, con exactamente esos nombres.
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}} con
#'   proyección asignada (código EPSG).
#' @param epsg Vector numérico de longitud uno con el código EPSG del sistema de
#'   referencia de coordenadas (CRS) empleado en los datos de mortalidad (por
#'   defecto se usa el 4326 con datum WGS84).
#' @param medea3 ¿Se desea utilizar las 22 agrupaciones MEDEA3? Por defecto sí.
#' @param otras_causas Véctor de caracteres que indica los nombres de las
#'   columnas (columnas con valor 0-1) en la base de datos de mortalidad que
#'   identifican a dichas otras causas.
#' @param corte_edad Numérico: punto de corte para los grupos de edad (85 o
#'   100). Argumento opcional en caso de proporcionar datos de poblaciones o
#'   mortalidad.
#' @param periodo Vector numérico de longitud >= 1 con los años para los que se
#'   desee construir el \code{array} de mortalidad.
#'
#' @usage crea_cubo_mortalidad(datos, cartografia, epsg = 4326, medea3 = TRUE,
#'   otras_causas = NULL, corte_edad = 85, periodo = 1996:2015)
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link{une_secciones}} y \code{\link{proyecta_secciones}}.
#'
#' @export
crea_cubo_mortalidad <- function(datos, cartografia, epsg = 4326, medea3 = TRUE,
                                 otras_causas = NULL, corte_edad = 85,
                                 periodo = 1996:2015) {

  comprueba_datos(datos, "mortalidad")
  comprueba_datos(cartografia, "cartografia")
  stopifnot(corte_edad %in% c(85, 100))
  periodo    <- sort(periodo)
  datos_c    <- proyecta_secciones(datos, cartografia, epsg)
  datos_c    <- datos_c[!is.na(datos_c$seccion), ]
  datos_c    <- datos_c[between(year_defuncion, first(periodo), last(periodo))]
  causas_def <- causas_defuncion(datos_c, medea3, otras_causas)

  if (corte_edad == 85) {
    grupo_edad <- c(paste("q", seq(0, 84, 5), seq(4, 85, 5), sep = "_"), "q_85_plus")
  } else {
    grupo_edad <-  c(paste("q", seq(0, 99, 5), seq(4, 100, 5), sep = "_"), "q_100_plus")
  }

  datos_c$edad <- as.numeric(datos_c$edad)
  datos_c$edad <- cut(
    datos_c$edad, c(-1, (5 * seq_along(grupo_edad[-length(grupo_edad)])) - 1, 125)
  )
  datos_c$edad <- factor(
    datos_c$edad, levels = levels(datos_c$edad), labels = grupo_edad
  )
  datos_c$seccion        <- factor(
    datos_c$seccion,
    levels = sort(unique(cartografia$seccion)),
    labels = sort(unique(cartografia$seccion))
  )
  datos_c$year_defuncion <- factor(
    datos_c$year_defuncion, levels = periodo, labels = periodo
  )
  datos_c$sexo           <- factor(datos_c$sexo)

  mort_array <- array(
    dim      = c(length(periodo), 2, length(grupo_edad),
                 length(unique(datos_c$seccion)), length(causas_def)),
    dimnames = list(
      paste(periodo),
      levels(datos_c$sexo),
      grupo_edad,
      sort(unique(cartografia$seccion)),
      names(causas_def)
    )
  )
  for (i in seq_along(causas_def)) {
    mort_array[ , , , , i] <- table(
      datos_c$year_defuncion[causas_def[[i]]],
      datos_c$sexo[causas_def[[i]]],
      datos_c$edad[causas_def[[i]]],
      datos_c$seccion[causas_def[[i]]]
    )
  }

  return(mort_array)
}


#' @title Crear la matriz 4-dimensional de poblaciones
#'
#' @description Esta función crea la matriz 4-dimensional con las poblaciones
#'   (año, sexo, grupo de edad quinquenal y sección censal).
#'
#' @param datos Objeto de clase \code{data.frame} con las cifras de población.
#'   Este objeto debe contener las variables \code{seccion},
#'   \code{sexo}, \code{year} y los grupos quinquenales desde \code{q_0_4}
#'   hasta \code{q_85_plus} o \code{q_100_plus}, con exactamente esos nombres.
#' @param corte_edad Numérico: punto de corte para los grupos de edad (85 o
#'   100). Argumento opcional en caso de proporcionar datos de poblaciones o
#'   mortalidad.
#' @param periodo Vector numérico de longitud >= 1 con los años para los que se
#'   desee construir el \code{array} de mortalidad.
#' @param datos_propios Valor lógico. Indica si se dispone de datos de población
#'   a nivel puntual. Por defecto tiene tiene el valor \code{FALSE}. En caso
#'   contrario la función devolvería el \code{array} de poblaciones pero con
#'   \code{NA} en todos los conteos, con lo que el usuario será el encargado de
#'   rellenar este \code{array} posteriormente.
#'
#' @usage crea_cubo_poblacion(datos, corte_edad = 85, periodo = 1996:2015,
#'   datos_propios = FALSE)
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link{une_secciones}} y \code{\link{crea_cubo_mortalidad}}.
#'
#' @export
crea_cubo_poblacion <- function(datos, corte_edad = 85, periodo = 1996:2015,
                                datos_propios = FALSE) {

  comprueba_datos(datos, "poblacion")
  periodo       <- sort(unique(periodo))
  datos_c       <- as.data.table(datos)[between(year, first(periodo), last(periodo))]

  if (length(grep("_plus", names(datos_c))) == 2) {
    if (corte_edad == 85) {
      col_eliminar <- names(datos_c)[grep("9\\d|89|100", names(datos_c))]
      if (length(col_eliminar) == 4) {
        datos_c <- elige_corte(datos_c, corte_edad)
      } else {
        if (length(col_eliminar) > 0) {
          for (i in seq_along(col_eliminar)) {
            set(datos_c, j = col_eliminar[i], value = NULL)
          }
        }
      }
    } else {
      col_eliminar <- names(datos_c)[grep("85_plus", names(datos_c))]
      if (length(col_eliminar) == 1) {
        datos_c <- elige_corte(datos_c, corte_edad)
      }
    }
  }
  name_dim_pob <- list(
    periodo,
    unique(datos_c$sexo),
    names(datos_c[, -c("seccion", "sexo", "year")]),
    sort(unique(datos_c$seccion))
  )
  pob_array <- array(dim = sapply(name_dim_pob, length), dimnames = name_dim_pob)

  if (datos_propios) {
    pob_array[, , , ] <- NA_integer_
  } else {
    if (!all(periodo %in% unique(datos_c$year))) {
      stop("\nNo hay datos de poblaci\u00f3n para todos los a\u00f1os marcados en el ",
           "argumento 'periodo'.")
    }
    for (i in seq_along(name_dim_pob[[4]])) {
      pob_array[, , , name_dim_pob[[4]][i]] <- unlist(
        datos_c[seccion == name_dim_pob[[4]][i], -c("seccion", "sexo", "year")]
      )
    }
  }

  return(pob_array)
}


#' @title Eliminar cambios manualmente de la base de datos cambios_seccion
#'
#' @description Esta función elimina los cambios especificados del listado de
#'   cambios de sección detectados.
#'
#' @param datos Objeto de clase \code{cambios_ine} y \code{data.frame} con los
#'   cambios de seccionado.
#' @param sc_ref Cadena de caracteres de longitud >= 1 con las secciones de
#'   2011 a eliminar.
#' @param sc_new Cadena de caracteres de longitud >= 1 con las secciones del
#'   resto de años a eliminar.
#'
#' @details Los argumentos sc_ref y sc_new deben hacer referencia a una misma
#'   fila de la base de datos.
#'
#' @return Un \code{data.frame} sin los cambios seleccionados.
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link{detecta_cambios}} y \code{\link{une_secciones}}.
#'
#' @export
elimina_cambios <- function(datos, sc_ref, sc_new) {
  datos_c  <- copy(as.data.table(datos))
  eliminar <- mapply(
    function(x, y) which(datos_c$sc_ref == x & datos_c$sc_new == y),
    sc_ref,
    sc_new,
    SIMPLIFY = FALSE
  )
  eliminar <- unique(unlist(eliminar))
  if (length(eliminar) > 0) {
    return(datos_c[-eliminar, ][])
  } else {
    return(datos_c[])
  }
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
    "umbral", "umbral_T", "incluido", "N", "geo_dir", "pr", "tr", "prob", "lng",
    "lat", "g_edad", "edad", "year_defuncion", "causa_defuncion", "address",
    "direccion", "georef", "id_mort", "poblacion", "..vars_out", "..denom")
)
