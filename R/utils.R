
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


#' @title Función para detectar cambios de seccionado en trameros
#'
#' @description Detecta cambios de sección censal para las provincias y el
#'   período marcados.
#'
#' @param datos Objeto de clase \code{tramero_ine}.
#' @param years Vector numérico de longitud >= 2 con los años para los que se
#'   desee consultar las variaciones de seccionado.
#'
#' @usage detecta_cambios(datos, years = 2004:2017)
#'
#' @details El tiempo de ejecución de la función varía según el número de
#'   provincias y el rango de años. La forma más sencilla de acelerar el proceso
#'   de computación es mediante la ejecución en paralelo de la función.
#'
#'   Los códigos de sección censal siguen un orden preestablecido: los primeros
#'   dos dígitos identifican la provincia, los siguientes tres dígitos el
#'   municipio, los próximos dos dígitos el distrito y los últimos tres
#'   dígitos hacen referencia a la sección censal.
#'
#' @return Un objeto de clase \code{cambios_ine} con 4 columnas:
#'   \item{sc_old}{Código de la sección censal en el primer año.}
#'   \item{sc_new}{Código de la sección censal en el segundo año.}
#'   \item{year}{Primer año.}
#'   \item{year}{Segundo año.}
#'
#' @examples
#' \dontrun{
#' trameros <- descarga_trameros(cod_provincia = c("51", "52"))
#' cambios  <- detecta_cambios(datos = trameros)
#' cambios
#' }
#'
#' @encoding UTF-8
#'
#' @export
detecta_cambios <- function(datos, years = 2004:2017) {

  stopifnot("tramero_ine" %in% class(datos))
  stopifnot(is.numeric(years))
  stopifnot(length(years) > 1 & years %in% 2001:2017)
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


#' @title Carga los datos privados de población para el proyecto MEDEA3
#'
#' @description Algunos datos del proyecto MEDEA3 están encriptados para poder
#'   cumplir con la licencia INE (poblaciones desde 1998 a 2003). Esta función
#'   los desencripta y los adjunta al entorno global.
#'
#'   CUIDADO: se sobreescribirá cualquier objeto nombrado como poblacion.
#'
#' @details La contraseña no se almacena en el historial.
#'
#' @param key Cadena de caracteres con la contraseña.
#' @return No se devuelve nada.
#'
#' @usage carga_datos(key)
#'
#' @seealso poblacion
#'
#' @keywords datasets
#'
#' @examples
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
  poblacion <- data.table::rbindlist(list(poblacion, cifrado), fill = TRUE)[order(year, sexo, seccion)]
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
          q_85_plus, q_85_89, q_90_94, q_95_99, q_100_plus , na.rm = TRUE),
        by = .(seccion, sexo, year)
        ][, c("q_85_89", "q_90_94", "q_95_99", "q_100_plus") := NULL]
  }
  return(res)
}


limpia_vias <- function(vias) {
  tvias <- c("calle", "avenida", "plaza", "partida", "camino", "carretera",
             "pasaje", "paseo", "vereda", "paraje", "ronda", "travesia",
             "parque", "grupo")

  vias      <- tolower(vias)
  vias[is.na(vias)] <- ""
  vias      <- tolower(vias)
  vias      <- gsub("\\sna(?=,)", "", vias, perl = TRUE)
  vias      <- gsub("^na\\s", "", vias)
  vias      <- gsub("\\sna\\s", "", vias)
  vias      <- gsub("\\s0(?=,)", "", vias, perl = TRUE)
  vias      <- gsub("\\s(9999?)", " ", vias)
  vias      <- gsub("\\d+(?=[a-z])", "", vias, perl = TRUE)
  vias_list <- strsplit(vias, ",")
  tvia_nvia <- mapply(function(x, y) x[!x %in% y], vias_list, lapply(vias_list, utils::tail, n = 3))
  tvia_nvia <- sapply(tvia_nvia, paste, collapse = " ")

  for (i in seq_along(tvias)) {
    ind_norm    <- grep(paste0("^", tvias[i], "[a-z]"), tvia_nvia)
    tvia_nvia[ind_norm] <- gsub(tvias[i], paste0(tvias[i], " "), tvia_nvia[ind_norm])
  }
  calle     <- "^(ca[^monstbp])\\w+\\b|^(c)\\b|^(cl[^rnia][^b])|^([^bv]lle*)\\w"
  avenida   <- "^(a.v)[^t]\\w+\\b|^(av)\\w+\\b|^(abg)\\w+\\b|^(vda)\\w+\\b|^a\\b|^av\\b"
  plaza     <- "^(pz?l?z?[^tsrqopjigedau])\\w+"
  partida   <- "^(par?t)\\w+|^(pda)\\w+|^(pr?t)\\w+|^pa.*da\\w+|^p.tda\\b"
  camino    <- "^(cam)[^p]\\w+|^(cm[^p])\\w+"
  carretera <- "^(ctr)\\w+|^(crt)\\w+"
  pasaje    <- "^(pa?s[^e]j?)\\w+|^(pas[^e](.*))\\w+|^pje\\b|^psj\\b"
  paseo     <- "^(pa?s[^a]e?)\\w+"
  travesia  <- "^(trav)(.*)\\b|^tr?v\\w+"
  tvia_nvia <- gsub(calle,     "calle",     tvia_nvia)
  tvia_nvia <- gsub(avenida,   "avenida",   tvia_nvia)
  tvia_nvia <- gsub(plaza,     "plaza",     tvia_nvia)
  tvia_nvia <- gsub(partida,   "partida",   tvia_nvia)
  tvia_nvia <- gsub(camino,    "camino",    tvia_nvia)
  tvia_nvia <- gsub(carretera, "carretera", tvia_nvia)
  tvia_nvia <- gsub(pasaje,    "pasaje",    tvia_nvia)
  tvia_nvia <- gsub(paseo,     "paseo",     tvia_nvia)
  tvia_nvia <- gsub(travesia,  "travesia",  tvia_nvia)
  tvia_nvia <- gsub("3a",      "tercera",   tvia_nvia)
  resto     <- lapply(vias_list, utils::tail, n = 3)
  resto     <- lapply(resto, gsub, pattern = "\\/(?<=\\/)(.*)", replacement = "", perl = TRUE)
  resto     <- gsub("\\s,", ",", trimws(sapply(resto, paste0, collapse = ",")))
  nvia      <- regmatches(tvia_nvia, gregexpr("\\d+", tvia_nvia))
  nvia      <- sapply(sapply(nvia, utils::tail, n = 1), paste0, collapse = "")
  nvia      <- unname(gsub("\\D",  "",  nvia))
  tvia_nvia <- trimws(mapply(function(x, y) gsub(x, "", y), nvia, tvia_nvia, USE.NAMES = FALSE))

  return(list(vias = unname(tvia_nvia), nvia = nvia, resto = resto))
}


filtro <- function(vias, nivel) {
  tvias     <- vias$vias
  indice    <- integer()
  tvia_norm <- c("calle", "avenida", "plaza", "partida", "camino", "carretera",
                 "pasaje", "paseo", "vereda", "paraje", "ronda", "travesia",
                 "parque", "grupo")
  inutiles  <- paste0("^(", paste0(tvia_norm, collapse = "|"),
                      ")\\s{1,10}\\d+,|^\\s?([a-z]+|\\d+)\\s{0,10},|^,|^\\s,")

  if (nivel == 1) {
    for (i in seq_along(tvia_norm)) {
      for (j in seq_along(tvia_norm)) {
        eliminar <- grep(paste0(tvia_norm[i], "\\s{1,10}", tvia_norm[j]), tvias)
        indice   <- sort(unique(c(indice, eliminar)))
        tvias[eliminar] <- gsub("^[a-z]+\\s{1,10}", "", tvias[eliminar])
      }
    }

  } else if (nivel == 2) {
    patron_ini  <- c("\\s", "\\(", "-")
    patron_fin  <- c("\\s", "\\.")
    descripcion <- c("urb", "urbanizacion", "ed", "edf", "edif", "edificio", "res",
                     "rsd", "rsden", "resid", "residencia", "geriatric.", "centro",
                     "grupo", "grup", "polig", "poligono", "finca", "aptos",
                     "complejo", "cooperativa", "coop")
    for (k in seq_along(descripcion)) {
      for (i in seq_along(patron_ini)) {
        for (j in seq_along(patron_fin)) {
          patron <- paste0("(?<=", patron_ini[i], descripcion[k], patron_fin[j], ")(.*)")
          indice <- sort(unique(c(indice, grep(patron, tvias, perl = TRUE))))
          tvias  <- gsub(patron, "", tvias, perl = TRUE)
          tvias  <- gsub(
            pattern     = paste0(patron_ini[i], descripcion[k], patron_fin[j]),
            replacement = "",
            x           = tvias
          )
        }
      }
    }
  } else if (nivel == 3) {
    indice <- grep("\\b[[:alpha:]]{1,3}\\b\\.?", tvias)
    tvias  <- gsub("\\b[[:alpha:]]{1,3}\\b\\.?", "", tvias)
  } else {
    tvias <- gsub("[[:punct:]]", "", tvias)
    for (i in seq_along(tvia_norm)) {
      eliminar <- grep(paste0("^", tvia_norm[i], "\\s{1,10}"), tvias)
      indice   <- sort(unique(c(indice, eliminar)))
      tvias[eliminar] <- gsub(paste0("^", tvia_norm[i], "\\s{1,10}"), "", tvias[eliminar])
    }
  }
  res <- data.table(idn = indice)
  res[, via := paste0(trimws(tvias[idn]), " ", vias$nvia[idn], ", ", vias$resto[idn])]
  res <- res[!grep(inutiles, via)]
  return(res)
}

aplica_filtros <- function(vias, datos, indice_nogeo, version_cc, nivel,
                           filtro_geo, cartografia, codigos, intentos = 10) {
  datos_f <- copy(datos)
  vias_f  <- lapply(vias, `[`, indice_nogeo)
  direcciones_f <- datos_f[indice_nogeo][["direcciones"]]

  if (nivel < 5) {
    res <- filtro(vias_f, nivel)
  } else {
    f1 <- filtro(vias_f, 1)
    direcciones_f[f1[["idn"]]] <- f1[["via"]]
    vias_f <- limpia_vias(direcciones_f)
    f2 <- filtro(vias_f, 2)
    direcciones_f[f2[["idn"]]] <- f2[["via"]]
    vias_f <- limpia_vias(direcciones_f)
    f3 <- filtro(vias_f, 3)
    direcciones_f[f3[["idn"]]] <- f3[["via"]]
    vias_f <- limpia_vias(direcciones_f)
    f4 <- filtro(vias_f, 4)
    direcciones_f[f4[["idn"]]] <- f4[["via"]]
    res <- data.table(idn = sort(unique(c(f1[["idn"]], f2[["idn"]],
                                         f3[["idn"]], f4[["idn"]]))))
    res[, via := direcciones_f[idn]]
  }

  if (nrow(res) > 0) {
    message("\nSe ha aplicado el filtro ", nivel,
            "\nBuscando en CartoCiudad (versi\u00f3n ",
            ifelse(version_cc == "prev", "previa", "actual"), ")...")
    geo_res    <- data.table(
      suppressWarnings(
        caRtociudad::cartociudad_geocode(
          full_address = res[["via"]],
          version      = version_cc,
          ntries       = intentos
        )
      )
    )
    if (version_cc == "prev") {
      indice_aux   <- which(geo_res[["state"]] %in% 1:2)
      indice_geo_f <- indice_nogeo[res[["idn"]][indice_aux]]
    } else {
      indice_aux   <- which(geo_res[["state"]] %in% 1:4)
      indice_geo_f <- indice_nogeo[res[["idn"]][indice_aux]]
    }
    if (length(indice_geo_f) > 0) {
      datos_f[indice_geo_f, `:=`(
        geocodificados = version_cc,
        lat            = geo_res[indice_aux][["lat"]],
        lng            = geo_res[indice_aux][["lng"]],
        via_modificada = res[indice_aux][["via"]]
      )]
      indice_nogeo <- indice_nogeo[!indice_nogeo %in% indice_geo_f]
      if (filtro_geo != "ninguno") {
        geom_aux <- sf::st_as_sf(datos_f[indice_geo_f, c("lng", "lat")],
                                 coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
        geom_aux <- sf::st_transform(geom_aux, crs = sf::st_crs(cartografia)$epsg)
        indice_nogeo <-
          if (filtro_geo == "nombre_municipio") {
            suppressMessages(sort(unique(c(indice_geo_f[which(sapply(
              sf::st_intersects(
                geom_aux, cartografia[cartografia$CUMUN %in% codigos[indice_geo_f], ]
              ),
              length) == 0)], indice_nogeo))))
          } else {
            suppressMessages(sort(unique(c(indice_geo_f[which(sapply(
              sf::st_intersects(
                geom_aux, cartografia[cartografia$CPRO %in% substr(codigos[indice_geo_f], 1, 2), ]
              ),
              length) == 0)], indice_nogeo))))
          }
      }
    }
    if (version_cc == "prev") {
      indice_nogeo_via <- datos_f[which(vias$nvia == "")][geocodificados == "prev"][["idn"]]
      indice_nogeo     <- unique(sort(c(indice_nogeo, indice_nogeo_via, which(geo_res[["state"]] == 2))))
    }
  }


  return(list(datos = datos_f, indice_nogeo = indice_nogeo))
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
