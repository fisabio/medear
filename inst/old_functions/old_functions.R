
limpia_vias <- function(vias) {
  tvias <- c("calle", "avenida", "plaza", "partida", "camino", "carretera",
             "pasaje", "paseo", "vereda", "paraje", "ronda", "travesia",
             "parque", "grupo")

  vias      <- tolower(vias)
  vias[is.na(vias)] <- ""
  vias      <- gsub("\\sna(?=,)", "", vias, perl = TRUE)
  vias      <- gsub("^na\\s", "", vias)
  vias      <- gsub("\\sna\\s", "", vias)
  vias      <- gsub("\\s0(?=,)", "", vias, perl = TRUE)
  vias      <- gsub("\\s(9999?)", " ", vias)
  vias      <- gsub("\\s3a\\s", "tercera", vias)
  vias_list <- strsplit(vias, ",")
  tvia_nvia <- mapply(function(x, y) x[!x %in% y], vias_list,
                      lapply(vias_list, utils::tail, n = 3))
  tvia_nvia <- sapply(tvia_nvia, paste, collapse = " ", USE.NAMES = FALSE)

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
  resto     <- lapply(vias_list, utils::tail, n = 3)
  resto     <- lapply(resto, gsub, pattern = "\\/(?<=\\/)(.*)",
                      replacement = "", perl = TRUE)
  resto     <- gsub("\\s,", ",", trimws(sapply(resto, paste0, collapse = ",")))
  nvia      <- regmatches(tvia_nvia, gregexpr("\\d+", tvia_nvia))
  nvia      <- sapply(sapply(nvia, utils::tail, n = 1),
                      paste0, collapse = "", USE.NAMES = FALSE)
  nvia      <- gsub("\\D",  "",  nvia)
  nvia      <- gsub("^0*(?=\\d+)", "", nvia, perl = TRUE)
  tvia_nvia <- trimws(mapply(function(x, y) gsub(x, "", y),
                             nvia, tvia_nvia, USE.NAMES = FALSE))
  res <- list(vias = tvia_nvia, nvia = nvia, resto = resto)
  res <- lapply(res, gsub, pattern = "\\s{2,}", replacement = " ")

  return(res)
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
          if (i == 2) {
            patron <- paste0(patron, "\\)|", patron)
          } else if (i == 3) {
            patron <- paste0(patron, "-|", patron)
          }
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
  if (length(indice) > 0) {
    indice <- indice[nchar(tvias[indice]) >= 4]
  }
  if (length(indice) > 0) {
    res <- data.table(idn = indice)
    res[, via := paste0(trimws(tvias[idn]), " ", vias$nvia[idn], ", ", vias$resto[idn])]
    res <- res[!grep(inutiles, via)]
  } else {
    res    <- data.table(idn = numeric(), via = character())
  }

  return(res)
}

aplica_filtros <- function(vias, datos, indice_nogeo, version_cc, nivel,
                           filtro_geo, cartografia, codigos, intentos = 10) {
  datos_f        <- copy(datos)
  indice_nogeo_f <- indice_nogeo
  vias_f         <- lapply(vias, `[`, indice_nogeo_f)
  direcciones_f  <- datos_f[indice_nogeo_f][["direcciones"]]

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
      indice_aux_f <- which(geo_res[["state"]] %in% 1:2)
      indice_geo_f <- indice_nogeo_f[res[["idn"]][indice_aux_f]]
    } else {
      indice_aux_f <- which(geo_res[["state"]] %in% 1:4)
      indice_geo_f <- indice_nogeo_f[res[["idn"]][indice_aux_f]]
    }
    if (length(indice_geo_f) > 0) {
      indice_fuera <- integer()
      if (filtro_geo != "ninguno") {
        geom_res <- sf::st_as_sf(geo_res[indice_aux_f, c("lng", "lat")],
                                 coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
        geom_res <- sf::st_transform(geom_res, crs = sf::st_crs(cartografia)$epsg)

        indice_fuera <-
          if (filtro_geo == "nombre_municipio") {
            suppressMessages(indice_geo_f[which(sapply(
              sf::st_intersects(
                geom_res, cartografia[cartografia$CUMUN %in% codigos[indice_geo_f], ]
              ),
              length) == 0)])
          } else {
            suppressMessages(indice_geo_f[which(sapply(
              sf::st_intersects(
                geom_res,
                cartografia[substr(cartografia$seccion, 1, 2) %in%
                              substr(codigos[indice_geo_f], 1, 2), ]
              ),
              length) == 0)])
          }
        if (length(indice_fuera) > 0) {
          indice_fuera   <- which(indice_geo_f %in% indice_fuera)
          indice_aux_f   <- indice_aux_f[-indice_fuera]
          indice_geo_f   <- indice_geo_f[-indice_fuera]
        }
      }
      indice_nogeo_f <- sort(unique(c(indice_nogeo_f, indice_fuera)))
      datos_f[
        indice_geo_f,
        `:=`(
          id           = geo_res[indice_aux_f][["id"]],
          province     = geo_res[indice_aux_f][["province"]],
          muni         = geo_res[indice_aux_f][["muni"]],
          tip_via      = geo_res[indice_aux_f][["tip_via"]],
          address      = geo_res[indice_aux_f][["address"]],
          portalNumber = geo_res[indice_aux_f][["portalNumber"]],
          refCatastral = geo_res[indice_aux_f][["refCatastral"]],
          postalCode   = geo_res[indice_aux_f][["postalCode"]],
          lat          = geo_res[indice_aux_f][["lat"]],
          lng          = geo_res[indice_aux_f][["lng"]],
          state        = as.character(geo_res[indice_aux_f][["state"]]),
          type         = as.character(geo_res[indice_aux_f][["type"]]),
          version      = as.character(geo_res[indice_aux_f][["version"]])
        )
        ]
      if (version_cc == "prev") {
        indice_old_2_f <- which(datos_f[indice_geo_f][["state"]] == 2)
        indice_nogeo_f <- unique(sort(c(indice_nogeo_f, indice_old_2_f)))
      } else {
        indice_nogeo_f <- which(is.na(datos_f[["state"]]))
      }
    }
  }

  return(list(datos = datos_f, indice_nogeo = indice_nogeo_f))
}


#' @title Implementación de la primera parte del algoritmo de geocodificación de
#'   direcciones de MEDEA3 (geocodificado con CartoCiudad)
#'
#' @description Esta función implementa la primera parte del algoritmo de
#'   geocodificación de MEDEA3. En esta parte se intentan geocodificar en
#'   primera instancia las direcciones haciendo uso del servicio CartoCiudad en
#'   su \href{http://www.cartociudad.es/CartoGeocoder/Geocode}{versión antigua}.
#'   En esta primera parte daremos por válidos todos aquellos direcciones que
#'   hayan obtenido estado == 1 (se ha encontrado la dirección correspondiente
#'   de forma exacta) o estado == 2 (dirección asignada al portal más próximo).
#'   El resto de individuos no geocodifados por la versión anterior, junto con
#'   los individuos geocodificados que no tuvieran portal y los que hayan
#'   obtenido status == 2 serán intentados geocodificar de nuevo con
#'   \href{http://www.cartociudad.es/geocoder/api/geocoder/findJsonp}{la nueva
#'   versión de CartoCiudad}. La geocodificación de las direcciones que no
#'   tienen portal resulta menos fiable en la versión antigua de CartoCiudad ya
#'   que son situadas en el inicio de su vía, mientras que en la nueva versión
#'   se sitúan en el centro, haciendo esta geocodificación más acertada. Por
#'   otro lado, la versión antigua de CartoCiudad en ocasiones cambia de acera
#'   (numeros pares a impares y viceversa) algunas direcciones mientras que en
#'   la versión nueva esto no ocurre. Por ello, intentaremos regeocodificar
#'   estas dos situaciones con la nueva versión de CartoCiudad, y en caso de que
#'   no encontremos la dirección correspondiente en dicha versión mantendríamos
#'   la geocodificación original conseguida por la versión previa.
#'
#'   Tras el proceso descrito, pueden considerarse una serie de variantes de las
#'   cadenas de carácteres de las direcciones no geocodificadas. La intención es
#'   valorar si esas variaciones podrían producir en alguna ocasión una
#'   geocodificación exitosa. En concreto, \code{geocodificar} contempla 5
#'   posibles variantes para las direcciones que no han podido ser
#'   geocodificadas: 1.- eliminar duplicidad de tipos de vía (ejemplo: calle
#'   camino ...-> camino ...); 2.- eliminar descripciones de vía (ejemplo:
#'   Avenida rosa (Edificio azul)->Avenida rosa); 3.- eliminar palabras de 3 o
#'   menos carácteres (ejemplo: calle la marina alta-> calle marina alta); 4.-
#'   eliminar signos de puntuación (ejemplo: calle gral. pedro.->calle gral
#'   pedro); 5.- implementación de todas las variantes anteriores de forma
#'   conjunta. \code{geocodificar} contempla todas estas variantes para
#'   cualquier dirección que no haya podido ser geocodificada a partir de su
#'   dirección original. En cualquier caso el chequeo de estas variantes puede
#'   ser deshabilitado si así se prefiere.
#'
#'   En todos los casos, y siempre que se desee (aunque la opción viene marcada
#'   por defecto), se puede aplicar un filtro cartográfico que deseche aquellas
#'   localizaciones que caigan fuera del municipio o la provincia de interés.
#'   Para habilitar este filtrado se debe proporcionar una cartografía (como la
#'   disponible mediante \code{\link{descarga_cartografia}}) y el código INE del
#'   municipio o provincia de interés (disponible en el banco de datos
#'   \code{\link{codigos_ine}}).
#'
#'   La función \code{geocodificar} va informando en la consola de \code{R} del
#'   progreso de cada una de las actividades que hemos descrito para que estamos
#'   informados en todo momento de en qué situación se encuentra el proceso.
#'
#'
#' @usage geocodificar(direcciones, idn = NULL, codigos = NULL, cartografia =
#'   NULL, filtro_geo = c("municipio", "provincia", "ninguno"),
#'   limpiar_direcciones = TRUE, intentos = 10)
#'
#' @param direcciones Vector de carácteres con las direcciones a geocodificar.
#'   Las direcciones deben proporcionarse \strong{OBLIGATORIAMENTE} con el
#'   formato "TIPO_DE_VÍA NOMBRE_DE_VÍA NÚMERO_DE_VÍA, MUNICIPIO, PROVINCIA,
#'   CÓDIGO_POSTAL". La codificación de carácteres a utilizar debe ser,
#'   \strong{OBLIGATORIAMENTE}, ASCII, de forma que no aparezcan carácteres como
#'   eÃ±es o tildes. Para transformar la codificación de carácteres al tipo
#'   ASCII, se puede emplear la función \code{\link[base]{iconv}} de la
#'   siguiente forma:
#'
#'   \code{iconv(direcciones, from = "CODIFICACION_DE_ORIGEN", to =
#'   "ascii//translit")}
#'
#'   donde \code{CODIFICACION_DE_ORIGEN} tomará valores como "utf8", "latin1" o
#'   "Windows-1252", por ejemplo.
#'
#' @param idn Vector de la misma longitud que \code{direcciones} con un
#'   identificador de cada uno de los registros en caso de que se quiera que
#'   dicho identificador aparezca en el data.frame (campo \code{idn}) devuelto
#'   por esta función. Si no se da ningún valor a este argumento, el campo
#'   \code{idn} del data.frame anterior será simplemente un vector secuencial
#'   que numera cada uno de los registros.
#' @param codigos Vector de carácteres de longitud igual al vector de
#'   direcciones. Contiene los códigos INE (5 carácteres por código) de los
#'   municipios a los que hacen referencia las direcciones. Si el valor es nulo
#'   (opción por defecto), la función trata de averiguar el código desde la
#'   propia dirección.
#' @param cartografia Objeto de clase \code{cartografia_ine} con la cartografía
#'   que contenga las geometrías de municipios o provincias a los que hacen
#'   referencias las direcciones.
#' @param filtro_geo Vector de carácteres de longitud 1 indicando el nivel de
#'   filtrado cartográfico a efectuar (eliminación de coordenadas devueltas pero
#'   que no correspondan a su polígono correspondiente). Las tres opciones son
#'   municipio (las coordenadas están dentro del polígono del municipio al que
#'   hace referencia el código de la dirección), provincia (las coordenadas
#'   están dentro del polígono de la provincia al que hace referencia el código
#'   de la dirección) y ninguno.
#' @param limpiar_direcciones Valor lógico. ¿Se desea considerar las variantes
#'   de las cadenas de texto de las direcciones que no hayan podido ser
#'   geocodificadas? Se recomienda activarlo conjuntamente al filtro
#'   cartográfico para evitar falsos positivos que puedan ser geocodificados
#'   fuera de la región de estudio.
#' @param intentos Valor numérico. Número de intentos de conexión con el
#'   servidor de CartoCiudad en caso de fallo de la misma, por defecto 10. Este
#'   parámetro evita que la geocodificación pare en caso de que haya un error de
#'   conexión en el servidor de CartoCiudad en algún momento.
#'
#' @return Un \code{data.frame} con tantas filas como la longitud de
#'   \code{direcciones} y con entre 7 y 8 columnas (en función de si se limpian
#'   las direcciones o no): \item{idn}{Identificador de cada uno de los
#'   registros de dirección, si no se ha determinado el argumento \code{idn} se
#'   rellena de forma secuencial).} \item{direcciones}{Dirección original según
#'   aparece en el argumento \code{direcciones}} \item{id}{Identificador
#'   catastral de la vía geocodificada.} \item{province}{Provincia de la vía
#'   geocodificada.} \item{muni}{Municipio de la vía geocodificada.}
#'   \item{tip_via}{Tipo de la vía geocodificada (calle, avenida...).}
#'   \item{address}{Nombre de la vía geocodificada.} \item{portalNumber}{Número
#'   de la vía geocodificada.} \item{refCatastral}{Referencia catastral del
#'   número de la vía geocodificada (solo devuelto por la versión previa de
#'   CartoCiudad).} \item{postalCode}{Código postal al que pertenece la vía
#'   geocodificada.} \item{lat}{Latitud asignada a la dirección.}
#'   \item{lng}{Longitud asignada a la dirección.}  \item{state}{Estado devuelto
#'   por los servicios de CartoCiudad (diferente según la versión, ver la ayuda
#'   de la función \code{\link[caRtociudad]{cartociudad_geocode}}).}
#'   \item{type}{Procedencia de la información relativa a la geocodificación
#'   (solo disponible en la versión actual de CartoCiudad).}
#'   \item{version}{Versión de CartoCiudad con la que se ha realizado la
#'   geocodificación.}
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   direcciones <- c(
#'     "Calle Aben Al Abbar 6, Valencia, Valencia",
#'     "Avenida Constitución 900, Valencia, Valencia",
#'     "Avenida Constitución 901, Valencia, Valencia",
#'     "Plaza Doctor Balmis 2, Alicante, Alicante",
#'     "A7 150",
#'     "A7 3000",
#'     "Calle Inventadísima 1, Valencia, Valencia"
#'   )
#'   codigos <- c(rep("46250", 3), "03014", "12082", "43148", "46250")
#'   cartografia <- descarga_cartografia()
#'   geocodificar(direcciones = direcciones, codigos = codigos,
#'                cartografia = cartografia, filtro_geo = "municipio",
#'                limpiar_direcciones = TRUE)
#' }
#'
#' @encoding latin1
#'
geocodificar <- function(direcciones, idn = NULL, codigos = NULL, cartografia = NULL,
                         filtro_geo = c("municipio", "provincia", "ninguno"),
                         limpiar_direcciones = TRUE, intentos = 10) {

  stopifnot(is.character(direcciones) && length(direcciones) >= 1)
  stopifnot(is.null(codigos) || is.character(codigos) || length(codigos) != length(direcciones))
  stopifnot(is.null(cartografia) || all(sf::st_is(cartografia, "MULTIPOLYGON")))
  stopifnot(is.logical(limpiar_direcciones))
  if (!"sf" %in% class(cartografia))
    stop("El objeto 'cartografia' debe ser de clase 'sf'.")

  filtro_geo <- match.arg(filtro_geo)
  if (is.null(codigos) & filtro_geo != "ninguno")
    warning(
      "Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
      "la cartograf\u00eda, pero no has proporcionado un vector de car\u00e1cteres\n",
      "con los c\u00f3digos INE del nivel del filtro:\n",
      "la funci\u00f3n intentar\u00e1 extraer el nivel de filtrado de las direcciones,\n",
      "lo cual puede proporcionar resultados incorrectos si utilizas la\n",
      "funci\u00f3n en n\u00facleos de poblaci\u00f3n que no pertenezcan al proyecto MEDEA 3.")
  if (is.null(cartografia) & filtro_geo != "ninguno")
    stop("Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
         "la cartograf\u00eda, pero no has proporcionado esta \u00faltima:\n",
         "utiliza la funci\u00f3n `descarga_cartografia()` para obtener la cartografia\n",
         "de toda Espa\u00f1a, o carga la cartografia MEDEA con `data(cartografia)`.")

  vias         <- limpia_vias(direcciones)
  direcciones  <- paste0(trimws(vias$vias), " ", vias$nvia, ", ", vias$resto)
  direcciones  <- gsub("\\s(?=,)", "", direcciones, perl = TRUE)
  direcciones  <- gsub("^,(.*)", "", direcciones)
  if (!is.null(idn)) {
    stopifnot(length(direcciones) == length(idn))
  } else {
    idn <- seq_along(direcciones)
  }
  datos <- data.table(idn = idn, direcciones = direcciones)

  # Geocodificado con CC Viejo
  message("Buscando en CartoCiudad (versi\u00f3n previa)...")
  geo_old      <- data.table(
    suppressWarnings(
      caRtociudad::cartociudad_geocode(
        full_address = datos[["direcciones"]],
        version      = "prev",
        ntries       = intentos
      )
    )
  )
  indice_geo   <- which(geo_old[["state"]] %in% 1:2)
  indice_nogeo <- which(!geo_old[["state"]] %in% 1:2)
  if (filtro_geo != "ninguno") {
    filtro_geo <- switch(filtro_geo,
                         "municipio" = "nombre_municipio",
                         "provincia" = "nombre_provincia")
    if (is.null(codigos)) {
      muni_provs   <- lapply(
        lapply(strsplit(datos[["direcciones"]], split = ","), trimws),
        `[`,
        2:3
      )
      codigos      <- character(length(muni_provs))
      provs_carto  <- munis_carto <- vector("list", length(muni_provs))
      utils::data("codigos_ine", envir = environment(), package = "medear")
      for (i in seq_along(codigos)) {
        provs_carto[[i]] <- grep(
          paste0("^", muni_provs[[i]][2], "$|^", muni_provs[[i]][2], "\\/"),
          unique(codigos_ine[["nombre_provincia"]]),
          ignore.case = TRUE,
          value       = TRUE
        )
        munis_carto[[i]] <- grep(
          paste0("^", muni_provs[[i]][1], "$|^", muni_provs[[i]][1], "\\/"),
          codigos_ine[["nombre_municipio"]],
          ignore.case = TRUE,
          value       = TRUE
        )
        temp <- codigos_ine[
          nombre_provincia %in% provs_carto[[i]] & nombre_municipio %in% munis_carto[[i]],
          paste0(cod_provincia, cod_municipio)
          ]
        temp <- ifelse(length(temp) == 0, "", temp)
        codigos[i] <- temp
        if (codigos[i] == "") {
          warning("No se ha podido filtrar por cartograf\u00eda en la direcci\u00f3n ", i)
          codigos[i] <- NA_character_
        }
      }
    }
    if (length(indice_geo) > 0) {
      geom_old <- sf::st_as_sf(geo_old[indice_geo, c("lng", "lat")],
                               coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
      geom_old <- sf::st_transform(geom_old, crs = sf::st_crs(cartografia)$epsg)
      indice_fuera <-
        if (filtro_geo == "nombre_municipio") {
          suppressMessages(indice_geo[which(sapply(
            sf::st_intersects(
              geom_old, cartografia[cartografia$CUMUN %in% codigos[indice_geo], ]
            ),
            length) == 0)])
        } else {
          suppressMessages(indice_geo[which(sapply(
            sf::st_intersects(
              geom_old, cartografia[substr(cartografia$seccion, 1, 2) %in% substr(codigos[indice_geo], 1, 2), ]
            ),
            length) == 0)])
        }
      indice_geo   <- indice_geo[!indice_geo %in% indice_fuera]
      indice_nogeo <- sort(unique(c(indice_nogeo, indice_fuera)))
    }
  }
  datos[
    indice_geo,
    `:=`(
      id           = geo_old[indice_geo][["id"]],
      province     = geo_old[indice_geo][["province"]],
      muni         = geo_old[indice_geo][["muni"]],
      tip_via      = geo_old[indice_geo][["tip_via"]],
      address      = geo_old[indice_geo][["address"]],
      portalNumber = geo_old[indice_geo][["portalNumber"]],
      refCatastral = geo_old[indice_geo][["refCatastral"]],
      postalCode   = geo_old[indice_geo][["postalCode"]],
      lat          = geo_old[indice_geo][["lat"]],
      lng          = geo_old[indice_geo][["lng"]],
      state        = as.character(geo_old[indice_geo][["state"]]),
      type         = as.character(geo_old[indice_geo][["type"]]),
      version      = as.character(geo_old[indice_geo][["version"]])
    )
    ]
  indice_old_2     <- which(datos[["state"]] == 2)
  indice_nogeo_via <- unique(sort(c(indice_nogeo, indice_old_2)))


  if (length(indice_nogeo_via > 0)) {
    # Geocodificado con CC Nuevo
    message("\nBuscando en CartoCiudad (versi\u00f3n actual)...")
    geo_new     <- data.table(
      suppressWarnings(
        caRtociudad::cartociudad_geocode(
          full_address = datos[indice_nogeo_via][["direcciones"]],
          ntries       = intentos
        )
      )
    )
    indice_aux <- which(geo_new[["state"]] %in% 1:4)
    if (length(indice_aux) > 0) {
      indice_geo2 <- indice_nogeo_via[indice_aux]
      if (filtro_geo != "ninguno") {
        geom_new <- sf::st_as_sf(geo_new[indice_aux, c("lng", "lat")],
                                 coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
        geom_new <- sf::st_transform(geom_new, crs = sf::st_crs(cartografia)$epsg)

        indice_fuera <-
          if (filtro_geo == "nombre_municipio") {
            suppressMessages(indice_geo2[which(sapply(
              sf::st_intersects(
                geom_new, cartografia[cartografia$CUMUN %in% codigos[indice_geo2], ]
              ),
              length) == 0)])
          } else {
            suppressMessages(indice_geo2[which(sapply(
              sf::st_intersects(
                geom_new, cartografia[substr(cartografia$seccion, 1, 2) %in% substr(codigos[indice_geo2], 1, 2), ]
              ),
              length) == 0)])
          }
        if (length(indice_fuera) > 0) {
          indice_fuera <- which(indice_geo2 %in% indice_fuera )
          indice_aux   <- indice_aux[-indice_fuera]
          indice_geo2  <- indice_geo2[-indice_fuera]
        }
      }
      datos[
        indice_geo2,
        `:=`(
          id           = geo_new[indice_aux][["id"]],
          province     = geo_new[indice_aux][["province"]],
          muni         = geo_new[indice_aux][["muni"]],
          tip_via      = geo_new[indice_aux][["tip_via"]],
          address      = geo_new[indice_aux][["address"]],
          portalNumber = geo_new[indice_aux][["portalNumber"]],
          refCatastral = geo_new[indice_aux][["refCatastral"]],
          postalCode   = geo_new[indice_aux][["postalCode"]],
          lat          = geo_new[indice_aux][["lat"]],
          lng          = geo_new[indice_aux][["lng"]],
          state        = geo_new[indice_aux][["state"]],
          type         = geo_new[indice_aux][["type"]],
          version      = geo_new[indice_aux][["version"]]
        )
        ]
      indice_nogeo <- datos[!sort(unique(c(indice_geo, indice_geo2)))][["idn"]]
    }


    # Aplicación de filtros a las cadenas de carácteres
    if (limpiar_direcciones) {
      f1_old <- aplica_filtros(vias, copy(datos), indice_nogeo, "prev", 1, filtro_geo,
                               cartografia, codigos, intentos)
      f1_new <- aplica_filtros(vias, copy(f1_old$datos), f1_old$indice_nogeo, "current",
                               1, filtro_geo, cartografia, codigos, intentos)
      f2_old <- aplica_filtros(vias, copy(f1_new$datos), f1_new$indice_nogeo, "prev", 2,
                               filtro_geo, cartografia, codigos, intentos)
      f2_new <- aplica_filtros(vias, copy(f2_old$datos), f2_old$indice_nogeo, "current",
                               2, filtro_geo, cartografia, codigos, intentos)
      f3_old <- aplica_filtros(vias, copy(f2_new$datos), f2_new$indice_nogeo, "prev", 3,
                               filtro_geo, cartografia, codigos, intentos)
      f3_new <- aplica_filtros(vias, copy(f3_old$datos), f3_old$indice_nogeo, "current",
                               3, filtro_geo, cartografia, codigos, intentos)
      f4_old <- aplica_filtros(vias, copy(f3_new$datos), f3_new$indice_nogeo, "prev", 4,
                               filtro_geo, cartografia, codigos, intentos)
      f4_new <- aplica_filtros(vias, copy(f4_old$datos), f4_old$indice_nogeo, "current",
                               4, filtro_geo, cartografia, codigos, intentos)
      f5_old <- aplica_filtros(vias, copy(f4_new$datos), f4_new$indice_nogeo, "prev", 5,
                               filtro_geo, cartografia, codigos, intentos)
      f5_new <- aplica_filtros(vias, copy(f5_old$datos), f5_old$indice_nogeo, "current",
                               5, filtro_geo, cartografia, codigos, intentos)
      datos        <- copy(f5_new$datos)
      indice_nogeo <- f5_new$indice_nogeo
    }
  }
  datos[
    indice_nogeo,
    `:=`(
      id           = NA_character_,
      province     = NA_character_,
      muni         = NA_character_,
      tip_via      = NA_character_,
      address      = NA_character_,
      portalNumber = NA_character_,
      refCatastral = NA_character_,
      postalCode   = NA_character_,
      lat          = NA_real_,
      lng          = NA_real_,
      state        = NA_character_,
      type         = NA_character_,
      version      = NA_character_
    )
    ][]


  return(datos)
}
