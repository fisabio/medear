
#' @title Implementación del algoritmo de geocodificación de direcciones
#'
#' @description Esta función implementa el algoritmo de geocodificación de
#'   MEDEA3, en el cual se realiza una primera pasada en la versión previa de
#'   CartoCiudad. Sobre los no geocodificados (considerando como no
#'   geocodificados a los que obtienen un estado diferente a 1 o 2, los que
#'   devuelven el portal más próximo pero sin respetar la numeración par o impar
#'   y los que no cumplan con el nivel de filtrado cartográfico indicado) se
#'   realiza una segunda pasada en la nueva versión de CartoCiudad.
#'
#'   Tras ello puede aplicarse una serie de filtros sobre las cadenas de
#'   caracteres para tratar de georreferenciar direcciones complejas (que
#'   incluyan repeticiones del tipo de vía, descripciones sobre el punto, o
#'   abreviaturas), y aunque el comportamiento por defecto es hacerlo, se trata
#'   de un argumento opcional.
#'
#'   En todos los casos, y siempre que se desee (aunque la opción viene marcada
#'   por defecto), se puede aplicar un filtro cartográfico que deseche aquellas
#'   localizaciones que caigan fuera del municipio o la provincia de interés.
#'   Para habilitar este filtrado se debe proporcionar una cartografía (como la
#'   disponible mediante \code{\link{descarga_cartografia}}) y el código INE del
#'   municipio o provincia de interés (disponible en el banco de datos
#'   \code{\link{codigos_ine}}).
#'
#' @usage geocodificar(direcciones, codigos = NULL, cartografia = NULL,
#'   filtro_geo = c("municipio", "provincia", "ninguno"), limpiar_direcciones =
#'   TRUE, intentos = 10)
#'
#' @param direcciones Vector de caracteres con las direcciones a geocodificar.
#'   Las direcciones deben proporcionarse \strong{OBLIGATORIAMENTE} con el
#'   formato "TIPO_DE_VÍA NOMBRE_DE_VÍA NÚMERO_DE_VÍA, MUNICIPIO, PROVINCIA,
#'   CÓDIGO_POSTAL". La codificación de caracteres a utilizar debe ser,
#'   \strong{OBLIGATORIAMENTE}, ASCII, de forma que no aparezcan caracteres como
#'   eñes o tildes. Para transformar la codificación de caracteres al tipo
#'   ASCII, se puede emplear la función \code{\link[base]{iconv}} de la
#'   siguiente forma:
#'
#'   \code{iconv(direcciones, from = "CODIFICACION_DE_ORIGEN", to =
#'   "ascii//translit")}
#'
#'   donde \code{CODIFICACION_DE_ORIGEN} tomará calores como "utf8", "latin1" o
#'   "Windows-1252", por ejemplo.
#'
#' @param codigos Vector de caracteres de longitud igual al vector de
#'   direcciones. Contiene los códigos INE (5 caracteres por código) de los
#'   municipios a los que hacen referencia las direcciones. Si el valor es nulo
#'   (opción por defecto), la función trata de averiguar el código desde la
#'   propia dirección.
#' @param cartografia Objeto de clase \code{cartografia_ine} con la cartografía
#'   que contenga las geometrías de municipios o provincias a los que hacen
#'   referencias las direcciones.
#' @param filtro_geo Vector de caracteres de longitud 1 indicando el nivel de
#'   filtrado cartográfico a efectuar (eliminación de coordenadas devueltas pero
#'   que no correspondan a su polígono correspondiente). Las tres opciones son
#'   municipio (las coordenadas están dentro del polígono del municipio al que
#'   hace referencia el código de la dirección), provincia (las coordenadas
#'   están dentro del polígono de la provincia al que hace referencia el código
#'   de la dirección) y ninguno.
#' @param limpiar_direcciones Valor lógico. ¿Se desea limpiar las cadenas de
#'   texto tras una primera pasada de georreferenciación? Se recomienda
#'   activarlo conjuntamente al filtro cartográfico para evitar falsos
#'   positivos. La función informa del tipo de limpieza aplicada conforme se va
#'   ejecutando. Hay cinco posibilidades: eliminar duplicidad de tipos de vía
#'   (1), eliminar descripciones de vía (2), eliminar palabras de 3 o menos
#'   caracteres (3), eliminar signos de puntuación (4) y todos los anteriores de
#'   forma secuencial (5).
#' @param intentos Valor numérico. Número de intentos de conexión con el
#'   servidor de CartoCiudad en caso de fallo de la misma. Por defecto 10.
#'
#' @return Un \code{data.frame} con tantas filas como direcciones se
#'   proporcionaron y con entre 7 y 8 columnas (en función de si se limpian las
#'   direcciones o no): \item{id}{Identificación de la dirección (se otorga
#'   secuencialmente).} \item{direcciones}{Direcciones originales
#'   proporcionadas.} \item{geocodificados}{Resultado de la geocodificación:
#'   cc_prev o cc_new si alguna de las dos versiones logra geocodificar o NA si
#'   ninguna lo logra.} \item{lat}{Latitud devuelta.} \item{lng}{Longitud
#'   devuelta.} \item{dir_cc_old}{Direcciones devueltas por la versión previa de
#'   CartoCiudad.} \item{dir_cc_new}{Dirección devuelta por la nueva versión de
#'   CartoCiudad.} \item{via_modificada}{Dirección devuelta por CartoCiudad tras
#'   limpiar la dirección.}
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
#' @encoding UTF-8
#'
#' @export
#'
geocodificar <- function(direcciones, codigos = NULL, cartografia = NULL,
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
      "la cartograf\u00eda, pero no has proporcionado un vector de caracteres\n",
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
  direcciones  <- gsub("^,\\s$", "", direcciones)
  datos        <- data.table(idn = seq_along(direcciones), direcciones = direcciones)

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
  geo_old[, c("stateMsg") := NULL]
  indice_geo   <- which(geo_old[["state"]] %in% 1:2)
  indice_nogeo <- which(!geo_old[["state"]] %in% 1:2)
  datos[
    indice_geo,
    `:=`(
      geocodificados = "prev",
      lat            = geo_old[indice_geo][["lat"]],
      lng            = geo_old[indice_geo][["lng"]],
      dir_cc_old     = geo_old[
        indice_geo,
        paste0(tip_via, " ", address, " ", portalNumber, ", ",
               muni, ", ", province, ", ", postalCode)
      ]
    )
  ]

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
      utils::data("codigos_ine")
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
      geom_old <- sf::st_as_sf(datos[indice_geo, c("lng", "lat")],
                               coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
      geom_old <- sf::st_transform(geom_old, crs = sf::st_crs(cartografia)$epsg)
      indice_nogeo <-
        if (filtro_geo == "nombre_municipio") {
          suppressMessages(sort(unique(c(indice_geo[which(sapply(
            sf::st_intersects(
              geom_old, cartografia[cartografia$CUMUN %in% codigos[indice_geo], ]
            ),
            length) == 0)], indice_nogeo))))
        } else {
          suppressMessages(sort(unique(c(indice_geo[which(sapply(
            sf::st_intersects(
              geom_old, cartografia[cartografia$CPRO %in% substr(codigos[indice_geo], 1, 2), ]
            ),
            length) == 0)], indice_nogeo))))
        }
    }
  }
  indice_nogeo_via <- datos[which(vias$nvia == "")][!is.na(geocodificados)][["idn"]]
  indice_nogeo_via <- unique(sort(c(indice_nogeo, indice_nogeo_via, which(geo_old[["state"]] == 2))))
  datos[
    indice_nogeo,
    `:=`(
      geocodificados = NA_character_,
      lat            = NA_real_,
      lng            = NA_real_,
      dir_cc_old     = NA_character_
    )
  ]

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
    indice_geo2 <- indice_nogeo_via[which(geo_new[["state"]] %in% 1:4)]
    if (length(indice_geo2) > 0) {

      indice_aux  <- indice_nogeo_via[indice_nogeo_via %in% indice_geo2] # 1's, 2's y != que new ha localizado. indice sobre geo_new
      indice_geo2 <- unique(sort(c(indice_geo2, indice_aux)))
      datos[
        indice_geo2,
        `:=`(
          geocodificados = "current",
          lat            = geo_new[indice_aux][["lat"]],
          lng            = geo_new[indice_aux][["lng"]],
          dir_cc_new     = geo_new[
            indice_aux,
            paste0(tip_via, " ", address, " ", portalNumber, ", ",
                   muni, ", ", province, ", ", postalCode)
            ]
          )
      ]
      indice_nogeo <- datos[is.na(geocodificados)][["idn"]]
      if (filtro_geo != "ninguno") {
        geom_new <- sf::st_as_sf(datos[indice_geo2, c("lng", "lat")],
                                 coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
        geom_new <- sf::st_transform(geom_new, crs = sf::st_crs(cartografia)$epsg)
        indice_nogeo <-
          if (filtro_geo == "nombre_municipio") {
            suppressMessages(sort(unique(c(indice_geo2[which(sapply(
              sf::st_intersects(
                geom_new, cartografia[cartografia$CUMUN %in% codigos[indice_geo2], ]
              ),
              length) == 0)], indice_nogeo))))
          } else {
            suppressMessages(sort(unique(c(indice_geo2[which(sapply(
              sf::st_intersects(
                geom_new, cartografia[cartografia$CPRO %in% substr(codigos[indice_geo2], 1, 2), ]
              ),
              length) == 0)], indice_nogeo))))
          }
      }
    }
    datos[
      indice_nogeo,
      `:=`(
        geocodificados = NA_character_,
        lat            = NA_real_,
        lng            = NA_real_,
        dir_cc_old     = NA_character_,
        dir_cc_new     = NA_character_
      )
    ]

    # Aplicación de filtros a las cadenas de caracteres
    if (limpiar_direcciones) {
      f1_old <- aplica_filtros(vias, datos, indice_nogeo, "prev", 1, filtro_geo,
                               cartografia, codigos, intentos)
      f1_new <- aplica_filtros(vias, f1_old$datos, f1_old$indice_nogeo, "current",
                               1, filtro_geo, cartografia, codigos, intentos)
      f2_old <- aplica_filtros(vias, f1_new$datos, f1_new$indice_nogeo, "prev", 2,
                               filtro_geo, cartografia, codigos, intentos)
      f2_new <- aplica_filtros(vias, f2_old$datos, f2_old$indice_nogeo, "current",
                               2, filtro_geo, cartografia, codigos, intentos)
      f3_old <- aplica_filtros(vias, f2_new$datos, f2_new$indice_nogeo, "prev", 3,
                               filtro_geo, cartografia, codigos, intentos)
      f3_new <- aplica_filtros(vias, f3_old$datos, f3_old$indice_nogeo, "current",
                               3, filtro_geo, cartografia, codigos, intentos)
      f4_old <- aplica_filtros(vias, f3_new$datos, f3_new$indice_nogeo, "prev", 4,
                               filtro_geo, cartografia, codigos, intentos)
      f4_new <- aplica_filtros(vias, f4_old$datos, f4_old$indice_nogeo, "current",
                               4, filtro_geo, cartografia, codigos, intentos)
      f5_old <- aplica_filtros(vias, f4_new$datos, f4_new$indice_nogeo, "prev", 5,
                               filtro_geo, cartografia, codigos, intentos)
      f5_new <- aplica_filtros(vias, f5_old$datos, f5_old$indice_nogeo, "current",
                               5, filtro_geo, cartografia, codigos, intentos)
      datos        <- f5_new$datos
      indice_nogeo <- f5_new$indice_nogeo
    }
    datos[
      indice_nogeo,
      `:=`(
        geocodificados = NA_character_,
        lat            = NA_real_,
        lng            = NA_real_,
        dir_cc_old     = NA_character_,
        dir_cc_new     = NA_character_,
        via_modificada = NA_character_
      )
    ]
  }


  return(datos)
}
