
#' @title Implementación de la primera parte del algoritmo de geocodificación de direcciones de MEDEA3 (geocodificado con CartoCiudad)
#'
#' @description Esta función implementa la primera parte del algoritmo de geocodificación de
#'   MEDEA3. En esta parte se intentan geocodificar en primera instancia las direcciones haciendo uso del servicio CartoCiudad en su versión antigua (http://www.cartociudad.es/CartoGeocoder/Geocode). En esta primera parte daremos por válidos todos aquellos direcciones que hayan obtenido estado=1 (se ha encontrado la dirección correspondiente de forma exacta) o estado=2 (dirección asignada al portal más próximo). El resto de individuos no geocodifados por la versión anterior, junto con los individuos geocodificados que no tuvieran portal y los que hayan obtenido status=2 serán intentados geocodificar de nuevo con la nueva versión de CartoCiudad (http://www.cartociudad.es/geocoder/api/geocoder/findJsonp). La geocodificación de las direcciones que no tienen portal resulta menos fiables en la versión antigua de CartoCiudad ya que son situadas en el inicio de su vía, mientras que en la nueva versión se sitúan en el centro, haciendo esta geocodificación más acertada. Por otro lado, la versión antigua de CartoCiudad en ocasiones cambia de acera (numeros pares a impares y viceversa) algunas direcciones mientras que en la versión nueva esto no ocurre. Por ello, intentaremos regeocodificar estas dos situaciones con la nueva versión de CartoCiudad, y en caso de que no encontremos la dirección correspondiente en dicha versión mantendríamos la geocodificación original conseguida por la versión previa.
#'
#'   Tras el proceso descrito, pueden considerarse una serie de variantes de las cadenas de caracteres de las direcciones no geocodificadas. La intención es valorar si esas variaciones podrían producir en alguna ocasión una geocodificación exitosa. En concreto, \code{geocodificar} contempla 5 posibles variantes para las direcciones que no han podido ser geocodificadas: 1.- eliminar duplicidad de tipos de vía (ejemplo: calle camino ...-> camino ...); 2.- eliminar descripciones de vía (ejemplo: Avenida rosa (Edificio azul)->Avenida rosa); 3.- eliminar palabras de 3 o menos caracteres (ejemplo: calle la marina alta-> calle marina alta); 4.- eliminar signos de puntuación (ejemplo: calle gral. pedro.->calle gral pedro); 5.- implementación de todas las variantes anteriores de forma conjunta. \code{geocodificar} contempla todas estas variantes para cualquier dirección que no haya podido ser geocodificada a partir de su dirección original. En cualquier caso el chequeo de estas variantes puede ser deshabilitado si así se prefiere.
#'
#'   En todos los casos, y siempre que se desee (aunque la opción viene marcada
#'   por defecto), se puede aplicar un filtro cartográfico que deseche aquellas
#'   localizaciones que caigan fuera del municipio o la provincia de interés.
#'   Para habilitar este filtrado se debe proporcionar una cartografía (como la
#'   disponible mediante \code{\link{descarga_cartografia}}) y el código INE del
#'   municipio o provincia de interés (disponible en el banco de datos
#'   \code{\link{codigos_ine}}).
#'
#'   La función \code{geocodificar} va informando en la consola de \code{R} del progreso de cada una de las actividades que hemos descrito para que estamos informados en todo momento de en qué situación se encuentra el proceso.
#'
#'
#' @usage geocodificar(direcciones, idn=NULL, codigos = NULL, cartografia = NULL,
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
#'   donde \code{CODIFICACION_DE_ORIGEN} tomará valores como "utf8", "latin1" o
#'   "Windows-1252", por ejemplo.
#'
#' @param idn Vector de la misma longitud que \code{direcciones} con un identificador de cada uno de los registros en caso de que se quiera que dicho identificador aparezca en el data.frame (campo \code{idn}) devuelto por esta función. Si no se da ningún valor a este argumento, el campo \code{idn} del data.frame anterior será simplemente un vector secuencial que numera cada uno de los registros.
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
#' @param limpiar_direcciones Valor lógico. ¿Se desea considerar las variantes de las cadenas de
#'   texto de las direcciones que no hayan podido ser geocodificadas? Se recomienda
#'   activarlo conjuntamente al filtro cartográfico para evitar falsos
#'   positivos que puedan ser geocodificados fuera de la región de estudio.
#' @param intentos Valor numérico. Número de intentos de conexión con el
#'   servidor de CartoCiudad en caso de fallo de la misma, por defecto 10. Este parámetro evita que la geocodificación pare en caso de que haya un error de conexión en el servidor de CartoCiudad en algún momento.
#'
#' @return Un \code{data.frame} con tantas filas como la longitud de \code{direcciones} y con entre 7 y 8 columnas (en función de si se limpian las
#'   direcciones o no): \item{idn}{Identificar de cada uno de los registros dirección, si no se ha determinado el argumento \code{idn} se rellena de forma secuencial).} \item{direcciones}{Dirección original según aparece en el argumento \code{direcciones}} \item{geocodificados}{Resultado de la geocodificación:
#'   cc_prev o cc_new si alguna de las dos versiones logra geocodificar la dirección correspondiente o NA si
#'   ninguna lo logra.} \item{lat}{Latitud asignada a la dirección.} \item{lng}{Longitud
#'   asignada a la dirección.} \item{dir_cc_old}{Direcciones devueltas por la versión previa de
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
geocodificar <- function(direcciones, idn=NULL, codigos = NULL, cartografia = NULL,
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
    datos[indice_geo2, `:=`(
      geocodificados = "current",
      lat            = geo_new[indice_aux][["lat"]],
      lng            = geo_new[indice_aux][["lng"]],
      dir_cc_new     = geo_new[
        indice_aux,
        paste0(tip_via, " ", address, " ", portalNumber, ", ",
               muni, ", ", province, ", ", postalCode)
        ]
    )]
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

  return(datos)
}
