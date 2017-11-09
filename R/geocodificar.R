
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
#' @usage geocodificar(direcciones, codigos = NULL, cartografia = NULL,
#' nivel_filtro = c("municipio", "provincia", "ninguno"),
#' encoding = c("utf8", "latin1", "ascii"))
#'
#' @param direcciones Vector de caracteres con las direcciones a geocodificar.
#'   Las direcciones deben proporcionarse con el formato "TIPO_DE_VÍA
#'   NOMBRE_DE_VÍA NÚMERO_DE_VÍA, MUNICIPIO, PROVINCIA, CÓDIGO_POSTAL", donde el
#'   número de la vía y el código postal son campos opcionales.
#' @param codigos Vector de caracteres de longitud igual al vector de
#'   direcciones. Contiene los códigos INE (5 caracteres por código) de los
#'   municipios a los que hacen referencia las direcciones. Si el valor es nulo
#'   (opción por defecto), la función trata de averiguar el código desde la
#'   propia dirección.
#' @param cartografia Objeto de clase \code{cartografia_ine} con la cartografía
#'   que contenga las geometrías de municipios o provincias a los que hacen
#'   referencias las direcciones.
#' @param nivel_filtro Vector de caracteres de longitud 1 indicando el nivel de
#'   filtrado cartográfico a efectuar (eliminación de coordenadas devueltas pero
#'   que no correspondan a su polígono correspondiente). Las tres opciones son
#'   municipio (las coordenadas están dentro del polígono del municipio al que
#'   hace referencia el código de la dirección), provincia (las coordenadas
#'   están dentro del polígono de la provincia al que hace referencia el código
#'   de la dirección) y ninguno.
#' @param encoding Vector de caracteres de longitud 1 indicando el tipo de
#'   codificación de caracteres empleada. Las posibles opciones son UTF8,
#'   Latin-1 y ASCII (UTF8 por defecto).
#'
#' @return Un \code{data.frame} con tantas filas como direcciones se
#'   proporcionaron y con que cuenta con 10 columnas:
#'   \item{id}{Identificación de la dirección (se otorga secuencialmente).}
#'   \item{direcciones}{Direcciones originales proporcionadas.}
#'   \item{geocodificados}{Resultado de la geocodificación: cc_prev o cc_new si
#'     alguna de las dos versiones logra geocodificar o NA si ninguna lo logra.}
#'   \item{lat}{Latitud devuelta.}
#'   \item{lng}{Longitud devuelta.}
#'   \item{dir_cc_old}{Direcciones devueltas por la versión previa de
#'     CartoCiudad.}
#'   \item{dir_cc_new}{Dirección devuelta por la nueva versión de CartoCiudad.}
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
#'                cartografia = cartografia, nivel_filtro = "municipio",
#'                encoding = "utf8")
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
geocodificar <- function(direcciones, codigos = NULL, cartografia = NULL,
                         nivel_filtro = c("municipio", "provincia", "ninguno"),
                         encoding = c("utf8", "latin1", "ascii")) {

  stopifnot(is.character(direcciones) && length(direcciones) >= 1)
  stopifnot(is.null(codigos) || is.character(codigos) || length(codigos) != length(direcciones))
  stopifnot(is.null(cartografia) || all(sf::st_is(cartografia, "MULTIPOLYGON")))
  if (!"sf" %in% class(cartografia))
    stop("El objeto 'cartografia' debe ser de clase 'sf'.")

  nivel_filtro <- match.arg(nivel_filtro)
  encoding     <- match.arg(encoding)
  if (is.null(codigos) & nivel_filtro != "ninguno")
    warning(
      "Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
      "la cartograf\u00eda, pero no has proporcionado un vector de caracteres\n",
      "con los c\u00f3digos INE del nivel del filtro:\n",
      "la funci\u00f3n intentar\u00e1 extraer el nivel de filtrado de las direcciones,\n",
      "lo cual puede proporcionar resultados incorrectos si utilizas la\n",
      "funci\u00f3n en n\u00facleos de poblaci\u00f3n que no pertenezcan al proyecto MEDEA 3.")
  if (is.null(cartografia) & nivel_filtro != "ninguno")
    stop("Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
         "la cartograf\u00eda, pero no has proporcionado esta \u00faltima:\n",
         "utiliza la funci\u00f3n `descarga_cartografia()` para obtener la cartografia\n",
         "de toda Espa\u00f1a, o carga la cartografia MEDEA con `data(cartografia)`.")

  direcciones <- iconv(direcciones, from = encoding, to = "ascii//translit")
  direcciones[is.na(direcciones)] <- ""
  datos       <- data.table(id = seq_along(direcciones), direcciones = direcciones)
  geo_old     <- data.table(caRtociudad::cartociudad_geocode(datos[["direcciones"]], version = "prev"))
  indice_geo   <- which(geo_old[["state"]] %in% 1:2)

  datos[indice_geo, `:=`(
    geocodificados = "cc_prev",
    lat            = geo_old[indice_geo][["lat"]],
    lng            = geo_old[indice_geo][["lng"]],
    dir_cc_old     = geo_old[
      indice_geo,
      paste0(tip_via, " ", address, " ", portalNumber, ", ",
             muni, ", ", province, ", ", postalCode)
    ]
  )]

  if (nivel_filtro != "ninguno") {
    nivel_filtro <- switch(nivel_filtro,
                           "municipio" = "nombre_municipio",
                           "provincia" = "nombre_provincia")

    indice_nogeo <- which(is.na(datos[["geocodificados"]]) | geo_old[["state"]] == 2)

    if (is.null(codigos)) {
      muni_provs   <- lapply(
        lapply(strsplit(datos[indice_nogeo][["direcciones"]], split = ","), trimws),
        `[`,
        2:3
      )
      codigos      <- character(length(muni_provs))
      provs_carto  <- munis_carto <- vector("list", length(muni_provs))
      utils::data("codigos_ine")
      for (i in seq_along(indice_nogeo)) {
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

    geom_old <- sf::st_as_sf(datos[!indice_nogeo, c("lng", "lat")],
                             coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
    geom_old <- sf::st_transform(geom_old, crs = sf::st_crs(cartografia)$epsg)

    indice_nogeo <-
      if (nivel_filtro == "nombre_municipio") {
        sort(c(which(sapply(
          sf::st_intersects(
            geom_old, cartografia[cartografia$CUMUN %in% codigos, ]
          ),
          length) == 0), indice_nogeo))
      } else {
        sort(c(which(sapply(
          sf::st_intersects(
            geom_old, cartografia[cartografia$CPRO %in% substr(codigos, 1, 2), ]
          ),
          length) == 0), indice_nogeo))
      }
  }

  geo_new     <- data.table(caRtociudad::cartociudad_geocode(datos[indice_nogeo][["direcciones"]]))

  indice_geo2  <- indice_nogeo[which(geo_new[["state"]] %in% 1:4)]

  datos[indice_geo2, `:=`(
    geocodificados = "cc_new",
    lat            = geo_new[which(geo_new[["state"]] %in% 1:4)][["lat"]],
    lng            = geo_new[which(geo_new[["state"]] %in% 1:4)][["lng"]],
    dir_cc_new     = geo_new[
      which(geo_new[["state"]] %in% 1:4),
      paste0(tip_via, " ", address, " ", portalNumber, ", ",
             muni, ", ", province, ", ", postalCode)
      ]
  )]

  geom_new <- sf::st_as_sf(datos[indice_geo2, c("lng", "lat")],
                           coords = c("lng", "lat"), na.fail = FALSE, crs = 4258)
  geom_new <- sf::st_transform(geom_new, crs = sf::st_crs(cartografia)$epsg)

  indice_nogeo <- indice_nogeo[!indice_nogeo %in% indice_geo2]
  indice_nogeo <-
    if (nivel_filtro == "nombre_municipio") {
      sort(c(which(sapply(
        sf::st_intersects(
          geom_new, cartografia[cartografia$CUMUN %in% codigos, ]
        ),
        length) == 0), indice_nogeo))
    } else {
      sort(c(which(sapply(
        sf::st_intersects(
          geom_new, cartografia[cartografia$CPRO %in% substr(codigos, 1, 2), ]
        ),
        length) == 0), indice_nogeo))
    }
  datos[
    indice_nogeo,
    `:=`(
      geocodificados = NA,
      lat = NA,
      lng = NA,
      dir_cc_old = NA,
      dir_cc_new = NA
    )
  ]

# retira_descripcion <- function(x, descripcion = NULL) {
#   patron_inicio <- c("\\s", "\\(", "-")
#   patron_final  <- c("\\s", "\\.")
#   if (is.null(descripcion)) {
#     descripcion <- c("urb", "ed", "edif", "edificio", "resid", "residencia", "finca")
#   }
#   direccion_completa <- strsplit(x, ",")
#   direccion <- sapply(direccion_completa, `[`, 1)
#   direccion <- ifelse(is.na(direccion), "", direccion)
#   direccion <- gsub("\\sNA$", " ", direccion)
#   direccion <- gsub("\\s0$", " ", direccion)
#   n_cambios <- list()
#   cambios   <- matrix(0, nrow = length(direccion), ncol = 7)
#   cont <- 1
#   for (k in seq_along(descripcion)) {
#     for (i in seq_along(patron_inicio)) {
#       for (j in seq_along(patron_final)) {
#         patron    <- paste0("(?<=", patron_inicio[i], descripcion[k],
#                             patron_final[j], ")(.*)(?=\\s\\d+|\\s$)")
#         cambios[, cont] <- grepl(patron, direccion, perl = TRUE, ignore.case = TRUE)
#         direccion <- gsub(patron, "", direccion, perl = TRUE, ignore.case = TRUE)
#         direccion <- gsub(
#           pattern     = paste0(patron_inicio[i], descripcion[k], patron_final[j]),
#           replacement = "",
#           x           = direccion,
#           ignore.case = TRUE
#         )
#         cont            <- cont + 1
#       }
#     }
#     cambios[, cont] <- rowSums(cambios)
#     n_cambios[[k]]  <- cambios
#     cont            <- 1
#   }
#   res <- paste0(direccion, ",", sapply(lapply(direccion_completa, `[`, -1),
#                                        function(x) paste0(x[1], ",", x[2])))
#
#   utiles <- data.table(id = which(n_cambios[[k]][, 7] != 0))
#   utiles <- utiles[, direcciones := res[id]][!grepl("^calle\\s(9999?),|^calle\\s,|^\\s,",
#                                                     direcciones, ignore.case = T)]
#
#
#   return(res)
# }
# retira_descripcion(x)





















  return(datos)
}
