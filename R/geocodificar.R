
#' @title Implementación del algoritmo de geocodificación de direcciones
#'
#' @export
#'
geocodificar <- function(direcciones, codigos_ine = NULL, cartografia = NULL,
                         nivel_filtro = c("muni", "prov", "ninguno")) {

  nivel_filtro <- match.arg(nivel_filtro)
  if (is.null(codigos_ine) & nivel_filtro != "ninguno")
    warning(
      "Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
      "la cartografía, pero no has proporcionado un vector de caracteres\n",
      "con los códigos INE del nivel del filtro:\n",
      "la función intentará extraer el nivel de filtrado de las direcciones,\n",
      "lo cual puede proporcionar resultados incorrectos si utilizas la\n",
      "función en núcleos de población que no pertenezcan al proyecto MEDEA 3.")
  if (is.null(cartografia) & nivel_filtro != "ninguno")
    stop("Has escogido realizar un filtrado de geolocalizaciones atendiendo a\n",
         "la cartografía, pero no has proporcionado esta última:\n",
         "utiliza la función descarga_cartografia() para obtener la cartografia\n",
         "de toda España, o carga la cartografia MEDEA con data(cartografia).")

  datos   <- data.table(id = seq_along(direcciones), direcciones = direcciones)
  geo_old <- data.table(caRtociudad::cartociudad_geocode(datos[["direcciones"]], version = "prev"))
  datos[geo_old[["state"]] %in% 1:2, geocodificados := "cc_prev"]
  datos[geo_old[["state"]] == 2, `:=`(
    parimp_o = ifelse(
      as.numeric(gsub("[a-z]|\\s", "", sapply(strsplit(direcciones, ","), `[`, 1))) %% 2 == 0,
      "par",
      "impar"
    ),
    parimp_c = ifelse(
      as.numeric(geo_old[geo_old$state == 2][["portalNumber"]]) %% 2 == 0,
      "par",
      "impar"
    ),
    coinciden = parimp_o == parimp_c
  )]
  no_geo_prev <- datos[is.na(geocodificados) | !coinciden][["direcciones"]]

  if (nivel_filtro != "ninguno") {
    if (is.null(codigos_ine)) {
      nivel_filtro <- switch(nivel_filtro, "muni" = "nombre_municipio", "prov" = "nombre_provincia")
      muni_provs   <- lapply(
        lapply(strsplit(direcciones, split = ","), trimws),
        `[`,
        2:3
      )
      codigos      <- length(muni_provs)
      provs_carto  <- munis_carto <- list(length(muni_provs))
      for (i in seq_along(muni_provs)) {
        provs_carto[[i]] <- grep(
          paste0("^", muni_provs[[i]][2], "|\\\\", muni_provs[[i]][2]),
          unique(codigos_ine[["nombre_provincia"]]),
          ignore.case = TRUE,
          value       = TRUE
        )
        munis_carto[[i]] <- grep(
          paste0("^", muni_provs[[i]][1], "|\\\\", muni_provs[[i]][1]),
          unique(codigos_ine[["nombre_municipio"]]),
          ignore.case = TRUE,
          value       = TRUE
        )
        codigos[i] <- codigos_ine[
          nombre_provincia %in% provs_carto[[i]] & nombre_municipio %in% munis_carto[[i]],
          paste0(cod_provincia, cod_municipio)
        ]
        if (length(codigos[i]) == 0) {
          warning("No se ha podido filtrar por cartografía en la dirección ", i)
          codigos[i] <- NA_character_
        }
      }
    }





  }









## google: objeto$results[[1]]$types indica el tipo de resultado. no utilizar codigos distintos a street address
## del mismo modo: objeto$results[[1]]$geometry$location_type equivale a sts 1 si es = a rooftop (geometría exacta).
## Si es = a range_interpolated, sería equivalente a un sts de 2-3, si geometric_center se devuelve el centro de la calle,
## eliminar resultados == approximate
##
##
## Se puede agregar bounds a la query, de forma que se filtra
##
## Filtrar por provincia, municipio, cp... httr::content(httr::RETRY(verb = "GET", url = "https://maps.googleapis.com/maps/api/geocode/json?address=valencia&sensor=false&components=administrative_level:comunidad%20valenciana|postal_code:46010"))
## o añadir a ggmap::geocode() sensor = "false&components=administrative_level:comunidad%20valenciana|postal_code:46010
}
