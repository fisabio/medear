
#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE (tomando como referencia
#'   la cartografía INE 2011), adaptando a su vez las poblaciones por sexo año y
#'   sección censal. Si el archivo de cambios incorpora información catastral
#'   (número de viviendas afectada por cada cambio de sección), se puede fijar
#'   un umbral de cambio (\%) para rechazar aquellos cambios que involucren a
#'   muy pocas viviendas.
#'
#' @details Cuando se quiera unir cambios no solo en la cartografía sino también
#'   en los datos de población, puede aparecer un comportamiento inestable de la
#'   función. Esto es debido divergencias existentes en la información contenida
#'   en los datos de población y en los trameros (que es desde donde se crea el
#'   listado de cambios de sección), a pesar de que en ambos casos la fuente de
#'   información es el propio INE.
#'
#'   Lo anterior se traduce en que, para determinadas consultas, el número de
#'   secciones contenidas en los datos de cartografía y poblaciones no será el
#'   mismo, siendo lo más habitual que esto ocurra con los datos de población.
#'   Cuando esto pase (si pasa) la función devolverá un aviso, indicando qué
#'   secciones se ven afectadas y en qué años, de forma que el usuario pueda
#'   tratar de solucionarlo por su cuenta, aunque no hay una solución perfecta.
#'
#'   Las dos soluciones más efectivas (aunque son soluciones \emph{ad hoc} y
#'   recae en el usuario encontrar la más apropiada para su consulta concreta)
#'   que se han encontrado son:
#'
#'   \enumerate{ \item modificar los criterios temporales de la consulta,
#'   ampliando o reduciendo el marco temporal (p. ej., pasar de un período
#'   2001:2015 a 1996:2015 o 2002:2014); \item consultar las secciones
#'   problemáticas (accesibles mediante la consulta \code{attr(yy,
#'   "sc_not_in_cartografia")}) en los datos de población y, en base al archivo
#'   de cambios de sección, decidir con qué sección se debería realizar la
#'   unión.}
#'
#'   En el ejemplo se desarrollan ambos abordajes, con un tratamiento más
#'   extensivo en la viñeta de unión de seccionado (XXXXXXXXX).
#'
#' @param cambios Objeto de clase \code{cambios_ine}.
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}}, y con
#'   datos de clase \code{cartografia_ine}.
#' @param years Vector numérico de longitud >= 1 con los años para los que se
#'   desee consultar las variaciones de seccionado. El año 2011 debe figurar
#'   dentro del vector, cuyo rango debe ser continuo (sin saltos de más de un
#'   año).
#' @param poblacion Objeto de clase \code{poblaciones_ine}. Argumento opcional a
#'   proporcionar en caso de querer agregar las poblaciones.
#' @param corte_edad Numérico: punto de corte para los grupos de edad (85 o
#'   100). Argumento opcional en caso de proporcionar datos de poblaciones.
#' @param catastro Lógico: ¿El archivo de cambios incorpora datos sobre el
#'   catastro? Por defecto \code{catastro = FALSE}.
#' @param umbral_vivienda Numérico: porcentaje de viviendas afectadas en el
#'   cambio de sección. Solo se utiliza si \code{catastro = TRUE}. Por defecto
#'   se fija al 5 \%.
#' @param umbral_tramo Numérico: porcentaje de tramos afectados por la unión de
#'   secciones respecto al total de tramos contenidos en la sección de
#'   referencia (2011). Solo se utiliza si \code{catastro = TRUE}. Por defecto
#'   se fija a 10 \%.
#'
#' @usage une_secciones(cambios, cartografia, years = 1996:2016, poblacion =
#'   NULL, corte_edad = 85, catastro = FALSE, umbral_vivienda = 5, umbral_tramo
#'   = 10)
#'
#' @return El resultado devuelto varía en función de si se proporcionan datos de
#'   poblaciones o no. Si no se proporcionan se devuelve un objeto de clase
#'   \code{cartografia_ine} y \code{\link[sp]{SpatialPolygonsDataFrame}} con la
#'   cartografía, donde cada fila es una sección censal y que cuenta con 9
#'   columnas: \describe{\item{seccion}{Cadena de 10 caracteres con el código de
#'   sección censal (incluye provincia, municipio y distrito).}
#'   \item{CUMUN}{Cadena de 5 caracteres con el código del municipio (incluye
#'   provincia).} \item{CCA}{Cadena de 2 caracteres con el código de comunidad
#'   autónoma.} \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre de la
#'   comunidad autónoma.} \item{NMUN}{Nombre del municipio.}
#'   \item{geometry}{Columna de tipo lista con la geometría asociada a cada
#'   sección censal.} \item{cluster_id}{Código de identificación del cluster de
#'   uniones.} \item{sc_unida}{Código de las secciones unidas.}}
#'
#'   En caso de proporcionan poblaciones, se devuelve una lista de longitud
#'   igual a dos, donde el primer elemento es la cartografía descrita
#'   anteriormente y el segundo elemento de la lista es un objeto de clase
#'   \code{poblaciones_ine} donde las filas representan las distintas secciones
#'   censales. Las tres primeras columnas son: \describe{\item{seccion}{Código
#'   de la sección censal.} \item{sexo}{Sexo de la población (0 = masculino; 1 =
#'   femenino).} \item{year}{Año de referencia.}} El resto de columnas
#'   representan los distintos grupos de edad, tras realizar el corte en los
#'   grupos de edad (85 0 100).
#'
#' @examples
#'
#' \dontrun{
#'   # En este ejemplo se trabaja con la ciudad de Palma de Mallorca (código ine: 07040)
#'   # en los años 2004-2015
#'
#'   library(medear)
#'   data("poblacion")
#'   data("cambios_seccion")
#'   data("cartografia")
#'   cambios_pm     <- cambios_seccion[substr(cambios_seccion$sc_ref, 1, 5) == "07040", ]
#'   cartografia_pm <- cartografia[cartografia$CUMUN == "07040", ]
#'   poblacion_pm   <- poblacion[substr(poblacion$seccion, 1, 5) == "07040", ]
#'
#'   ##########################################################################
#'   ## Ejemplo sin utilizar el filtro de catastro                           ##
#'   ##########################################################################
#'
#'   union_sin_cat <- une_secciones(
#'     cambios         = cambios_pm,
#'     cartografia     = cartografia_pm,
#'     years           = 2004:2015,
#'     poblacion       = poblacion_pm,
#'     catastro        = FALSE
#'   )
#'
#'   nrow(union_sin_cat$cartografia) # 197 secciones
#'   length(unique(union_sin_cat$poblacion$seccion)) # 197 secciones
#'   round(nrow(union_sin_cat$cartografia) / nrow(cartografia_ali), 2) # Conserva el 78 \% de secciones
#'   # No salta ningún aviso sobre divergencia en seccionado entre cartografía y poblaciones,
#'   # pero se puede comprobar manualmente:
#'   unique(union_sin_cat$poblacion$seccion)[!unique(union_sin_cat$poblacion$seccion) %in% union_sin_cat$cartografia$seccion]
#'   union_sin_cat$cartografia$seccion[!union_sin_cat$cartografia$seccion %in% unique(union_sin_cat$poblacion$seccion)]
#'   # No hay ninguna sección problemática.
#'
#'
#'   ##########################################################################
#'   ## Ejemplo utilizando el filtro de catastro                             ##
#'   ##########################################################################
#'
#'   union_con_cat <- une_secciones(
#'     cambios         = cambios_pm,
#'     cartografia     = cartografia_pm,
#'     years           = 2004:2016,
#'     poblacion       = poblacion_pm,
#'     catastro        = TRUE,
#'     umbral_vivienda = 5,
#'     umbral_tramo    = 10
#'   )
#'
#'   nrow(union_con_cat$cartografia) # 197 secciones
#'   length(unique(union_con_cat$poblacion$seccion)) # 197 secciones
#'   round(nrow(union_con_cat$cartografia) / nrow(cartografia_ali), 2) # Conserva el 78 \% de secciones
#'   # No salta ningún aviso sobre divergencia en seccionado entre cartografía y poblaciones,
#'   # pero se puede comprobar manualmente:
#'   unique(union_con_cat$poblacion$seccion)[!unique(union_con_cat$poblacion$seccion) %in% union_con_cat$cartografia$seccion]
#'   union_con_cat$cartografia$seccion[!union_con_cat$cartografia$seccion %in% unique(union_con_cat$poblacion$seccion)]
#'   # No hay ninguna sección problemática.
#'
#'
#'
#'
#'
#'   poblacion   <- uniones$poblacion
#'   cartografia <- uniones$cartografia
#' }
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @seealso \code{\link{detecta_cambios}}, \code{\link{descarga_poblaciones}} y
#'   \code{\link{descarga_cartografia}}
#'
une_secciones <- function(cambios, cartografia, years = 1996:2016,
                          poblacion = NULL, corte_edad = 85, catastro = FALSE,
                          umbral_vivienda = 5, umbral_tramo = 10) {

  if (!"cambios_ine" %in% class(cambios))
    stop("El objeto 'cambios' debe ser de clase 'cambios_ine'.")
  if (!is.null(poblacion) && !"poblaciones_ine" %in% class(poblacion))
    stop("El objeto 'poblacion' debe ser de clase 'poblaciones_ine'.")
  if (!is.numeric(years) & length(years) < 2)
    stop("El objeto 'years' debe ser un vector numeric de longitud >= 2.")
  if (!2011 %in% years)
    stop("2011 debe estar incluido en el objeto 'years'.")
  if ("SpatialPolygonsDataFrame" != class(cartografia))
    stop("El objeto 'cartografia' debe ser de clase 'SpatialPolygonsDataFrame'.")
  years <- sort(years)
  if (any(years != min(years):max(years)))
    stop("El rango de years debe ser continuo (sin saltos mayores a uno).")
  stopifnot(corte_edad %in% c(85, 100))
  stopifnot(is.numeric(umbral_vivienda))
  stopifnot(is.numeric(umbral_tramo))
  stopifnot(is.logical(catastro))


  if ("vias" %in% names(cambios)) {
    cambios[, vias := NULL]
  }
  utils::data("secciones")
  fuente     <- "Fuente: Sitio web del INE: www.ine.es"

  secciones_2011 <- secciones[
    year == 2011 & substr(seccion, 1, 5) %in% cambios[, substr(sc_ref, 1, 5)]
  ][, n_viv := cartografia@data[match(cartografia$seccion, seccion), "n_viv"]]
  cambios        <- cambios[between(year2, years[1], years[length(years)])]
  if (nrow(cambios) > 0) {
    carto_metro    <- sp::spTransform(
      cartografia,
      sp::CRS("+proj=utm +zone=28 +datum=WGS84")
    )
    cambios$no_11 <- FALSE
    cambios$dista <- NA_real_
    for (i in seq_len(nrow(cambios))) {
      carto1 <- carto_metro[carto_metro$seccion == cambios$sc_ref[i], ]
      carto2 <- carto_metro[carto_metro$seccion == cambios$sc_new[i], ]
      if (all(nrow(carto1) > 0, nrow(carto2) > 0)) {
        cambios$dista[i] <- rgeos::gDistance(carto1, carto2)
      } else {
        cambios$no_11[i] <- TRUE
      }
    }

    # Quiza se podria pasar esta parte tras una primera union de secciones con cambios[no_11 == FALSE]...
    part <- cambios[no_11 == TRUE]
    tmp  <- fsetdiff(cambios, part)
    for (i in seq_len(nrow(part))) {
      sc_inv <- fsetdiff(cambios, part[i])[
        sc_new == part[i, sc_new], c(sc_ref, sc_new)
      ]
      sc_inv <- unique(sc_inv[sc_inv != part[i, sc_new]])
      dista  <- numeric()

      for (j in seq_along(sc_inv)) {
        carto1 <- carto_metro[carto_metro$seccion == part[i, sc_ref], ]
        carto2 <- carto_metro[carto_metro$seccion == sc_inv[j], ]
        if (all(nrow(carto1) > 0, nrow(carto2) > 0)) {
          dista[j] <- rgeos::gDistance(carto1, carto2)
        }
      }
      if (length(dista) > 0)
        part$dista[i] <- dista[which.max(dista)]
    }
    cambios <- rbindlist(list(tmp, part))[order(sc_ref, sc_new)]
    # Hasta aqui

    if (catastro) {
      for (i in seq_len(nrow(cambios))) {
        viv_r    <- secciones_2011[seccion == cambios[["sc_ref"]][i], n_viv]
        cambios[i, viv_ref := ifelse(length(viv_r) != 0, viv_r, NA_integer_)]
        cambios[i, cambio_ref := (viviendas / viv_ref * 100)]
      }
      # Conservar cambios cuya seccion de comparacion no exista en 2011
      # Conservar cambios que impliquen cambio >= umbral_vivienda y dista < 500 m, o
      # cambios que, con cambio < umbral_vivienda, tienen tramo_por != 0 y dista < 500 m
      cambios <- cambios[
        no_11 == TRUE |
          ((cambio_ref >= umbral_vivienda | (cambio_ref < umbral_vivienda & tramo_por >= umbral_tramo)) &
             (dista < 500 | is.na(dista)))
      ]
    } else {
      cambios <- cambios[no_11 == TRUE | (dista < 500 | is.na(dista))]
    }

    sc_unicas <- sort(
      unique(
        secciones[
          year %in% years & seccion %in% c(cambios$sc_ref, cambios$sc_new),
          seccion
        ]
      )
    )
    cluster_sc <- data.table(sc = sc_unicas, id_cluster = sc_unicas)
    for (i in seq_len(nrow(cambios))) {
      sc_select <- which(cluster_sc[, sc] %in% cambios[i, c(sc_ref, sc_new)])
      sc_min    <- min(cluster_sc[sc_select, id_cluster])
      sc_assign <- which(cluster_sc[, id_cluster] %in%
                           cluster_sc[sc_select, id_cluster])
      cluster_sc[sc_assign, id_cluster := sc_min][]
    }
    cluster_sc <- cluster_sc[order(id_cluster)]
    indice_ini <- which(cluster_sc$sc == cluster_sc$id_cluster)
    indice_fin <- c(indice_ini[-1] - 1, nrow(cluster_sc))
    id_cluster <- character(length(indice_ini))
    sc_ini     <- character(length(indice_ini))
    for (i in seq_along(indice_ini)) {
      sc_ini[i]     <- cluster_sc[indice_ini[i], sc]
      id_cluster[i] <- paste(cluster_sc[indice_ini[i]:indice_fin[i], sc], collapse = "-")
    }

    cartografia$cluster_id <- cluster_sc$id_cluster[match(cartografia$seccion, cluster_sc$sc)]
    cartografia$cluster_id[is.na(cartografia$cluster_id)] <-
      cartografia$seccion[is.na(cartografia$cluster_id)]
    cartografia <- stats::aggregate(
      x   = cartografia,
      by  = list(cartografia$cluster_id),
      FUN = function(x) x[[1]]
    )
    cartografia$seccion    <- cartografia$cluster_id
    cartografia$cluster_id <- NA_character_
    for (i in seq_along(sc_ini)) {
      cartografia$cluster_id[cartografia$seccion == sc_ini[i]] <- id_cluster[i]
    }
    cartografia$Group.1   <- NULL
  } else if (is.null(poblacion)) {
    message("En el per\u00edodo establecido no se ha detectado ning\u00fan cambio:\n",
            "se devuelve la misma cartograf\u00eda.")
  }
  attributes(cartografia@data)$fuente  <- fuente
  attributes(cartografia@data)$cluster <- cluster_sc
  res <- cartografia


  if (!is.null(poblacion)) {
    poblacion <- poblacion[between(year, min(years), max(years))]
    poblacion <- elige_corte(poblacion, corte_edad)
    if (nrow(cambios) > 0) {
      poblacion[, cluster := cluster_sc[match(seccion, sc), id_cluster]]
      poblacion[is.na(cluster), cluster := seccion]
      in_col <- names(poblacion)[
        !names(poblacion) %in% c("seccion", "sexo", "year", "cluster")
      ]
      poblacion <- poblacion[
        ,
        lapply(.SD, sum),
        by      = .(cluster, sexo, year),
        .SDcols = in_col
      ]
      setnames(poblacion, "cluster", "seccion")
      sc_11 <- unique(poblacion[year == 2011, seccion])
      years2 <- years[years != 2011]
      for (i in seq_along(years2)) {
        if (!all(sc_11 %in% poblacion[year == years2[i], seccion])) {
          sc_not_11 <- sc_11[which(!sc_11 %in% poblacion[year == years2[i], seccion])]
          for (j in seq_along(sc_not_11)) {
            pob1 <- poblacion[year == years2[i]][1:2]
            pob1[, `:=`(
              seccion = sc_not_11[j],
              sexo    = 0:1
            )][]
            set(pob1, j = names(pob1)[-c(1:3)], value = 1)
            poblacion <- rbindlist(list(poblacion, pob1))[order(seccion, sexo, year)]
          }
        }
      }
    } else {
      message(
        "En el per\u00edodo establecido no se ha detectado ning\u00fan cambio:\n",
        "se devuelve la misma cartograf\u00eda y poblaci\u00f3n para ese per\u00edodo,",
        "\najustando esta \u00faltima al corte de edad marcado."
      )
    }

    attributes(poblacion)$fuente <- fuente
    res <- list(cartografia = cartografia, poblacion = poblacion)

    if (!identical(sort(cartografia$seccion), sort(unique(poblacion$seccion)))) {
      not_in_pobla <- cartografia$seccion[!cartografia$seccion %in% unique(poblacion$seccion)]
      not_in_carto <- unique(poblacion$seccion)[!unique(poblacion$seccion) %in% cartografia$seccion]
      if (length(not_in_pobla) > 0) {
        warning(
          "Tras realizar la uni\u00f3n con las opciones marcadas, las secciones \nc('",
          paste0(not_in_pobla, collapse = "', '"),
          "')\naparecen en la cartograf\u00eda pero no en los datos de poblaci\u00f3n.\n",
          "Por favor, consulte la ayuda de la funci\u00f3n para tratar de solucionarlo.",
          call. = FALSE
        )
        attr(res, "sc_not_in_poblacion") <- not_in_pobla
      }
      if (length(not_in_carto) > 0) {
        not_in_years <- unique(poblacion[seccion %in% not_in_carto, year])
        warning(
          "Tras realizar la uni\u00f3n con las opciones marcadas, las secciones\n c('",
          paste0(not_in_carto, collapse = "', '"),
          "')\naparecen en los datos de poblaci\u00f3n pero no en la cartograf\u00eda,\n",
           " para los a\u00f1os c(", paste0(not_in_years, collapse = ", "), ").\n",
          "Por favor, consulte la ayuda de la funci\u00f3n para tratar de solucionarlo.",
          call. = FALSE
        )
        attr(res$poblacion, "sc_not_in_cartografia") <- not_in_carto
        attr(attr(res$poblacion, "sc_not_in_cartografia"), "years") <- not_in_years
      }
    }

    uno_vect <- rowSums(poblacion[, -c(1:3)]) == sum(ncol(poblacion[, -c(1:3)]))

    if (any(uno_vect)) {
      warning(
        "En el per\u00edodo seleccionado algunas secciones no sufrieron cambios pero \n",
        "aparecieron m\u00e1s tarde que el a\u00f1o de inicio elegido. Se asigna el valor 1 como \n",
        "poblaci\u00f3n a dichas secciones para los a\u00f1os previos (hasta el a\u00f1o de inicio fijado).\n",
        "Por favor, consulte la ayuda de la funci\u00f3n para explorar este aspecto.",
        call. = FALSE
      )
      attr(res$poblacion, "pob_igual_uno") <- unique(poblacion[uno_vect, seccion])
    }
  }

  return(res)
}
