
#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE (tomando como referencia
#'   la cartografía INE 2011), adaptando a su vez las poblaciones por sexo año y
#'   sección censal. Si el archivo de cambios incorpora información catastral
#'   (número de viviendas afectada por cada cambio de sección), se puede fijar
#'   un umbral de cambio (\%) para rechazar aquellos cambios que involucren a muy
#'   pocas viviendas.
#'
#' @param cambios Objeto de clase \code{cambios_ine}.
#' @param cartografia Objeto de clase
#'   \code{\link[sp]{SpatialPolygonsDataFrame}}, y con datos de clase
#'   \code{cartografia_ine}.
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
#'   library(medear)
#'   data("poblacion")
#'   data("cambios_seccion")
#'   data("cartografia")
#'   uniones <- une_secciones(
#'     cambios         = cambios_seccion,
#'     cartografia     = cartografia,
#'     years           = 2006:2016,
#'     poblacion       = poblacion,
#'     catastro        = TRUE,
#'     umbral_vivienda = 5,
#'     umbral_tramo    = 10
#'   )
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
  car_class  <- attributes(cartografia@data)$class
  fuente     <- "Fuente: Sitio web del INE: www.ine.es"

  secciones_2011 <- as.data.table(cartografia@data[, c("seccion", "n_viv")])
  cambios        <- cambios[between(year2, years[1], years[length(years)])]

  carto_metro   <- sp::spTransform(
    cartografia,
    sp::CRS("+proj=utm +zone=28 +datum=WGS84")
  )
  cambios$no_11 <- FALSE
  cambios$colin <- NA
  cambios$dista <- NA_real_
  for (i in seq_len(nrow(cambios))) {
    carto1 <- carto_metro[carto_metro$seccion == cambios$sc_ref[i], ]
    carto2 <- carto_metro[carto_metro$seccion == cambios$sc_new[i], ]
    if (all(nrow(carto1) > 0, nrow(carto2) > 0)) {
      cambios$colin[i] <- rgeos::gIntersects(carto1, carto2)
      cambios$dista[i] <- rgeos::gDistance(carto1, carto2)
    } else {
      cambios$no_11[i] <- TRUE
    }
  }
  cambios[, camb_distrito := substr(sc_ref, 6, 7) != substr(sc_new, 6, 7)]
  part <- cambios[camb_distrito == TRUE & no_11 == TRUE]
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
  part    <- part[dista < 100]
  cambios <- rbindlist(list(tmp, part))[order(sc_ref, sc_new)]

  if (catastro) {
    for (i in seq_len(nrow(cambios))) {
      viv_r    <- secciones_2011[seccion == cambios[["sc_ref"]][i], n_viv]
      cambios[i, viv_ref := ifelse(length(viv_r) != 0, viv_r, NA_integer_)]
      cambios[i, cambio_ref := (viviendas / viv_ref * 100)]
    }
    filtrado <- cambios[
      (colin == TRUE | is.na(colin)) & (dista < 100 | is.na(dista)) &
        cambio_ref >= umbral_vivienda & tramo_por <= umbral_tramo
      ]
  } else {
    filtrado <- cambios[
      (colin == TRUE | is.na(colin)) & (dista < 100 | is.na(dista))
    ]
  }

  cambios   <- unique(rbindlist(list(filtrado, cambios[!sc_new %in% secciones_2011$seccion])))
  sc_unicas <- sort(
    unique(
      secciones[
        year %in% years & seccion %in% c(cambios$sc_ref, cambios$sc_new),
        seccion
      ]
    )
  )
  cluster_sc     <- data.table(sc = sc_unicas, id_cluster = sc_unicas)

  for (i in seq_len(nrow(cambios))) {
    sc_select <- which(cluster_sc[, sc] %in% cambios[i, c(sc_ref, sc_new)])
    sc_min    <- min(cluster_sc[sc_select, id_cluster])
    sc_assign <- which(cluster_sc[, id_cluster] %in%
                         cluster_sc[sc_select, id_cluster])
    cluster_sc[sc_assign, id_cluster := sc_min][]
  }

  cartografia$cluster_id <- cluster_sc$id_cluster[match(cartografia$seccion, cluster_sc$sc)]
  cartografia$cluster_id[is.na(cartografia$cluster_id)] <-
    cartografia$seccion[is.na(cartografia$cluster_id)]
  cartografia <- stats::aggregate(
    x        = cartografia,
    by       = list(cartografia$cluster_id),
    FUN      = function(x) x[[1]]
  )
  cartografia$seccion    <- cartografia$cluster_id
  cartografia$cluster_id <- NULL
  cartografia$Group.1    <- NULL

  attributes(cartografia@data)$fuente <- fuente
  attributes(cartografia@data)$class  <- car_class
  res <- cartografia

  if (!is.null(poblacion)) {
    pob_class <- class(poblacion)
    poblacion <- poblacion[between(year, min(years), max(years))]
    poblacion <- elige_corte(poblacion, corte_edad)
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
          for (col in names(pob1)[-c(1:3)])
            set(pob1, j = col, value = 1)
          poblacion <- rbindlist(list(poblacion, pob1))[order(seccion, sexo, year)]
        }
      }
    }
    class(poblacion)             <- pob_class
    attributes(poblacion)$fuente <- fuente
    res <- list(cartografia = cartografia, poblacion = poblacion)
  }

  return(res)
}
