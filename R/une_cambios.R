
#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE (tomando como referencia
#'   la cartografía INE 2011), adaptando a su vez las poblaciones por sexo año y
#'   sección censal.
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
#' @param umbral_cambio Numérico: porcentaje de viviendas afectadas en el cambio
#'   de sección.
#'
#' @usage une_secciones(cambios, cartografia, years = 1996:2016, poblacion =
#'   NULL, corte_edad = 85, umbral_cambio = 0)
#'
#' @return El resultado devuelto varía en función de si se proporcionan datos de
#'   poblaciones o no. Si no se proporcionan se devuelve un objeto de clase
#'   \code{cartografia_ine} y \code{\link[sp]{SpatialPolygonsDataFrame}} con la
#'   cartografía, donde cada fila es una sección censal y que cuenta con 9
#'   columnas: \item{seccion}{Cadena de 10 caracteres con el código de sección
#'   censal (incluye provincia, municipio y distrito).} \item{CUMUN}{Cadena de 5
#'   caracteres con el código del municipio (incluye provincia).}
#'   \item{CCA}{Cadena de 2 caracteres con el código de comunidad autónoma.}
#'   \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre de la comunidad
#'   autónoma.} \item{NMUN}{Nombre del municipio.} \item{geometry}{Columna de
#'   tipo lista con la geometría asociada a cada sección censal.}
#'   \item{cluster_id}{Código de identificación del cluster de uniones.}
#'   \item{sc_unida}{Código de las secciones unidas.}
#'
#'   En caso de proporcionan poblaciones, se devuelve una lista de longitud
#'   igual a dos, donde el primer elemento es la cartografía descrita
#'   anteriormente y el segundo elemento de la lista es un objeto de clase
#'   \code{poblaciones_ine} donde las filas representan las distintas secciones
#'   censales. Las tres primeras columnas son: \item{seccion}{Código de la
#'   sección censal en el primer año.} \item{sexo}{Código de la sección censal
#'   en el segundo año.} \item{year}{Primer año.} El resto de columnas
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
#'   uniones <- une_secciones(cambios_seccion, cartografia, 2006:2016, poblacion)
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
                          poblacion = NULL, corte_edad = 85, umbral_cambio = 0) {

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
  stopifnot(is.numeric(umbral_cambio))

  utils::data("secciones")
  car_class  <- attributes(cartografia@data)$class
  fuente     <- "Fuente: Sitio web del INE: www.ine.es"
  cambios    <- cambios[between(year2, years[1], years[length(years)]) & cambio_ref >= umbral_cambio]
  sc_unicas  <- sort(
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
  cartografia$Group.1 <- NULL

  attributes(cartografia@data)$fuente <- fuente
  attributes(cartografia@data)$class  <- car_class
  res <- cartografia

  if (!is.null(poblacion)) {
    pob_class <- class(poblacion)
    poblacion <- poblacion[between(year, min(years), max(years))]
    poblacion <- elige_corte(poblacion, corte_edad)
    poblacion[,
              cluster := cluster_sc[
                match(seccion, sc),
                id_cluster
                ]]
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
    class(poblacion)             <- pob_class
    attributes(poblacion)$fuente <- fuente
    res <- list(cartografia = cartografia, poblacion = poblacion)
  }

  return(res)
}
