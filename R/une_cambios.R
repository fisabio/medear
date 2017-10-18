

#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE en la cartografía INE
#'   2011 y en las poblaciones por sexo año y sección censal.
#'
#' @param cambios Objeto de clase \code{cambios_ine}.
#' @param poblacion Objeto de clase \code{poblaciones_ine}.
#' @param cartografia Objeto de clase \code{cartografia_ine}.
#' @param years Vector numérico de longitud >= 1 con los años para los que se
#'   desee consultar las variaciones de seccionado. El año 2011 debe figurar
#'   dentro del vector, cuyo rango debe ser continuo (sin saltos de más de un
#'   año).
#' @param corte_edad Numérico: punto de corte para los grupos de edad (85 o
#'   100).
#'
#' @return Una lista de longitud igual a dos, donde el primer elemento es un
#'   objeto de clase \code{cartografia_ine} y \code{sf}, donde cada fila es una
#'   sección censal y que cuenta con 15 columnas:
#'   \item{CUSEC}{Cádena de 10 caracteres con el código de sección censal
#'     (incluye provincia, municipio y distrito).}
#'   \item{CUMUN}{Cádena de 5 caracteres con el código del municipio (incluye
#'     provincia).}
#'   \item{CSEC}{Cádena de 3 caracteres con el código de sección censal.}
#'   \item{CDIS}{Cádena de 2 caracteres con el código de distrito.}
#'   \item{CPRO}{Cádena de 3 caracteres con el código de provincia.}
#'   \item{CCA}{Cádena de 2 caracteres con el código de comunidad autónoma.}
#'   \item{CUDIS}{Cádena de 7 caracteres con el código de distrito (incluye
#'     provincia y  municipio).}
#'   \item{OBS}{Observaciones por parte del proveedor de los datos.}
#'   \item{NPRO}{Nombre de la provincia.}
#'   \item{NCA}{Nombre de la comunidad autónoma.}
#'   \item{NMUN}{Nombre del municipio.}
#'   \item{geometry}{Columna de tipo lista con la geometría asociada a cada
#'     sección censal.}
#'   \item{cluster_id}{Código de identificación del cluster de uniones.}
#'   \item{sc_unida}{Código de las secciones unidas.}
#'
#'   El segundo elemento de la lista es un objeto de clase
#'   \code{poblaciones_ine} donde las filas representan las distintas secciones
#'     censales. Las tres primeras columnas son:
#'   \item{seccion}{Código de la sección censal en el primer año.}
#'   \item{sexo}{Código de la sección censal en el segundo año.}
#'   \item{year}{Primer año.}
#'   El resto de columnas representan los distintos grupos de edad, tras
#'     realizar el corte en los grupos de edad (85 0 100).
#'
#' @examples
#'
#' \dontrun{
#'   data(poblacion)
#'   data(cambios_seccion)
#'   data(cartografia)
#'   uniones <- une_secciones(cambios_seccion, poblacion, cartografia, 2006:2016)
#'
#'   poblacion   <- uniones$poblacion
#'   cartografia <- uniones$cartografia
#' }
#'
#' @encoding UTF-8
#'
#' @export
une_secciones <- function(cambios, poblacion, cartografia,
                          years = 2001:2016, corte_edad = 85) {

  if (!"cambios_ine" %in% class(cambios))
    stop("El objeto 'cambios' debe ser de clase 'cambios_ine'.")
  if (!"poblaciones_ine" %in% class(poblacion))
    stop("El objeto 'poblacion' debe ser de clase 'poblaciones_ine'.")
  if (!is.numeric(years) & length(years) < 2)
    stop("El objeto 'years' debe ser un vector numeric de longitud >= 2.")
  if (!2011 %in% years)
    stop("2011 debe estar incluido en el objeto 'years'.")
  if (!"sf" %in% class(cartografia))
    stop("El objeto 'cartografia' debe ser de clase 'sf'.")
  years <- sort(years)
  if (any(years != min(years):max(years)))
    stop("El rango de years debe ser continuo (sin saltos mayores a uno).")
  stopifnot(corte_edad %in% c(85, 100))

  pob_class <- class(poblacion)
  car_class <- class(cartografia)

  cambios    <- cambios[between(year, years[1], years[length(years)])]
  poblacion  <- poblacion[between(year, years[1], years[length(years)])]
  poblacion  <- elige_corte(poblacion, corte_edad)
  sc_unicas  <- sort(unique(poblacion$seccion))
  cluster_sc <- data.table(sc = sc_unicas, id_cluster = sc_unicas)

  for (i in seq_len(nrow(cambios))) {
    sc_select <- which(cluster_sc[, sc] %in%
                         cambios[i, c(sc_old, sc_new)])
    sc_min    <- min(cluster_sc[sc_select, id_cluster])
    sc_assign <- which(cluster_sc[, id_cluster] %in%
                         cluster_sc[sc_select, id_cluster])
    cluster_sc[sc_assign, id_cluster := sc_min]
  }

  cartografia$cluster <- cluster_sc[
    match(cartografia$CUSEC, cluster_sc[, sc]),
    id_cluster
  ]
  cartografia <- aggregate(
    x        = cartografia,
    by       = list(cartografia$cluster),
    FUN      = function(x) x[[1]],
    do_union = TRUE,
    simplify = TRUE
  )
  cartografia <- sf::st_cast(cartografia, "MULTIPOLYGON")
  cartografia[, c("Group.1", "cluster")] <- NULL

  poblacion[,
            cluster := cluster_sc[
              match(poblacion$seccion, cluster_sc[, sc]),
              id_cluster
            ]]
  in_col <- names(poblacion)[
    !names(poblacion) %in% c("seccion", "sexo", "year", "cluster")
  ]
  poblacion <- poblacion[,
                         lapply(.SD, sum),
                         by      = .(cluster, sexo, year),
                         .SDcols = in_col]
  setnames(poblacion, "cluster", "seccion")

  class(cartografia) <- car_class
  class(poblacion)   <- pob_class
  attributes(cartografia)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
  attributes(poblacion)$fuente   <- "Fuente: Sitio web del INE: www.ine.es"
  return(list(cartografia = cartografia, poblacion = poblacion))
}
