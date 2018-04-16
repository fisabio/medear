
#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE (tomando como referencia
#'   la cartograf�a INE 2011), adaptando a su vez las poblaciones por sexo a�o y
#'   secci�n censal. Si el archivo de cambios incorpora informaci�n catastral
#'   (n�mero de viviendas afectada por cada cambio de secci�n), se puede fijar
#'   un umbral de cambio (\%) para rechazar aquellos cambios que involucren a
#'   muy pocas viviendas.
#'
#' @details La funci�n trabaja con la siguiente din�mica:
#'
#'   \itemize{ \item Filtrado del archivo de cambios seg�n el rango de a�os
#'   escogido. \item Para cada cambio, si ambas secciones existen en la
#'   cartograf�a proporcionada se calculan las distancias (en metros) entre
#'   ellas. \item Si se ha decidido utilizar el filtro de catastro, se calcula
#'   el porcentaje de viviendas implicadas en cada cambio (opci�n no disponible
#'   para Euskadi y Navarra), y se procede al filtrado del archivo de cambios
#'   seg�n el umbral escogido en la llamada a la funci�n, asegurando siempre la
#'   presencia de aquellos cambios que implequen a secciones que no existan en
#'   la cartograf�a proporcionada, y restringiendo el filtrado a secciones que
#'   disten menos de 100 metros entre s�. \item  En caso de no utilizar el
#'   filtro de catastro, se filtra el archivo de cambios asegurando siempre la
#'   presencia de aquellos cambios que implequen a secciones que no existan en
#'   la cartograf�a proporcionada, y restringiendo el filtrado a secciones que
#'   disten menos de 100 metros entre s�. \item Una vez que se dispone del
#'   archivo de cambios definitivo, se crean las agrupaciones de secciones, y se
#'   realiza la uni�n de las mismas en la cartograf�a. \item Si se proporciona
#'   un archivo de poblaciones, se agrega la poblaci�n empleando las mismas
#'   agrupaciones de secciones del punto previo.}
#'
#'   No obstante, y dado que la funci�n asume que el callejero, el archivo de
#'   poblaciones y la cartograf�a est�n libres de errores. Como puede
#'   imaginarse, esto no es as�, de modo que la funci�n puede comportarse de
#'   forma inestable en dos supuestos:
#'
#'   \enumerate{ \item Cuando se quiera unir cambios no solo en la cartograf�a
#'   sino tambi�n en los datos de poblaci�n, puede aparecer un comportamiento
#'   inestable de la funci�, debido a divergencias existentes en la informaci�n
#'   contenida en los datos de poblaci�n y en los trameros (que es desde donde
#'   se crea el listado de cambios de secci�n), a pesar de que en ambos casos la
#'   fuente de informaci�n es el propio INE.
#'
#'   Lo anterior se traduce en que, para determinadas consultas, el n�mero de
#'   secciones contenidas en los datos de cartograf�a y poblaciones no ser� el
#'   mismo. Cuando esto pase (si pasa) la funci�n devolver� un aviso, indicando
#'   qu� secciones se ven afectadas y en qu� a�os, de forma que el usuario pueda
#'   tratar de solucionarlo por su cuenta, aunque no hay una soluci�n perfecta.
#'
#'   Las dos soluciones m�s efectivas (aunque son soluciones \emph{ad hoc} y
#'   recae en el usuario encontrar la m�s apropiada para su consulta concreta)
#'   que se han encontrado son:
#'
#'   \itemize{ \item modificar los criterios temporales de la consulta,
#'   ampliando o reduciendo el marco temporal (p. ej., pasar de un per�odo
#'   2001:2015 a 1996:2015 o 2002:2014); \item consultar las secciones
#'   problem�ticas (accesibles mediante la consulta \code{attr(objeto_devuelto,
#'   "sc_not_in_cartografia")}) en los datos de poblaci�n y, en base al archivo
#'   de cambios de secci�n, decidir con qu� secci�n se deber�a realizar la
#'   uni�n.}
#'
#'   \item Por otra parte, es posible encontrar v�as que aparecen literalmente
#'   "de la nada", especialmente en barrios de nueva creaci�n o gran expansi�n.
#'   El proceso de detecci�n de cambios de secci�n (funci�n
#'   \code{\link{detecta_cambios}}) compara las secciones a las que se asigna
#'   cada tramo del callejero de 2011, con las secciones a las que se asignan
#'   esos mismos tramos (u otros pero contengan portales asociados a los tramos
#'   previos) en los callejeros del resto de a�os.
#'
#'   No obstante, esto plantea un problema en la detecci�n de cambios al
#'   considerar la aparici�n de v�as completamente nuevas, puesto que la
#'   comparaci�n 2011-otros a�os no es posible. En esos casos, y siempre que no
#'   haya uniones adicionales que resuelvan el problema por s� solo, el archivo
#'   de poblaciones tras la uni�n contendr� valores iguales a uno en todas las
#'   categor�as de edad para los a�os anteriores a la creaci�n de la v�a.
#'   Nuevamente, cuando esto pase (si pasa) la funci�n devolver� un aviso,
#'   indicando qu� secciones se ven afectadas y en qu� a�os, de forma que el
#'   usuario pueda tratar de solucionarlo por su cuenta.
#'
#'   La soluci�n a este problema es similar al lo anteriormente expuesto: por un
#'   lado se puede variar el rango de a�os, y por otro tratar de solucionarlo
#'   manualmente consultando el archivo de cambios de secci�n y el de
#'   poblaciones, buscando las secciones que devuelva la consulta
#'   \code{attr(resultado, "pob_igual_uno")}.}
#'
#'   En el apartado de ejemplos se desarrollar�n los abordajes a estos
#'   problemas, con un tratamiento m�s extenso en la vi�eta de uni�n de
#'   seccionado (a�n por elaborar).
#'
#' @param cambios Objeto de clase \code{cambios_ine}.
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}}, y con
#'   datos de clase \code{cartografia_ine}.
#' @param years Vector num�rico de longitud >= 1 con los a�os para los que se
#'   desee consultar las variaciones de seccionado. El a�o 2011 debe figurar
#'   dentro del vector, cuyo rango debe ser continuo (sin saltos de m�s de un
#'   a�o).
#' @param poblacion Objeto de clase \code{poblaciones_ine}. Argumento opcional a
#'   proporcionar en caso de querer agregar las poblaciones.
#' @param corte_edad Num�rico: punto de corte para los grupos de edad (85 o
#'   100). Argumento opcional en caso de proporcionar datos de poblaciones.
#' @param catastro L�gico: �Debe aplicarse el filtro de informaci�n catastral?
#'   Por defecto \code{catastro = FALSE}.
#' @param umbral_vivienda Num�rico: porcentaje de viviendas afectadas en el
#'   cambio de secci�n. Solo se utiliza si \code{catastro = TRUE}. Por defecto
#'   se fija al 5 \%.
#' @param distancia Num�rico: M�xima distancia (en metros) de separaci�n entre
#'   secciones. Por defecto se fija en 100 metros. En algunos casos
#'   (principalmente en ciudades donde no haya problemas con pedan�as que
#'   compartan nombres de v�a con el n�cleo urbano principal) puede ser
#'   conveniente aumentar este par�metro.
#'
#' @usage une_secciones(cambios, cartografia, years = 1996:2016, poblacion =
#'   NULL, corte_edad = 85, catastro = FALSE, umbral_vivienda = 5, distancia =
#'   100)
#'
#' @return El resultado devuelto var�a en funci�n de si se proporcionan datos de
#'   poblaciones o no. Si no se proporcionan se devuelve un objeto de clase
#'   \code{cartografia_ine} y \code{\link[sp]{SpatialPolygonsDataFrame}} con la
#'   cartograf�a, donde cada fila es una secci�n censal y que cuenta con 9
#'   columnas: \describe{\item{seccion}{Cadena de 10 caracteres con el c�digo de
#'   secci�n censal (incluye provincia, municipio y distrito).}
#'   \item{CUMUN}{Cadena de 5 caracteres con el c�digo del municipio (incluye
#'   provincia).} \item{CCA}{Cadena de 2 caracteres con el c�digo de comunidad
#'   aut�noma.} \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre de la
#'   comunidad aut�noma.} \item{NMUN}{Nombre del municipio.}
#'   \item{geometry}{Columna de tipo lista con la geometr�a asociada a cada
#'   secci�n censal.} \item{cluster_id}{C�digo de identificaci�n del cluster de
#'   uniones.} \item{sc_unida}{C�digo de las secciones unidas.}}
#'
#'   En caso de proporcionan poblaciones, se devuelve una lista de longitud
#'   igual a dos, donde el primer elemento es la cartograf�a descrita
#'   anteriormente y el segundo elemento de la lista es un objeto de clase
#'   \code{poblaciones_ine} donde las filas representan las distintas secciones
#'   censales. Las tres primeras columnas son: \describe{\item{seccion}{C�digo
#'   de la secci�n censal.} \item{sexo}{Sexo de la poblaci�n (0 = masculino; 1 =
#'   femenino).} \item{year}{A�o de referencia.}} El resto de columnas
#'   representan los distintos grupos de edad, tras realizar el corte en los
#'   grupos de edad (85 0 100).
#'
#' @examples
#'
#' \dontrun{
#'   # En este ejemplo se trabaja con la ciudad de Sevilla (c�digo ine: 41091)
#'   # en los a�os 2004-2015
#'
#'   library(medear)
#'   data("poblacion")
#'   data("cambios_seccion")
#'   data("cartografia")
#'   cambios_se     <- cambios_seccion[substr(cambios_seccion$sc_ref, 1, 5) == "41091", ]
#'   cartografia_se <- cartografia[cartografia$CUMUN == "41091", ]
#'   poblacion_se   <- poblacion[substr(poblacion$seccion, 1, 5) == "41091", ]
#'
#'   ##########################################################################
#'   ## Ejemplo sin utilizar el filtro de catastro                           ##
#'   ##########################################################################
#'
#'   union_sin_cat <- une_secciones(
#'     cambios         = cambios_se,
#'     cartografia     = cartografia_se,
#'     years           = 2004:2015,
#'     poblacion       = poblacion_se,
#'     catastro        = FALSE,
#'     distancia       = 100
#'   )
#'
#'   nrow(union_sin_cat$cartografia) # 402 secciones
#'   length(unique(union_sin_cat$poblacion$seccion)) # 408 secciones
#'   round(nrow(union_sin_cat$cartografia) / nrow(cartografia_se) * 100) #
#'   Conserva el 76 \% de secciones
#'
#'   # La funci�n avisa acerca de divergencias en el seccionado entre
#'   cartograf�a y el archivo de poblaciones. # Las secciones afectadas son
#'   accesibles mediante la siguiente consulta:
#'   attributes(union_con_cat)$sc_not_in_cartografia # 12 SC problem�ticas
#'
#'   # En este caso, se resolver� la primera incidencia (SC 4109102089).
#'   sc_problematica <- attributes(union_con_cat)$sc_not_in_cartografia[1]
#'
#'   ## POR COMPLETAR ##
#'
#'
#'   ##########################################################################
#'   ## Ejemplo utilizando el filtro de catastro                             ##
#'   ##########################################################################
#'
#'   union_con_cat <- une_secciones(
#'     cambios         = cambios_se,
#'     cartografia     = cartografia_se,
#'     years           = 2004:2015,
#'     poblacion       = poblacion_se,
#'     catastro        = TRUE,
#'     umbral_vivienda = 5,
#'     distancia       = 100
#'   )
#'
#'   nrow(union_con_cat$cartografia) # 466 secciones
#'   length(unique(union_con_cat$poblacion$seccion)) # 478 secciones
#'   round(nrow(union_con_cat$cartografia) / nrow(cartografia_se) * 100) # Conserva el 88 \% de secciones
#'
#'   ## POR COMPLETAR ##
#'
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
                          umbral_vivienda = 5, distancia = 100) {

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
  stopifnot(is.numeric(distancia))
  stopifnot(is.logical(catastro))
  stopifnot(any(cambios$sc_ref %in% cartografia$seccion))
  stopifnot(!is.null(poblacion) && any(cambios$sc_ref %in% poblacion$seccion))

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
    islas_2011    <- sapply(cartografia@polygons, function(x) length(x@Polygons))
    sc_islas_2011 <- cartografia@data[which(islas_2011 != 1), "seccion"]
    carto_metro   <- sp::spTransform(
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
    cambios[, distan_T := (dista < distancia | is.na(dista))]

    if (catastro) {
      for (i in seq_len(nrow(cambios))) {
        viv_r    <- secciones_2011[seccion == cambios[["sc_ref"]][i], n_viv]
        cambios[i, viv_ref := ifelse(length(viv_r) != 0, viv_r, NA_integer_)]
        cambios[i, cambio_ref := (viviendas / viv_ref * 100)]
      }
      cambios[, umbral := cambio_ref + tramo_por]
      cambios[, umbral_T := umbral >= umbral_vivienda]
      cambios[, incluido := no_11 == TRUE | (umbral_T & distan_T)]
      cambios_copy <- copy(cambios)
      cambios <- cambios[incluido == TRUE]
    } else {
      cambios[, incluido := no_11 == TRUE | distan_T]
      cambios_copy <- copy(cambios)
      cambios <- cambios[incluido == TRUE]
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
    cartografia$Group.1 <- NULL

    islas_union    <- which(sapply(cartografia@polygons, function(x) length(x@Polygons)) != 1)
    sc_islas_union <- cartografia@data[islas_union, "seccion"]
    all_sc_islas   <- strsplit(cartografia@data[islas_union, "cluster_id"], "-")
    sc_comparacion <- ifelse(length(sc_islas_2011) == 0, TRUE, FALSE)
    if (!sc_comparacion & length(sc_islas_union) > 0) {
      sc_comparacion <- sapply(all_sc_islas, function(x) any(x %in% sc_islas_2011))
    }
    if ((length(sc_islas_union) > 0 & (length(sc_islas_union) > length(sc_islas_2011))) | any(!sc_comparacion)) {
      if (all(length(all_sc_islas) > 0, length(sc_islas_2011) > 0)) {
        islas_alerta <- sc_islas_union[!sc_comparacion]
      } else if (length(all_sc_islas) > 0) {
        islas_alerta <- sc_islas_union
      }
      cartografia$revision_manual[cartografia$seccion %in% islas_alerta] <- "Revisar manualmente"
      warning(
        "Las secciones de la cartograf\u00eda: c('", paste(islas_alerta, collapse = "-"), "'),",
        " est\u00e1n conformadas por varios pol\u00edgonos que no son colindantes.\n",
        "Por favor, rev\u00edselas manualmente (consulte la ayuda de la funci\u00f3n).",
        call. = FALSE
      )
    }
  } else if (is.null(poblacion)) {
    attributes(cartografia@data)$cambios <- cambios_copy
    message("En el per\u00edodo establecido no se ha detectado ning\u00fan cambio: ",
            "se devuelve la misma cartograf\u00eda.")
  }
  attributes(cartografia@data)$fuente  <- fuente
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
        "En el per\u00edodo establecido no se ha detectado ning\u00fan cambio: ",
        "se devuelve la misma cartograf\u00eda y poblaci\u00f3n para ese per\u00edodo, ",
        "ajustando esta \u00faltima al corte de edad marcado."
      )
    }

    attributes(poblacion)$fuente <- fuente
    res                          <- list(
      cartografia = cartografia, poblacion = poblacion
    )
    attributes(res)$fuente       <- fuente
    if (exists("cluster_sc")) {
      attributes(res)$cluster    <- cluster_sc
      attributes(res)$cambios    <- cambios_copy
    }

    if (!identical(sort(cartografia$seccion), sort(unique(poblacion$seccion)))) {
      not_in_pobla <- cartografia$seccion[!cartografia$seccion %in% unique(poblacion$seccion)]
      not_in_carto <- unique(poblacion$seccion)[!unique(poblacion$seccion) %in% cartografia$seccion]
      if (length(not_in_pobla) > 0) {
        warning(
          "Tras realizar la uni\u00f3n con las opciones marcadas, las secciones c('",
          paste0(not_in_pobla, collapse = "', '"),
          "') aparecen en la cartograf\u00eda pero no en los datos de poblaci\u00f3n.\n",
          "Por favor, consulte la ayuda de la funci\u00f3n para tratar de solucionarlo.",
          call. = FALSE
        )
        attr(res, "sc_not_in_poblacion") <- not_in_pobla
      }
      if (length(not_in_carto) > 0) {
        not_in_years <- unique(poblacion[seccion %in% not_in_carto, year])
        warning(
          "Tras realizar la uni\u00f3n con las opciones marcadas, las secciones c('",
          paste0(not_in_carto, collapse = "', '"),
          "') aparecen en los datos de poblaci\u00f3n pero no en la cartograf\u00eda, ",
           " para los a\u00f1os c(", paste0(not_in_years, collapse = ", "), ").\n",
          "Por favor, consulte la ayuda de la funci\u00f3n para tratar de solucionarlo.",
          call. = FALSE
        )
        attr(res, "sc_not_in_cartografia") <- not_in_carto
        attr(attr(res, "sc_not_in_cartografia"), "years") <- not_in_years
      }
    }

    uno_vect <- rowSums(poblacion[, -c(1:3)]) == sum(ncol(poblacion[, -c(1:3)]))

    if (any(uno_vect)) {
      warning(
        "En el per\u00edodo seleccionado algunas secciones no sufrieron cambios pero ",
        "aparecieron m\u00e1s tarde que el a\u00f1o de inicio elegido. Se asigna el valor 1 como ",
        "poblaci\u00f3n a dichas secciones para los a\u00f1os previos (hasta el a\u00f1o de inicio fijado).\n",
        "Por favor, consulte la ayuda de la funci\u00f3n para explorar este aspecto.",
        call. = FALSE
      )
      attr(res, "pob_igual_uno") <- unique(poblacion[uno_vect, seccion])
    }
  }

  return(res)
}
