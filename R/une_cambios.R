
#' @title Une los cambios del seccionado del INE
#'
#' @description Une los cambios del seccionado del INE (tomando como referencia
#'   la cartografía INE 2011), adaptando a su vez las poblaciones por sexo,
#'   año, grupo de edad y sección censal o la mortalidad por sexo, año de
#'   defunción, grupo de edad, sección censal y causa de defunción (siempre que
#'   se desee, pues son argumentos opcionales). Si el archivo de cambios
#'   incorpora información catastral (número de viviendas afectada por cada
#'   cambio de sección), se puede fijar un umbral de cambio (\%) para rechazar
#'   aquellos cambios que involucren a muy pocas viviendas.
#'
#' @details La función trabaja con la siguiente dinámica:
#'
#'   \itemize{ \item Filtrado del archivo de cambios según el rango de años
#'   escogido. \item Para cada cambio, si ambas secciones existen en la
#'   cartografía proporcionada se calculan las distancias (en metros) entre
#'   ellas. \item Si se ha decidido utilizar el filtro de catastro, se calcula
#'   el porcentaje de viviendas implicadas en cada cambio (opción no disponible
#'   para Euskadi y Navarra), y se procede al filtrado del archivo de cambios
#'   según el umbral escogido en la llamada a la función, asegurando siempre la
#'   presencia de aquellos cambios que implequen a secciones que no existan en
#'   la cartografía proporcionada, y restringiendo el filtrado a secciones que
#'   disten menos de 100 metros entre sí. \item  En caso de no utilizar el
#'   filtro de catastro, se filtra el archivo de cambios asegurando siempre la
#'   presencia de aquellos cambios que implequen a secciones que no existan en
#'   la cartografía proporcionada, y restringiendo el filtrado a secciones que
#'   disten menos de 100 metros entre sí. \item Una vez que se dispone del
#'   archivo de cambios definitivo, se crean las agrupaciones de secciones, y se
#'   realiza la unión de las mismas en la cartografía. \item Si se proporciona
#'   un archivo de poblaciones, se agrega la población empleando las mismas
#'   agrupaciones de secciones. \item Si se proporciona un archivo de
#'   mortalidad geocodificada, se agregan las defunciones empleando las mismas
#'   agrupaciones de secciones.}
#'
#'   No obstante, y dado que la función asume que el callejero, el archivo de
#'   poblaciones y la cartografía están libres de errores. Como puede
#'   imaginarse, esto no es así, de modo que la función puede comportarse de
#'   forma inestable en dos supuestos:
#'
#'   \enumerate{ \item Cuando se quiera unir cambios no solo en la cartografía
#'   sino también en los datos de población, puede aparecer un comportamiento
#'   inestable de la función, debido a divergencias existentes en la información
#'   contenida en los datos de población y en los trameros (que es desde donde
#'   se crea el listado de cambios de sección), a pesar de que en ambos casos la
#'   fuente de información es el propio INE.
#'
#'   Lo anterior se traduce en que, para determinadas consultas, el número de
#'   secciones contenidas en los datos de cartografía y poblaciones no será el
#'   mismo. Cuando esto pase (si pasa) la función devolverá un aviso, indicando
#'   qué secciones se ven afectadas y en qué años, de forma que el usuario pueda
#'   tratar de solucionarlo por su cuenta, aunque no hay una solución perfecta.
#'
#'   Las dos soluciones más efectivas (aunque son soluciones \emph{ad hoc} y
#'   recae en el usuario encontrar la más apropiada para su consulta concreta)
#'   que se han encontrado son:
#'
#'   \itemize{ \item modificar los criterios temporales de la consulta,
#'   ampliando o reduciendo el marco temporal (p. ej., pasar de un período
#'   2001:2015 a 1996:2015 o 2002:2014); \item consultar las secciones
#'   problemáticas (accesibles mediante la consulta \code{attr(objeto_devuelto,
#'   "sc_not_in_cartografia")}) en los datos de población y, en base al archivo
#'   de cambios de sección, decidir con qué sección se debería realizar la
#'   unión.}
#'
#'   \item Por otra parte, es posible encontrar vías que aparecen literalmente
#'   "de la nada", especialmente en barrios de nueva creación o gran expansión.
#'   El proceso de detección de cambios de sección (función
#'   \code{\link{detecta_cambios}}) compara las secciones a las que se asigna
#'   cada tramo del callejero de 2011, con las secciones a las que se asignan
#'   esos mismos tramos (u otros pero contengan portales asociados a los tramos
#'   previos) en los callejeros del resto de años.
#'
#'   No obstante, esto plantea un problema en la detección de cambios al
#'   considerar la aparición de vías completamente nuevas, puesto que la
#'   comparación 2011-otros años no es posible. En esos casos, y siempre que no
#'   haya uniones adicionales que resuelvan el problema por sí solo, el archivo
#'   de poblaciones tras la unión contendrá valores iguales a uno en todas las
#'   categorías de edad para los años anteriores a la creación de la vía.
#'   Nuevamente, cuando esto pase (si pasa) la función devolverá un aviso,
#'   indicando qué secciones se ven afectadas y en qué años, de forma que el
#'   usuario pueda tratar de solucionarlo por su cuenta.
#'
#'   La solución a este problema es similar al lo anteriormente expuesto: por un
#'   lado se puede variar el rango de años, y por otro tratar de solucionarlo
#'   manualmente consultando el archivo de cambios de sección y el de
#'   poblaciones, buscando las secciones que devuelva la consulta
#'   \code{attr(resultado, "pob_igual_uno")}.}
#'
#'   En el apartado de ejemplos se desarrollarán los abordajes a estos
#'   problemas, con un tratamiento más extenso en la viñeta de unión de
#'   seccionado (aún por elaborar).
#'
#' @param cambios Objeto de clase \code{cambios_ine}. Por defecto se le asigna
#'   el valor NULL para no realizar ninguna unión y trabajar con la cartografía
#'   proporcionada.
#' @param cartografia Objeto de clase \code{\link[sp]{SpatialPolygons}} con
#'   proyección asignada (código EPSG).
#' @param poblacion Objeto de clase \code{poblaciones_ine}. Argumento opcional a
#'   proporcionar en caso de querer agregar las poblaciones.
#' @param mortalidad Objeto con los registros de mortalidad geocodificados. Este
#'    es un argumento opcional a proporcionar en caso de querer agregar la
#'   mortalidad por sección censal. Se requiere que los datos tengan, como
#'   mínimo, las variables sexo, año de defunción, edad, causa de defunción y
#'   el par de coordenadas (longitud y latitud), y que tengan exactamente los
#'   siguientes nombres: 'sexo', 'year_defuncion', 'edad', 'causa_defuncion',
#'   'lng', y 'lat', respectivamente.
#' @param otras_causas Véctor de caracteres que indica los nombres de las
#'   columnas (columnas con valor 0-1) en la base de datos de mortalidad que
#'   identifican a dichas otras causas.
#' @param medea3 Valor lógico: ¿desea que se agrupen las causas de mortalidad
#'   siguiendo el patron de 22 grandes causas de MEDEA3? Este argumento es
#'   compatible con \code{otras_causas}.
#' @param years_estudio Vector numérico de longitud >= 1 con los años para los que se
#'   desee construir el \code{array} de poblaciones o mortalidad.
#' @param years_union Vector numérico de longitud >= 1 con los años para los que se
#'   desee consultar las variaciones de seccionado. El año 2011 debe figurar
#'   dentro del vector, cuyo rango debe ser continuo (sin saltos de más de un
#'   año).
#' @param epsg Vector numérico de longitud uno con el código EPSG del sistema de
#'   referencia de coordenadas (CRS) empleado en los datos de mortalidad (por
#'   defecto se usa el 4326 con datum WGS84).
#' @param corte_edad Numérico: punto de corte para los grupos de edad (85 o
#'   100). Argumento opcional en caso de proporcionar datos de poblaciones o
#'   mortalidad.
#' @param catastro Lógico: En caso de realizar uniones ¿debe aplicarse el filtro
#'   de información catastral? Por defecto \code{catastro = FALSE}.
#' @param umbral_vivienda Numérico: porcentaje de viviendas afectadas en el
#'   cambio de sección. Solo se utiliza si \code{catastro = TRUE}. Por defecto
#'   se fija al 5 \%.
#' @param distancia Numérico: En caso de realizar uniones, máxima distancia (en
#'   metros) de separación entre secciones. Por defecto se fija en 100 metros. En
#'   algunos casos (principalmente en ciudades donde no haya problemas con pedanías
#'   que compartan nombres de vía con el núcleo urbano principal) puede ser
#'   conveniente aumentar este parámetro.
#' @param modo Carácter: En caso de realizar uniones, modalidad de la unión
#'   (\code{auto} o \code{manual}), por defecto \code{auto}. Si se escoge el modo
#'   \code{auto} la función detectará los cambios que conllevan una unión de
#'   secciones según los parámetros que el usuario haya elegido; si se escoge el
#'   modo \code{manual}, además de realizar todo el proceso que involucra el otro
#'   modo, se permite al usuario incluir uniones que el modo \code{auto} puede
#'   haber pasado por alto, realizando uniones de seccionado 1-1 mediante los
#'   argumentos \code{sc1} y \code{sc2}.
#' @param sc1 Vector de caracteres: Primer bloque de secciones a unir
#'   manualmente, vacío por defecto. Si se proporciona debe tener la misma
#'   longitud que \code{sc2}.
#' @param sc2 Vector de caracteres: Segundo bloque de secciones a unir
#'   manualmente, vacío por defecto. Si se proporciona debe tener la misma
#'   longitud que \code{sc1}.
#'
#' @usage une_secciones(cambios = NULL, cartografia, poblacion = NULL, mortalidad = NULL,
#' otras_causas = NULL, medea3 = TRUE, years_estudio = 1996:2015,
#' years_union = years_estudio, epsg = 4326, corte_edad = 85,
#' catastro = FALSE, umbral_vivienda = 5, distancia = 100,
#' modo = c("auto", "manual"), sc1 = NULL, sc2 = NULL)
#'
#' @return El resultado devuelto varía en función de si se proporcionan datos de
#'   poblaciones o mortalidad. Si no se proporciona ninguno se devuelve un
#'   objeto de clase \code{\link[sp]{SpatialPolygons}} con la cartografía,
#'   donde cada fila es una sección censal y que puede tener las columnas:
#'   \describe{\item{seccion}{Cadena de 10 caracteres con el código
#'   de sección censal (incluye provincia, municipio y distrito).}
#'   \item{CUMUN}{Cadena de 5 caracteres con el código del municipio (incluye
#'   provincia).} \item{CCA}{Cadena de 2 caracteres con el código de comunidad
#'   autónoma.} \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre de la
#'   comunidad autónoma.} \item{NMUN}{Nombre del municipio.}
#'   \item{cluster_id}{Código de identificación del cluster de
#'   uniones.} \item{revision_manual}{Indica si debe revisarse esa unión.}}
#'
#'   En caso de proporcionar poblaciones, se devuelve una lista de longitud
#'   igual a dos, donde el primer elemento es la cartografía descrita
#'   anteriormente y el segundo elemento de la lista es un objeto de clase
#'   \code{array} con cuatro dimensiones: año de defunción, sexo (0 =
#'   masculino; 1 = femenino), grupo de edad (según corte establecido) y
#'   sección censal.
#'
#'   En caso de proporcionar datos de mortalidad geocodificada, se devuelve una
#'   lista de longitud igual a tres, donde los primeros elementos son los
#'   anteriormente descritos y el tercer elemento es un objeto de clase
#'   \code{array} con cinco dimensiones: año de defunción, sexo, grupo de edad
#'   (según corte establecido), sección censal y causa de muerte.
#'
#' @examples
#'
#' \dontrun{
#'   # En este ejemplo se trabaja con la ciudad de Córdoba (código ine: 14021)
#'   # en los años 2004-2015, sin usar códigos postales ni mortalidad.
#'
#'   library(medear)
#'   data("poblacion")
#'   data("cambios_seccion")
#'   data("cartografia")
#'   cartografia_co <- cartografia[cartografia$CUMUN == "14021", ]
#'   poblacion_co   <- poblacion[substr(poblacion$seccion, 1, 5) == "14021", ]
#'   cambios_co     <- cambios_seccion[
#'     substr(cambios_seccion$sc_ref, 1, 5) == "14021" & cambios_seccion$codigo_postal == FALSE,
#'   ]
#'
#'   ##########################################################################
#'   ## Ejemplo sin utilizar el filtro de catastro                           ##
#'   ##########################################################################
#'
#'   union_sin_cat <- une_secciones(
#'     cambios       = cambios_co,
#'     cartografia   = cartografia_co,
#'     years_estudio = 2004:2015,
#'     poblacion     = poblacion_co,
#'     catastro      = FALSE
#'   )
#'
#'   nrow(union_sin_cat$cartografia) # 215 secciones
#'   round(nrow(union_sin_cat$cartografia) / nrow(cartografia_co) * 100) #
#'   Conserva el 88 \% de secciones
#'
#'   # La función avisa acerca de divergencias en el seccionado entre
#'   cartografía y el archivo de poblaciones.
#'   # Las secciones afectadas son accesibles mediante la siguiente consulta:
#'   attributes(union_sin_cat)$pob_igual_uno # 1 SC problemática
#'
#'   # En este caso, se resolverá la incidencia (SC 1402106047).
#'   sc_problematica <- attributes(union_sin_cat)$pob_igual_uno
#'
#'   # Hay que revisar el archivo de cambios para comprobar si esta SC está
#'   # involucrada en algún otro cambio de sección:
#'   cambios_res <- attributes(union_sin_cat)$cambios
#'   cambios_res[cambios_res$sc_ref == sc_problematica]
#'   cambios_res[cambios_res$sc_new == sc_problematica]
#'
#'   # La SC debería unirse con la SC 1402106024, aunque ambas están a 104.9
#'   # metros. Comprobemos esa otra sección:
#'   cambios_res[cambios_res$sc_ref == "1402106024"]
#'   cambios_res[cambios_res$sc_new == "1402106024"]
#'
#'   # La SC 1402106024 se une con la SC 1402106050 (no está en la cartografía
#'   # de 2001) y con la 1402106038. Al representar estas SC (1402106047,
#'   # y la unión 1402106024-1402106038), vemos que colindan, así que se puede
#'   # realizar la unión manual:
#'   plot(union_sin_cat$cartografia[union_sin_cat$cartografia$seccion %in%
#'     c("1402106047", "1402106024"), ])
#'   union_sin_cat <- une_secciones(
#'     cambios       = cambios_co,
#'     cartografia   = cartografia_co,
#'     years_estudio = 2004:2015,
#'     poblacion     = poblacion_co,
#'     catastro      = FALSE,
#'     modo          = "manual",
#'     sc1           = "1402106047",
#'     sc2           = "1402106024"
#'   )
#'
#'   # El aviso ha desaparecido, aunque todavía se mantiene un aviso sobre
#'   # secciones conformadas por varios polígonos. Vamos a ver qué secciones
#'   # forman esta unión:
#'   union_sin_cat$cartografia@data[union_sin_cat$cartografia$seccion == "1402101006", ]
#'
#'   # La sección involucrada es 1402106010. Al representar ambas secciones
#'   # vemos que sí se tocan, aunque solo en una esquina. A pesar del aviso,
#'   # damos por buena la unión.
#'   plot(union_sin_cat$cartografia[union_sin_cat$cartografia$seccion == "1402101006", ])
#'
#'   ##########################################################################
#'   ## Ejemplo utilizando el filtro de catastro                             ##
#'   ##########################################################################
#'
#'   union_con_cat <- une_secciones(
#'     cambios         = cambios_co,
#'     cartografia     = cartografia_co,
#'     years_estudio   = 2004:2015,
#'     poblacion       = poblacion_co,
#'     catastro        = TRUE,
#'     umbral_vivienda = 5
#'   )
#'
#'   nrow(union_con_cat$cartografia) # 223 secciones
#'   round(nrow(union_con_cat$cartografia) / nrow(cartografia_co) * 100)
#'   # Conserva el 91 \% de secciones
#'
#'   # Al utilizar información de catastro se ha aumentado de un 88 \% a un
#'   # 91 \%, recuperando 8 secciones. Surgen los mismos avisos que antes, así
#'   # que procedemos de la misma forma, tras lo cual se da por cerrada la unión:
#'   union_con_cat <- une_secciones(
#'     cambios         = cambios_co,
#'     cartografia     = cartografia_co,
#'     years_estudio   = 2004:2015,
#'     poblacion       = poblacion_co,
#'     catastro        = TRUE,
#'     umbral_vivienda = 5,
#'     modo            = "manual",
#'     sc1             = "1402106047",
#'     sc2             = "1402106024"
#'   )
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
une_secciones <- function(cambios = NULL, cartografia, poblacion = NULL, mortalidad = NULL,
                          otras_causas = NULL, medea3 = TRUE, years_estudio = 1996:2015,
                          years_union = years_estudio, epsg = 4326, corte_edad = 85,
                          catastro = FALSE, umbral_vivienda = 5, distancia = 100,
                          modo = c("auto", "manual"), sc1 = NULL, sc2 = NULL) {

  ##############################################################################
  ##  COMPROBACIONES INICIALES Y PREPARACION DE DATOS                         ##
  ##############################################################################

  modo <- match.arg(modo)
  if (!is.numeric(years_estudio) & length(years_estudio) < 0)
    stop("\nEl objeto 'years_estudio' debe ser un vector num\u00e9rico de longitud >= 1.")
  if (!is.numeric(years_union) & length(years_union) < 0)
    stop("\nEl objeto 'years_union' debe ser un vector num\u00e9rico de longitud >= 1.")
  if (!2011 %in% years_union)
    stop("\n2011 debe estar incluido en el objeto 'years_union'.")
  years_estudio <- sort(years_estudio)
  years_union   <- sort(years_union)
  if (any(years_estudio != min(years_estudio):max(years_estudio)))
    stop("\nEl rango de 'years_estudio' debe ser continuo (sin saltos mayores a uno).")
  if (any(years_union != min(years_union):max(years_union)))
    stop("\nEl rango de 'years_union' debe ser continuo (sin saltos mayores a uno).")
  stopifnot(corte_edad %in% c(85, 100))
  stopifnot(is.numeric(umbral_vivienda))
  stopifnot(is.numeric(distancia))
  stopifnot(is.logical(catastro))

  comprueba_datos(cartografia, "cartografia")

  if (!is.null(mortalidad)) {
    comprueba_datos(mortalidad, "mortalidad", periodo = years_estudio)
    if (is.null(poblacion) && !is.null(cambios)) {
      stop("\nPara calcular el cubo de mortalidad es preciso aportar los datos",
           " de poblaci\u00f3n y cambios.\nPor favor, revise los argumentos de la funci\u00f3n.")
    }
    mortalidad_c        <- copy(as.data.table(mortalidad))
    names(mortalidad_c) <- tolower(names(mortalidad_c))
    for (i in seq_along(mortalidad_c)) {
      set(mortalidad_c, j = i, value = as.character(mortalidad_c[[i]]))
    }
  }

  if (modo == "manual") {
    if (is.null(sc1) && is.null(sc2)) {
      stop("\nHa seleccionado modo manual, pero no ha proporcionado las ",
           "secciones a unir manualmente en los argumentos 'sc1' y 'sc2'.")
    }
    if (length(sc1) != length(sc2)) {
      stop("\nHa seleccionado modo manual, pero no ha proporcionado el mismo ",
           "n\u00famero de secciones en los argumentos 'sc1' y 'sc2'.")
    }
    sc1 <- as.character(sc1)
    sc2 <- as.character(sc2)
    if (any(nchar(sc1) != 10) || any(nchar(sc2) != 10)) {
      stop("\nHa seleccionado modo manual, pero alguna de las secciones que ha ",
           "proporcionado en los argumentos 'sc1' y 'sc2' no tienen 10 ",
           "caracteres (dos para provincia, tres para municipio, dos para ",
           "distrito y tres para secci\u00f3n.).\nPor favor, revise estos apartados.")
    }
  }

  datos_propios <- !identical(years_estudio, years_union)
  cartografia <- sp::spTransform(cartografia, sp::CRS(paste0("+init=epsg:", epsg)))
  utils::data("secciones", envir = environment(), package = "medear")
  if (!is.null(poblacion)) {
    secciones <- secciones[seccion %in% unique(c(cartografia$seccion, poblacion$seccion))]
  } else {
    secciones <- secciones[seccion %in% cartografia$seccion]
  }


  ##############################################################################
  ##  IDENTIFICACION DE SECCIONES AUSENTES EN POBLACION                       ##
  ##############################################################################

  if (!is.null(poblacion)) {
    comprueba_datos(poblacion, "poblacion", periodo = years_estudio)
    if (!is.null(cambios)) {
      if (!any(cambios$sc_ref %in% poblacion$seccion)) {
        stop("\nEl archivo de cambios de seccionado no contiene ninguna ",
             "secci\u00f3n coincidente con las del archivo de poblaci\u00f3n.\n",
             "Por favor, revise que los datos de cambios de seccionado hacen ",
             "referencia a secciones contenidas en los datos de poblaciones.")
      }
    }
    poblacion <- poblacion[between(year, min(years_estudio), max(years_estudio))]

    if (!all(unique(poblacion$seccion) %in% unique(secciones$seccion))) {
      sc_pob_not_sc <- unique(poblacion$seccion)[!unique(poblacion$seccion) %in% unique(secciones$seccion)]
      secciones <- unique(
        rbindlist(
          list(
            secciones,
            poblacion[sexo == 0 & seccion %in% sc_pob_not_sc, seccion, by = year]
          )
        )
      )[order(seccion, year)]
    }
    if (!all(unique(poblacion$year) %in% unique(secciones$year))) {
      year_not_sc <- unique(poblacion$year)[!unique(poblacion$year) %in% unique(secciones$year)]
      secciones <- unique(
        rbindlist(
          list(
            secciones,
            poblacion[sexo == 0 & year %in% year_not_sc, seccion, by = year]
          )
        )
      )[order(seccion, year)]
    }

    tmp1 <- tmp2 <- list()
    secciones[, tmp := FALSE]
    for (i in years_estudio) {
      tmp1[[paste(i)]] <- unique(secciones[year == i, seccion])
    }
    tmp2[[paste(last(years_estudio))]] <- secciones[year == last(years_estudio)][, tmp := TRUE]
    for (i in years_estudio[years_estudio != last(years_estudio)]) {
      tmp2[[paste(i)]] <- secciones[year == i]
      tmp2[[paste(i)]][!tmp1[[paste(i)]] %in% tmp1[[paste(i + 1)]] == TRUE, tmp := TRUE]
    }
    secciones <- rbindlist(tmp2)
    secciones[, final := last(years_estudio)]
    secciones[tmp == TRUE, final := as.integer(year)]
    secciones[, tmp := NULL]
    sc_pob_conservar <- secciones[between(final, 2012, max(year) - 1), seccion]
    secciones_2011   <- data.table(year = 2011, seccion = cartografia$seccion)
    secciones_2011   <- secciones_2011[, n_viv := cartografia@data[match(cartografia$seccion, seccion), "n_viv"]]
  }


  ##############################################################################
  ##  SELECCION DE CAMBIOS Y UNION DE SECCIONES EN CARTOGRAFIA                ##
  ##############################################################################

  if (!is.null(cambios)) {
    if (!"cambios_ine" %in% class(cambios))
      stop("\nEl objeto 'cambios' debe ser de clase 'cambios_ine', tal ",
           "cual se origina al usar la funci\u00f3n 'detecta_cambios'.")
    if (!any(cambios$sc_ref %in% cartografia$seccion)) {
      stop("\nEl archivo de cambios de seccionado no contiene ninguna ",
           "secci\u00f3n coincidente con las de la cartograf\u00eda.\n",
           "Por favor, revise que los datos de cambios de seccionado hacen ",
           "referencia a secciones contenidas en los datos de cartograf\u00eda")
    }
    cartografia <- cartografia[cartografia$CUMUN %in% substr(cambios$sc_ref, 1, 5), ]

    if ("vias" %in% names(cambios)) cambios$vias <- NULL

    cambios      <- cambios[between(year2, years_union[1], years_union[length(years_union)])]
    cambios$modo <- "auto"

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
      cambios[, distan_T := (dista <= distancia | is.na(dista))]

      if (catastro) {
        for (i in seq_len(nrow(cambios))) {
          viv_r    <- secciones_2011[seccion == cambios[["sc_ref"]][i], n_viv]
          cambios[i, viv_ref := ifelse(length(viv_r) != 0, viv_r, NA_integer_)]
          cambios[i, cambio_ref := (viviendas / viv_ref * 100)]
        }
        cambios[, umbral := cambio_ref + tramo_por]
        cambios[, umbral_T := umbral >= umbral_vivienda]
        cambios[, incluido := umbral_T == T & (no_11 | distan_T)]
        cambios_copy <- copy(cambios)
        cambios <- cambios[incluido == TRUE]
      } else {
        cambios[, incluido := no_11 == TRUE | distan_T]
        cambios_copy <- copy(cambios)
        cambios      <- cambios[incluido == TRUE]
      }

      sc_pob_conservar <- cambios_copy[
        (sc_ref %in% sc_pob_conservar | sc_new %in% sc_pob_conservar) &
          year2 > 2011
        ]
      cambios <- unique(rbindlist(list(cambios, sc_pob_conservar)))

      if (modo == "manual") {
        cambios_m <- cambios_copy[seq_len(length(sc1))]
        cambios_m[, `:=`(
          sc_ref     = sc1,
          sc_new     = sc2,
          year       = NA_integer_,
          year2      = NA_integer_,
          viviendas  = NA_integer_,
          tramo_por  = NA_real_,
          modo       = "manual",
          no_11      = NA,
          dista      = NA_real_,
          distan_T   = NA,
          viv_ref    = NA_integer_,
          cambio_ref = NA_real_,
          umbral     = NA_real_,
          umbral_T   = NA,
          incluido   = TRUE
        )]
        cambios_copy <- rbindlist(list(cambios_copy, cambios_m), fill = TRUE)
        cambios      <- rbindlist(list(cambios, cambios_m), fill = TRUE)
      }

      sc_unicas <- sort(
        unique(
          secciones[
            year %in% years_union & seccion %in% c(cambios$sc_ref, cambios$sc_new),
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
        sc_comparacion <- lapply(all_sc_islas, function(x) ifelse(is.na(x), TRUE, any(x %in% sc_islas_2011)))
      }
      if ((length(sc_islas_union) > 0 & (length(sc_islas_union) > length(sc_islas_2011))) |
          any(!sapply(sc_comparacion, any))) {
        if (all(any(sapply(all_sc_islas, length) > 0), length(sc_islas_2011) > 0)) {
          islas_alerta <- sc_islas_union[!sapply(sc_comparacion, any)]
        } else if (length(all_sc_islas) > 0) {
          islas_alerta <- sc_islas_union
        }
        if (exists("islas_alerta")) {
          cartografia$revision_manual[cartografia$seccion %in% islas_alerta] <- "Revisar manualmente"
          warning(
            "\nLas secciones de la cartograf\u00eda: c('", paste(islas_alerta, collapse = "', '"), "'),",
            " est\u00e1n conformadas por varios pol\u00edgonos que no son colindantes.\n",
            "Por favor, rev\u00edselas manualmente (consulte la ayuda de la funci\u00f3n).",
            call. = FALSE
          )
        }
      }
      attributes(cartografia@data)$cambios <- cambios_copy
    } else if (is.null(poblacion)) {
      message("\nEn el per\u00edodo establecido no se ha detectado ning\u00fan cambio: ",
              "se devuelve la misma cartograf\u00eda.")
    }
  }
  res <- cartografia


  ##############################################################################
  ##  UNION DE SECCIONES EN POBLACION Y CREACION DE CUBO                      ##
  ##############################################################################

  if (!is.null(poblacion)) {
    if (length(grep("_plus", names(poblacion))) == 2) {
      if (corte_edad == 85) {
        col_eliminar <- names(poblacion)[grep("9\\d|89|100", names(poblacion))]
        if (length(col_eliminar) == 4) {
          poblacion <- elige_corte(poblacion, corte_edad)
        } else {
          if (length(col_eliminar) > 0) {
            for (i in seq_along(col_eliminar)) {
              set(poblacion, j = col_eliminar[i], value = NULL)
            }
          }
        }
      } else {
        col_eliminar <- names(poblacion)[grep("85_plus", names(poblacion))]
        if (length(col_eliminar) == 1) {
          poblacion <- elige_corte(poblacion, corte_edad)
        }
      }
    }
    if (!is.null(cambios) && !datos_propios) {
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
        years2 <- years_estudio[years_estudio != 2011]
        for (i in seq_along(years2)) {
          if (!all(sc_11 %in% poblacion[year == years2[i], seccion])) {
            sc_not_11 <- sc_11[which(!sc_11 %in% poblacion[year == years2[i], seccion])]
            for (j in seq_along(sc_not_11)) {
              pob1 <- poblacion[year == years2[i]][1:2]
              pob1[, `:=`(
                seccion = sc_not_11[j],
                sexo    = 0:1
              )][]
              set(pob1, j = names(pob1)[!names(pob1) %in% c("seccion", "sexo", "year")], value = 1)
              poblacion <- rbindlist(list(poblacion, pob1))
            }
          }
        }
      } else {
        message(
          "\nEn el per\u00edodo establecido no se ha detectado ning\u00fan cambio: ",
          "se devuelve la misma cartograf\u00eda y poblaci\u00f3n para ese per\u00edodo, ",
          "ajustando esta \u00faltima al corte de edad marcado."
        )
      }
    }
    poblacion <- poblacion[order(seccion, sexo, year)]
    pob_array <- crea_cubo_poblacion(
      poblacion,
      periodo       = years_estudio,
      datos_propios = datos_propios
    )
    res <- list(cartografia = cartografia, poblacion = pob_array)

    if (!datos_propios) {
      if (exists("cluster_sc")) {
        attributes(res)$cluster <- cluster_sc
        attributes(res)$cambios <- cambios_copy
      }
      if (!identical(sort(cartografia$seccion), sort(unique(poblacion$seccion)))) {
        not_in_pobla <- cartografia$seccion[!cartografia$seccion %in% unique(poblacion$seccion)]
        not_in_carto <- unique(poblacion$seccion)[!unique(poblacion$seccion) %in% cartografia$seccion]
        if (length(not_in_pobla) > 0) {
          warning(
            "\nTras realizar la uni\u00f3n con las opciones marcadas, las secciones c('",
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
            "\nTras realizar la uni\u00f3n con las opciones marcadas, las secciones c('",
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

      uno_vect <- rowSums(poblacion[, -c("seccion", "sexo", "year")]) == sum(ncol(poblacion[, -c("seccion", "sexo", "year")]))

      if (any(uno_vect)) {
        warning(
          "\nEn el per\u00edodo seleccionado las secciones c('",
          paste0(unique(poblacion$seccion[uno_vect]), collapse = "', '"),
          "') no sufrieron cambios pero aparecieron m\u00e1s tarde que el ",
          "a\u00f1o de inicio elegido. Se asigna el valor 1 como poblaci\u00f3n ",
          "a dichas secciones para los a\u00f1os previos (hasta el a\u00f1o de ",
          "inicio fijado).\nPor favor, consulte la ayuda de la funci\u00f3n ",
          "para explorar este aspecto.",
          call. = FALSE
        )
        attr(res, "pob_igual_uno") <- unique(poblacion[uno_vect, seccion])
      }
    }
  }

  ##############################################################################
  ##  ASIGNACION DE SECCIONES EN MORTALIDAD Y CREACION DE CUBO                ##
  ##############################################################################

  if (!is.null(mortalidad)) {
    mortalidad_c <- mortalidad_c[
      !is.na(year_defuncion) & !is.na(sexo) & !is.na(edad) & !is.na(causa_defuncion)
    ]
    mortalidad_c <- proyecta_secciones(mortalidad_c, cartografia, epsg)

    mort_array <- crea_cubo_mortalidad(
      mortalidad_c,
      cartografia,
      epsg         = epsg,
      medea3       = medea3,
      otras_causas = otras_causas,
      corte_edad   = corte_edad,
      periodo      = years_estudio
    )

    res_attr        <- attributes(res)
    res             <- append(res, list(mortalidad = mort_array))
    attributes(res) <- append(attributes(res), res_attr["names" != names(res_attr)])
  }

  return(res)
}
