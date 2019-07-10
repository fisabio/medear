
#' @title Datos de Población por Sección Censal para las Ciudades MEDEA3
#'   (período 2004-2015)
#'
#' @description Datos de población por sexo (0=Hombres, 1=Mujeres), edad (grupos
#'   quinquenales) y año (periodo 2004-2015) a nivel de sección censal para las
#'   ciudades de MEDEA3. Estos datos han sido descargados de la web del INE, que
#'   los publica de forma libre, y se pueden obtener usando la función
#'   \code{\link{descarga_poblaciones}} de este paquete.
#'
#' @details Los códigos de sección censal (columna \code{seccion} del
#'   \code{data.frame} \code{poblacion}) se corresponden con el identificador
#'   habitual de secciones censales según el INE, es decir: los primeros dos
#'   dígitos identifican la provincia, los siguientes tres dígitos el municipio,
#'   los próximos dos dígitos el distrito y los últimos tres la sección censal.
#'   Los 5 primeros dígitos de este identificador se corresponden con el código
#'   INE del respectivo municipio.
#'
#'   Hasta el año 2010 (inclusive) el INE agrupa la última categoría de edad
#'   como 85 y más, mientras que desde el año 2011 llega hasta 100 y más. Los
#'   últimas columnas de \code{poblacion} tienen información detallada de los
#'   grupos de edad mayores para los años posteriores a 2010, por si esta
#'   pudiera ser de utilidad en algún momento. En cualquier caso, la casilla
#'   correspondiente al grupo de edad 85 y más para dichos años también tiene la
#'   información agregada para los grupos de edad mayores de 85, de la misma
#'   forma que los años anteriores.
#'
#'   El paquete \code{medear} dispone también de los datos para todo el periodo
#'   1996-2015 pero estos están encriptados ya que los datos para el periodo
#'   1996-2003 son propiedad del INE, que han sido adquiridos para uso exclusivo
#'   del proyecto MEDEA3. Estos datos son accesibles mediante la función
#'   \code{\link{carga_datos}} que necesita una contraseña de desencriptación,
#'   que se hará disponible a todos los grupos del proyecto MEDEA. La llamada a
#'   \code{\link{carga_datos}} produce un data.frame con exactamente el mismo
#'   formato que \code{poblacion}, de hecho machaca dicho objeto, pero con la
#'   información adicional del periodo 1996-2005.
#'
#'   Notar que las poblaciones corresponden al seccionado censal de cada año por
#'   lo que algunas de las secciones censales consideradas pueden no tener
#'   información para todo el periodo 2004-2015 si es que dicha sección no ha
#'   existido durante todo este periodo. Este comentario también aplica a la
#'   función \code{\link{carga_datos}}.
#'
#' @name poblacion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{poblaciones_ine} donde las filas representan
#'   la combinación de las distintas secciones censales de MEDEA3, sexos y años
#'   del periodo de estudio. Las cuatro primeras columnas son: \describe{
#'   \item{seccion}{Código de la sección censal.} \item{sexo}{0 hombres; 1
#'   mujeres.} \item{year}{Año.} \item{medea3}{Pertenencia de la sección al
#'   proyecto MEDEA3.} } El resto de columnas representan los distintos grupos
#'   de edad. Todo objeto de la clase \code{poblaciones_ine} deberá tener este
#'   formato.
#'
#' @references \href{http://www.ine.es/}{ Sitio web del INE}.
#'
#'   \href{http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990}{
#'    Poblaciones}.
#'
#' @encoding UTF-8
#'
#' @keywords datasets
#'
#' @seealso \code{\link{descarga_poblaciones}}, \code{\link{carga_datos}}.
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("poblacion")
#'
#'   # Información de poblaciones de la sección censal 01001 de Valencia (código INE 46250)
#'   poblacion[poblacion$seccion == "4625001001", ]
#'
#'   # Información de poblaciones de toda la ciudad de Valencia
#'   poblacion[substring(poblacion$seccion, 1, 5) == "46250", ]
#' }
"poblacion"


#' @title Cartografía por Sección Censal para las Ciudades MEDEA3
#'
#' @description Contiene la cartografía por sección censal tal cual puede ser
#'   utilizada por el paquete \code{sp}.
#'
#' @name cartografia
#'
#' @docType data
#'
#' @format Un objeto de clase \code{SpatialPoligonsDataFrame}, donde los datos
#'   (\code{cartografia@data}) tienen clase \code{data.frame} y
#'   \code{cartografia_ine}, donde cada fila es una sección censal y que cuenta
#'   con 7 columnas: \describe{ \item{seccion}{Cádena de 10 caracteres con el
#'   código de sección censal (incluye provincia, municipio, distrito y
#'   sección).} \item{CUMUN}{Cádena de 5 caracteres con el código INE del
#'   municipio.} \item{CCA}{Cádena de 2 caracteres con el código INE de la
#'   comunidad autónoma} \item{NPRO}{Nombre de la provincia.} \item{NCA}{Nombre
#'   de la comunidad autónoma.} \item{NMUN}{Nombre del municipio.}
#'   \item{n_viv}{Número de viviendas por sección censal (datos cruzados con
#'   Dirección General de Catastro).}}
#'
#' @references \href{http://www.ine.es/}{ Sitio web del INE}.
#'
#'   \href{http://www.ine.es/censos2011_datos/cen11_datos_resultados_seccen.htm}{Cartografía}.
#'
#'
#' @encoding UTF-8
#'
#' @keywords datasets
#'
#' @seealso \code{\link{descarga_cartografia}}.
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   library(sp)
#'   data("cartografia")
#'
#'   # Representación de los secciones censales de Álava
#'   plot(cartografia[substr(cartografia$seccion, 1, 5) == "01059", ])
#'
#'   # Representación de los secciones censales de Álava, según distritos.
#'   distritos <- substr(cartografia[substr(cartografia$CUSEC, 1, 5) == "01059", "CUSEC"], 6, 7)
#'   plot(cartografia[substr(cartografia$CUSEC, 1, 5) == "01059", ], col = as.numeric(distritos))
#' }
"cartografia"


#' @title Nombres de Municipios y Provincias según Terminología Oficial del INE
#'
#' @description Codificación/nombres usados por el INE para las provincias y los
#'   municipios. También indica si se trata de un municipio participante en
#'   MEDEA3 o no.
#'
#' @name codigos_ine
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame}, donde cada
#'   fila es un municipio y que cuenta con 7 columnas:
#'   \describe{
#'     \item{cod_provincia}{Cádena de 2 caracteres con el código de la provincia.}
#'     \item{CUMUN}{Cádena de 3 caracteres con el código del municipio.}
#'     \item{nombre_municipio}{Nombre del municipio.}
#'     \item{medea3}{Valor lógico: ¿participa en MEDEA3?}
#'   }
#'
#' @references
#'   \href{http://www.ine.es/}{ Sitio web del INE}.
#'
#'   \href{http://www.ine.es/daco/daco42/codmun/codmunmapa.htm}{ Codificación INE}.
#'
#' @keywords datasets
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("codigos_ine")
#' }
"codigos_ine"


#' @title Cambios Temporales de Seccionado para todas las Ciudades MEDEA3
#'   (período 1996-2015)
#'
#' @description Relación de secciones censales que interseccionan
#'   geográficamente con el seccionado de 2011, según la definición de dicha
#'   sección sección censal en ambos años siguiendo el tramero del INE. Este
#'   objeto contiene todas las intersecciones entre secciones censales distintas
#'   para el período 1996-2015 (tanto haciendo uso del código postal como sin
#'   el: campo \code{codigo_postal}). Esta información es costosa de calcular y
#'   se necesita para la función \code{\link{une_secciones}} por ello se ha
#'   considerado conveniente almacenar sus resultados dentro del paquete.
#'
#'   Salvo Vitoria-Gasteiz, Barcelona, Donostia, Madrid, Pamplona y Bilbao, el
#'   resto de ciudades incorporan información sobre el número de viviendas
#'   afectadas por el cambio de sección así como el porcentaje de tramos, no
#'   detectados en catastro, de ese cambio con respecto al total de tramos que
#'   contiene la sección de 2011 (información útil a la hora de unir las
#'   secciones).
#'
#' @name cambios_seccion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{cambios_ine}, donde cada fila es un un
#'   cambio de sección y que cuenta con 7 columnas: \describe{
#'   \item{sc_ref}{Cádena de 10 caracteres con el código de la sección 2011.}
#'   \item{sc_new}{Cádena de 10 caracteres con el código de la sección en año ==
#'   year2.} \item{year}{Año de referencia: 2011.} \item{year2}{Año de
#'   comparacion.} \item{viviendas}{Número de viviendas afectadas por el cambio
#'   de sección.} \item{tramo_por}{Porcentaje de tramos, no detectados en
#'   catastro, afectados por el cambio de sección, respecto al total de tramos
#'   que incluye la sección de 2011.} \item{codigo_postal}{¿La detección de los
#'   cambios se hizo usando el código postal?.}  \item{vias}{Lista con las vías
#'   afectadas por el cambio de sección.} }
#'
#' @keywords datasets
#'
#' @seealso \code{\link{detecta_cambios}} y \code{\link{descarga_trameros}}.
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(cambios_seccion)
#' cambios_sin_cp <- cambios_seccion[cambios_seccion$cp == FALSE, ]
#' cambios_con_cp <- cambios_seccion[cambios_seccion$cp == TRUE, ]
#' }
"cambios_seccion"


#' @title Secciones Censales Únicas por Período (1996-2015)
#'
#' @description Listado con las secciones censales únicas por año (1996-2015) de
#'   ciudades MEDEA3.
#'
#' @name secciones
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame} y \code{data.table}, donde cada
#'   fila es una sección para un año determinado.
#'
#' @keywords datasets
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("secciones")
#' }
"secciones"


#' @title Ponderación de las Vecindades de las Ciudades MEDEA3 (1996-2015)
#'
#' @description Datos con la ponderación de la vecindad de cada una de las secciones
#'    censales que componen las ciudades MEDEA3 (1996-2015).
#'
#' @name vecindad_ponderada
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame}, donde cada
#'   fila es una sección censal, ofreciendo un valor \code{c} para hombres y mujeres:
#'   \describe{
#'     \item{seccion}{Cádena de 10 caracteres con el código INE de la sección censal.}
#'     \item{muni}{Cádena de 5 caracteres con el código INE del municipio.}
#'     \item{vecindad_hombres}{Valor de la vecindad en hombres.}
#'     \item{vecindad_mujeres}{Valor de la vecindad en mujeres.}
#'   }
#'
#' @keywords datasets
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("vecindad_ponderada")
#' }
"vecindad_ponderada"


#' @title Censos de 2001 y 2011
#'
#' @description Datos de los censos de 2001 y 2011 necesarios para construir el
#'   índice de privación. Estos datos se cargan con la función
#'   \code{\link{carga_datos}}.
#'
#' @name censo
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame} con los siguientes campos:
#'
#'   \itemize{
#'     \item seccion Código de sección censal (10 caracteres).
#'     \item muni Código INE del municipio.
#'     \item year Año del censo.
#'     \item i01 Numerador: Trabajadores manuales entre los ocupados, de 16 o más años (dividir por i01_d).
#'     \item i02 Numerador: Parados de 16 o más años entre los activos (dividir por i02_d).
#'     \item i03 Numerador: Asalariados eventuales entre los ocupados de 16 o más años (dividir por i01_d).
#'     \item i04 Numerador: Personas con instrucción insuficiente de 16 o más años (dividir por i03_d).
#'     \item i05 Numerador: Personas con instrucción insuficiente de 16 a 29 años (dividir por i04_d).
#'     \item i06 Numerador: Población de 65 o más años (dividir por i05_d).
#'     \item i07 Numerador: Nacidos en países de renta baja (dividir por i05_d).
#'     \item i08 Numerador: Nacidos en países de renta baja llegados a España
#'       en los cinco años previos al año de censo (dividir por i05_d).
#'     \item i01_d Denominador: Población ocupada de 16 o más años.
#'     \item i02_d Denominador: Población de 16 o más años en situación laboral = 1
#'       Ocupado/a + 2 Parado que ha trabajado antes + 3 Parado que busca
#'       empleo (población activa).
#'     \item i03_d Denominador: Población de 16 o más años.
#'     \item i04_d Denominador: Población de 16 a 29  años.
#'     \item i05_d Denominador: Población total.
#'   }
#'
#' @keywords datasets
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   censo <- carga_datos("contraseña", tipo = "censo")
#' }
#'
#' @seealso \code{\link{carga_datos}}
NULL


#' @title Índices de Privación por Sección Censal para Ciudades MEDEA3
#'
#' @description Índices de privación por sección censal según los censos INE 2001 y 2011,
#'   calculados siguiendo metodología MEDEA3 (publicación metodológica pendiente) y
#'    ajustados al seccionado MEDEA3 (tras realizar uniones de secciones 1996-2015).
#'
#' @details El índice es comparable entre ciudades y períodos.
#'
#' @name privacion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame}, donde cada
#'   fila es una sección censal, ofreciendo un valor del índice de privación para 2001 y 2011:
#'   \describe{
#'     \item{seccion}{Cádena de 10 caracteres con el código INE de la sección censal.}
#'     \item{cod_municipio}{Cádena de 5 caracteres con el código INE del municipio.}
#'     \item{nom_municipio}{Cádena caracteres con el nombre del municipio.}
#'     \item{privacion_2001}{Valor del índice de privación MEDEA3 en 2001.}
#'     \item{privacion_2011}{Valor del índice de privación MEDEA3 en 2011.}
#'   }
#'
#' @encoding UTF-8
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("privacion")
#' }
"privacion"


#' @title Límites (\code{bbox}) para los Mapas en MEDEA3
#'
#' @description Lista con los \code{\link[sp]{bbox}} para las ciudades MEDEA3.
#'
#' @name bboxm3
#'
#' @docType data
#'
#' @format Una lista donde el primer elemento es el código de municipio INE, el
#'   segundo es una lista con los bbox por ciudad y el tercero un vector
#'   definiendo la posición de la leyenda.
#'
#' @encoding UTF-8
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#'   library(medear)
#'   data("bboxm3")
#' }
"bboxm3"
