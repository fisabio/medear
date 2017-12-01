
#' @title Datos de población por sección censal para las ciudades MEDEA3 (periodo 2006-2016)
#'
#' @description Datos de población por sexo (0=Hombres, 1=Mujeres), edad (grupos quinquenales) y año (periodo 2006-2016) a nivel de sección censal para las ciudades de MEDEA3. Estos datos han sido descargados de la web del INE, que los publica de forma libre, y se pueden obtener usando la función \code{descarga_cartografia} de este paquete.
#'
#' @details Los códigos de sección censal (columna \code{seccion} del data.frame \code{poblacion}) se corresponden con el identificador habitual de secciones censales según el INE, es decir: los primeros dos dígitos identifican la provincia, los siguientes tres dígitos el municipio, los próximos dos dígitos el distrito y los últimos tres la sección censal. Los 5 primeros dígitos de este identificador se corresponden con el código INE del respectivo municipio.
#'
#' Hasta el año 2010 (inclusive) el INE agrupa la última categoría de edad como 85 y más, mientras que desde el año 2011 llega hasta 100 y más. Los últimas columnas de \code{poblacion} tienen información detallada de los grupos de edad mayores para los años posteriores a 2010, por si ésta pudiera ser de utilidad en algún momento. En cualquier caso, la casilla correspondiente al grupo de edad 85 y más para dichos años también tiene la información agregada para los grupos de edad mayores de 85, de la misma forma que los años anteriores.
#'
#' El paquete \code{medear} dispone también de los datos para todo el periodo 1996-2016 pero éstos están encriptados ya que los datos para el periodo 1996-2005 son propiedad del INE, que han sido adquiridos para uso exclusivo del proyecto MEDEA3. Estos datos son accesibles mediante la función \code{\link{carga_datos}} que necesita una contraseña de desencriptación, que se hará disponible a todos los grupos del proyecto MEDEA. La llamada a \code{carga_datos} produce un data.frame con exactamente el mismo formato que \code{poblacion}, de hecho machaca dicho objeto, pero con la información adicional del periodo 1996-2005.
#'
#' Notar que las poblaciones corresponden al seccionado censal de cada año por lo que algunas de las secciones censales consideradas pueden no tener información para todo el periodo 2006-2016 si es que dicha sección no ha existido durante todo este periodo. Este comentario también aplica a la función \code{\link{carga_datos}}.
#'
#' @name poblacion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{poblaciones_ine} donde las filas representan
#'   la combinación de las distintas secciones censales de MEDEA3, sexos y años del periodo de estudio. Las cuatro primeras columnas son:
#'   \describe{
#'     \item{seccion}{Código de la sección censal.}
#'     \item{sexo}{0 equivale a varones, 1 equivale a mujeres.}
#'     \item{year}{Año.}
#'     \item{medea3}{Pertenencia de la sección al proyecto MEDEA3.}
#'   }
#'   El resto de columnas representan los distintos grupos de edad.
#'
#' @references
#'   \url{http://www.ine.es/}{Sitio web del INE}.
#'   \url{http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990}{Población}.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{descarga_poblaciones}}, \code{\link{carga_datos}}
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(poblacion)
#' #Información de poblaciones de la sección censal 01001 de Valencia (código INE 46125)
#' poblacion[poblacion$seccion=="4612501001",]
#' #Información de poblaciones de toda la ciudad de Valencia
#' poblacion[substring(poblacion$seccion,1,5)=="46125",]
#' }

"poblacion"


#' @title Cartografía por sección censal para las ciudades MEDEA3
#'
#' @description Contiene la cartografía por sección censal en formato Simple
#'   Features tal cual puede ser utilizada por el paquete \code{sf}.
#'
#' @name cartografia
#'
#' @docType data
#'
#' @format Un objeto de clase \code{cartografia_ine} y \code{sf}, donde cada
#'   fila es una sección censal y que cuenta con 13 columnas:
#'   \describe{
#'     \item{CUSEC}{Cádena de 10 caracteres con el código de sección censal
#'     (incluye provincia, municipio y distrito).}
#'     \item{CUMUN}{Cádena de 5 caracteres con el código del municipio (incluye
#'     provincia).}
#'     \item{CSEC}{Cádena de 3 caracteres con el código de sección censal.}
#'     \item{CDIS}{Cádena de 2 caracteres con el código de distrito.}
#'     \item{CPRO}{Cádena de 3 caracteres con el código de provincia.}
#'     \item{CCA}{Cádena de 2 caracteres con el código de comunidad autónoma.}
#'     \item{CUDIS}{Cádena de 7 caracteres con el código de distrito (incluye
#'     provincia y  municipio).}
#'     \item{OBS}{Observaciones por parte del proveedor de los datos.}
#'     \item{NPRO}{Nombre de la provincia.}
#'     \item{NCA}{Nombre de la comunidad autónoma.}
#'     \item{NMUN}{Nombre del municipio.}
#'     \item{geometry}{Columna de tipo lista con la geometría asociada a cada
#'     sección censal.}
#'   }
#'
#' @references
#'   \url{http://www.ine.es/}{Sitio web del INE}.
#'   \url{http://www.ine.es/censos2011_datos/cen11_datos_resultados_seccen.htm}{Cartografía}.
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(cartografia)
#' #Representación de los distritos censales de Álava
#' plot(cartografia[cartografia$CUMUN=="01059","CDIS"])
#' }
"cartografia"


#' @title Código de provincias y municipios del INE
#'
#' @description Codificación usada por el INE para las provincias y los
#'   municipios. También indica si se trata de un municipio participante en
#'   MEDEA3 o no.
#'
#' @name codigos_ine
#'
#' @docType data
#'
#' @format Un objeto de clase \code{data.frame}, donde cada
#'   fila es un municipio y que cuenta con 4 columnas:
#'   \describe{
#'     \item{cod_provincia}{Cádena de 2 caracteres con el código de la provincia.}
#'     \item{CUMUN}{Cádena de 3 caracteres con el código del municipio.}
#'     \item{nombre_municipio}{Nombre del municipio.}
#'     \item{medea3}{Vector lógico: ¿participa en MEDEA3?}
#'   }
#'
#' @references
#'   \url{http://www.ine.es/}{Sitio web del INE}.
#'   \url{http://www.ine.es/daco/daco42/codmun/codmunmapa.htm}{Codificación INE}.
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(codigos_ine)
#' }
"codigos_ine"


#' @title Cambios de seccionado para todo el país.
#'
#' @description Cambios de seccionado para todo el país.
#'
#' @name cambios_pais
#'
#' @docType data
#'
#' @format Un objeto de clase \code{cambios_ine}, donde cada
#'   fila es un un cambio de sección y que cuenta con 4 columnas:
#'   \describe{
#'     \item{sc_old}{Cádena de 10 caracteres con el código de la sección en año
#'     == year.}
#'     \item{sc_new}{Cádena de 10 caracteres con el código de la sección en año
#'     == year2.}
#'     \item{year}{Primer año.}
#'     \item{year2}{Segundo año.}
#'   }
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(cambios_pais)
#' }
"cambios_pais"


#' @title Cambios de seccionado para las ciudades MEDEA3.
#'
#' @description Cambios de seccionado para las ciudades MEDEA3.
#'
#' @name cambios_seccion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{cambios_ine}, donde cada
#'   fila es un un cambio de sección y que cuenta con 4 columnas:
#'   \describe{
#'     \item{sc_old}{Cádena de 10 caracteres con el código de la sección en año
#'     == year.}
#'     \item{sc_new}{Cádena de 10 caracteres con el código de la sección en año
#'     == year2.}
#'     \item{year}{Primer año.}
#'     \item{year2}{Segundo año.}
#'   }
#'
#' @keywords datasets
#'
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(cambios_seccion)
#' }
"cambios_seccion"

#' @title Secciones censales únicas por año (1996-2016).
#'
#' @description Listado con las secciones censales únicas por año (1996-2016).
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
#' @examples
#'
#' \dontrun{
#' library(medear)
#' data(secciones)
#' }
"secciones"
