
#' @title Datos de población por sección censal para las ciudades MEDEA3
#'
#' @description Datos de población por sexo y edad (grupos quinquenales) para
#'   las ciudades implicadas en el proyecto a nivel de sección censal. Los datos
#'   para algunos años están encriptados por tratarse de consultas específicas
#'   realizadas al INE y solo son accesibles mediante contraseña. Los datos
#'   desde 2006 hasta 2017 son de libre acceso siguiendo la licencia del INE, y
#'   se pueden obtener usando las funciones contenidas en este paquete.
#'
#' @details Los códigos de sección censal siguen un orden preestablecido: los
#'   primeros dos dígitos identifican la provincia, los siguientes tres dígitos
#'   el municipio, los próximos dos dígitos el distrito y los últimos cuatro a
#'   la sección censal.
#'
#'   Hasta el año 2011 el INE agrupa la última categoría de edad como 85 y más,
#'   mientras que desde el año siguiente llega hasta 100 y más.
#'
#'   Se ha codificado el sexo como 0 (masculino) y 1 (femenino).
#'
#' @name poblacion
#'
#' @docType data
#'
#' @format Un objeto de clase \code{poblaciones_ine} donde las filas representan
#'   las distintas secciones censales. Las cuatro primeras columnas son:
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
#' @examples
#' data(poblacion)
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
#' data(cartografia)
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
#' data(codigos_ine)
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
#' data(cambios_pais)
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
#' data(cambios_seccion)
"cambios_seccion"
