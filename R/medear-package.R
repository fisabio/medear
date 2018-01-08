
#' @title Poblaciones y cartografía por sección censal del INE (proyecto MEDEA3)
#'
#' @description Paquete que contiene las funciones y los datos utilizados para
#'   llevar a cabo el proyecto MEDEA3, incluyendo los datos de población por
#'   sexo y grupos de edad para las ciudades implicadas en el proyecto a nivel
#'   de sección censal. Aunque algunos datos de población están encriptados por
#'   tratarse de consultas específicas realizadas al Instituto Nacional de
#'   Estadística -INE- (se puede acceder a ellos mediante contraseña), los datos
#'   desde 2006 hasta 2017 son de libre acceso siguiendo la licencia del INE.
#'
#' @details El uso habitual del paquete será la carga de datos o de la
#'   cartografia (\code{\link[utils]{data}}) y su preparación para unir las
#'   secciones según el rango de años que se desee (función
#'   \code{\link{une_secciones}}). En todo caso, se ofrece la fecha de la última
#'   actualización de los datos (siempre que esté disponible) como un atributo
#'   de los objetos.
#'
#'   Un uso potencial puede ser la prepararación de consultas personalizadas
#'   para ciudades o provincias completas, por lo que el paquete ofrece todas
#'   las funciones necesarias para descargar los datos y la cartografía
#'   directamente desde el INE y trabajarlos para obtener el producto final. En
#'   este sentido, las funciones deberían ejecutarse en el siguiente orden:
#'
#'   \describe{\item{\code{\link{descarga_cartografia}}}{Descarga de la
#'   cartografía nacional por sección censal para el año 2011 en formato SHP.}
#'   \item{\code{\link{descarga_poblaciones}}}{Descarga de la poblaciones por
#'   sección censal, sexo y grupos de edad (disponibilidad desde 2006 hasta
#'   2017).} \item{\code{\link{descarga_trameros}}}{Descarga de los trameros
#'   para las provincias indicadas en los años marcados (disponible desde 2004
#'   hasta 2017).} \item{\code{\link{detecta_cambios}}}{Identifica los cambios
#'   de sección censal entre distintos años.}
#'   \item{\code{\link{une_secciones}}}{Habiendo obtenido los cambios en el
#'   seccionado, se realiza la unión de las secciones en la cartografía y en las
#'   cifras de población.} }
#'
#'   Mantenedor: Carlos Vergara-Hernández \email{vergara_car@@gva.es}
#'
#'   Fuente: Elaboración propia con datos extraídos del \href{www.ine.es}{sitio
#'   web del INE}.
#'
#'   Licencia: Los datos contenidos en este paquete han sido extraidos desde el
#'   \href{www.ine.es}{sitio web del INE} y modificados de acuerdo a las
#'   necesidades del proyecto, quedando sujeto su uso a la
#'   \href{www.ine.es/ss/Satellite?L=0&c=Page&cid=1254735849170&p=1254735849170&pagename=Ayuda\%2FINELayout#}{licencia
#'    INE}.
#'
#'   Exención de responsabilidad: El INE no ha participado en la elaboración de
#'   este paquete.
#'
#' @seealso \code{\link{descarga_cartografia}},
#'   \code{\link{descarga_poblaciones}}, \code{\link{descarga_trameros}},
#'   \code{\link{detecta_cambios}}, \code{\link{une_secciones}}
#'
#' @references \url{www.ine.es/}
#'   \url{www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990}
#'    \url{www.ine.es/censos2011_datos/cen11_datos_resultados_seccen.htm}
#'   \url{www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1254735624326&p=1254735624326&pagename=ProductosYServicios\%2FPYSLayout}
#'
#'   \url{www.ine.es/ss/Satellite?L=0&c=Page&cid=1254735849170&p=1254735849170&pagename=Ayuda\%2FINELayout#}
#'
#'
#' @name medear-package
#'
#' @aliases medear-package medear
#'
#' @docType package
#'
#' @keywords package
#'
#' @encoding latin1
#'
#' @import data.table
#'
"_PACKAGE"
