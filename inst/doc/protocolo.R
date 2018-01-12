## ---- message=FALSE, warning=FALSE, eval = FALSE-------------------------
#  if (!"devtools" %in% installed.packages())
#    install.packages("devtools")
#  devtools::install_github("fisabio/medear") # Puede tardar unos minutos...
#  library(medear)

## ---- eval=FALSE---------------------------------------------------------
#  # Filtramos la cartografía, en nuestro caso nos quedamos sólo con las ciudades de la
#  # Comunitat Valenciana (adaptar en caso de otras CCAA)
#  carto.munis <- cartografia[cartografia$CCA == "10", ]

## ---- eval=FALSE---------------------------------------------------------
#  # No ejecutar este comando a menos que se quiera importar un archivo de cartografía
#  # El paquete rgdal se instala como dependencia del paquete medear
#  library(rgdal)
#  # Cambiar CartografiaDeseada.shp y XXXXXXXX por los argumentos oportunos
#  carto.munis <- readOGR(dsn = "CartografiaDeseada.shp", layer = "CartografiaDeseada")

## ---- eval=FALSE---------------------------------------------------------
#  colnames(datosmort)
#  # [1] "NID"        "SEXO"       "ANODEFUN"   "MESDEFUN"   "DIADEFUN"   "ANONAC"
#  # [7] "MESNAC"     "DIANAC"     "TVIA"       "NVIA"       "NPOLI"      "CODMUNIRES"
#  # [13]"NMUNIRES"   "NPROVRES"   "CODPST"      "CAUSABASIC"

## ---- eval=FALSE---------------------------------------------------------
#  datosmort$BOD.direccion <- ""    # Dirección tal cual ha sido intentada geocodificar
#  datosmort$georef        <- "NO"  # Status del proceso de georeferenciación
#  datosmort$id            <- ""
#  datosmort$province      <- ""
#  datosmort$muni          <- ""
#  datosmort$tip_via       <- ""
#  datosmort$address       <- ""
#  datosmort$portalNumber  <- ""
#  datosmort$refCatastral  <- ""
#  datosmort$postalCode    <- ""
#  datosmort$lat           <- NA_real_
#  datosmort$lng           <- NA_real_
#  datosmort$stateMsg      <- ""
#  datosmort$state         <- ""
#  datosmort$type          <- ""

## ---- eval = FALSE-------------------------------------------------------
#  # Seleccionamos individuos a georeferenciar, si se quisiera hacer una segunda
#  # ronda de geocodificación (como luego haremos con Google) una sentencia de selección
#  # de este tipo hará que sólo se aplique la nueva geocodificación a los registros
#  # que nos parezca oportuno.
#  
#  no.geo    <- which(datosmort$georef == "NO")
#  totno.geo <- length(no.geo)
#  
#  # Comenzamos bucle de geocodificación para los registros seleccionados
#  for (i in 1:totno.geo) {
#  
#    cont <- no.geo[i]
#  
#    # Preparamos la dirección (normalización y limpieza)
#    aux.direc <- limpia_dir(
#      tvia    = datosmort$TVIA[cont],
#      nvia    = datosmort$NVIA[cont],
#      npoli   = datosmort$NPOLI[cont],
#      muni    = datosmort$NMUNIRES[cont],
#      prov    = datosmort$NPROVRES[cont],
#      codpost = datosmort$CODPST[cont]
#    )
#  
#    if (aux.direc$nvia == "") {
#      datosmort$georef[cont] <- "DIREC VACIA"
#    } else {
#  
#      # Guardamos en "BOD.direccion" la dirección normalizada que vamos
#      # a mandar a Cartociudad.
#      datosmort$BOD.direccion[cont] <- paste0(
#        aux.direc$tvia, " ",
#        aux.direc$nvia, " ",
#        aux.direc$npoli, ", ",
#        aux.direc$muni, " , ",
#        aux.direc$prov, " , ",
#        aux.direc$codpost
#      )
#  
#      direc <- datosmort$BOD.direccion[cont]
#  
#      # Georeferenciación con caRtociudad con comprobación de que la
#      # geocodificación que hemos obtenido recae geográficamente dentro del
#      # límite geográfico correspondiente a la ciudad.
#      aux <- geocodificar_cartociudad(
#        direc    = direc,
#        poligono = carto.munis[carto.munis$CUMUN == datosmort$CODMUNIRES[cont], ]
#      )
#  
#      # En caso de que quisiéramos georeferenciar con caRtociudad sin más,
#      # sin comprobar que el punto que obtenemos está incluido en una región
#      # geográfica concreta podríamos hacer simplemente:
#      # aux <- geocodificar_cartociudad(direc = direc)
#  
#      columnas_elegidas <- c(
#        "id", "province", "muni", "tip_via", "address", "portalNumber", "refCatastral",
#        "postalCode", "lat", "lng", "stateMsg", "state", "type", "georef"
#      )
#  
#      if (substr(aux$georef, 1, 2) != "NO") {
#        datosmort[cont, columnas_elegidas] <- aux
#      } else {
#        datosmort$georef[cont] <- as.character(aux$georef)
#        # El resultado de la geocodificación puede ser NO.XXX además de un simple NO
#        # (donde XXX nos puede aportar información adicional), ese es el motivo por
#        # el que actualizamos el valor de la columna georef del registro correspondiente.
#  
#        # En caso de que la geocodificación de la dirección no haya tenido éxito,
#        #  probamos la geocodificación de algunas variantes de dicha dirección.
#        for (filtro in 1:2) {
#          if (substr(aux$georef, 1, 2) == "NO") {
#            # Si alguno de los filtros ha funcionado no se reintentaría la geocodificación.
#            aux.direcf <- filtra_dir(vias = aux.direc, filtro)
#            if (aux.direcf != "") {
#                direcf <- aux.direcf
#                aux    <- geocodificar_cartociudad(
#                  direc    = direcf,
#                  poligono = carto.munis[carto.munis$CUMUN == datosmort$CODMUNIRES[cont], ]
#                )
#            }
#            if(substr(aux$georef, 1, 2) != "NO") {
#              datosmort[cont, columnas_elegidas] <- aux
#              datosmort$georef[cont] <- paste0(datosmort$georef[cont], filtro)
#            }
#          }
#        }
#      }
#    }
#    #Contador
#    cat(paste(i, "de", totno.geo, "georef", datosmort$georef[cont], "\n"))
#  }
#  
#  # Una vez finalizado el proceso guardamos una copia de los datos georeferenciados por
#  # caRtociudad antes de pasar a google
#  save(datosmort, file = "datos/datosfinalescarto/datoscarto.RData")

## ---- eval=FALSE---------------------------------------------------------
#  load(file = "datos/datosfinalescarto/datoscarto.RData")
#  columnas_elegidas <- c(
#        "id", "province", "muni", "tip_via", "address", "portalNumber", "refCatastral",
#        "postalCode", "lat", "lng", "stateMsg", "state", "type", "georef"
#      )
#  # Seleccionamos aquellos individuos a georeferenciar que no lo hayan sido antes
#  # o hayan sido georeferenciados por Google de forma defectuosa.
#  no.geo <- which(substr(datosmort$georef, 1, 2) == "NO" &
#                    datosmort$state != "ZERO_RESULTS" &
#                    datosmort$georef != "NO punto google")
#  totno.geo <- length(no.geo)
#  
#  for (i in 1:totno.geo) {
#    cont  <- no.geo[i]
#  
#    direc <- limpiadirecGoogle(datosmort$BOD.direccion[cont])
#  
#    # Georeferencia con Google con comprobación de que es asignado al interior del polígono
#    # correspondiente a la ciudad.
#    aux <- geocodificar_google(
#      direc    = direc,
#      poligono = carto.munis[carto.munis$CUMUN == datosmort$CODMUNIRES[cont], ]
#    )
#  
#    # Para georeferenciar con Google sin más, si no se quisiera cruzar
#    # con los límites geográficos de la ciudad haríamos:
#    # aux <- geocodificar_google(direc)
#  
#    if (aux$georef == "NO punto") {
#      datosmort$georef[cont] <- "NO punto google"
#    }
#    if (aux$georef == "NO") {#Cuando NO se ha podido georeferenciar con google
#      #recogemos el motivo: "ZERO_RESULTS", "OVER_QUERY_LIMIT",...
#      datosmort$state[cont] <- as.character(aux$state)
#    }
#    if(!aux$georef %in% c("NO", "NO punto")) {
#      datosmort[cont, columnas_elegidas] <- aux
#    }
#    cat(paste(i, "de", totno.geo, "georef", datosmort$georef[cont], "\n"))
#  }
#  
#  save(datosmort, file = "datos/datosfinalescarto/datoscarto.RData")

