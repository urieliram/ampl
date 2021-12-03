
!**************************************************
!Definicion de variables globales de red eléctrica
!**************************************************
MODULE ParGloRed

Implicit none

!include 'C:\AUCHT\Parametros\param.par'  !Windows (comentarizar para Linux)
include 'param.par'  !Linux (comentarizar para Windows)

character*250 path_result

! Ruta de archivos de salida
common / path_result / path_result

! Bandera para definir si se usa factorización ( 1: si, 0:no )
logical banfactorizacion
common / banfactorizacion / banfactorizacion

! Bandera para definir si se usa factorización en perdidas ( 1: si, 0:no )
logical banfactorizacionp
common / banfactorizacionp / banfactorizacionp

!::::::::::::::::::::::::::::::::::::::
!        Datos asociados a AUHE
!::::::::::::::::::::::::::::::::::::::
!

character*10 letfecha
common / letfecha / letfecha

! Duales asociadas al límite superior de grupos de ramas restringidas
real dualgrurams 
common / dualgrurams / dualgrurams ( maxgruram, maxint ) 

! Duales asociadas al límite inferior de grupos de ramas restringidas
real dualgrurami 
common / dualgrurami / dualgrurami ( maxgruram, maxint ) 


!::::::::::::::::::::::::::::::::::::::
!        Datos generales
!::::::::::::::::::::::::::::::::::::::

! Potencia base
real basmva
common / basmva / basmva

! Precio tope para los costos marginales nodales
real preciotope
common / preciotope / preciotope

! Precio piso para los costos marginales nodales
real preciopiso
common / preciopiso / preciopiso


!::::::::::::::::::::::::::::::::::::::
!        Datos asociados a nodos
!::::::::::::::::::::::::::::::::::::::


! Nombre del nodo
character*20 nomnod
common / nomnod / nomnod (maxnod)

! Indice del nodo eléctrico
integer indnod
common / indnod / indnod (maxnod)

! Tipo del nodo eléctrico
! 4:Referencia angular, 3:Generación y carga, 2: Carga, 1:Generación, 
! 0:No hay generación ni carga
integer tipnod
common / tipnod / tipnod (maxnod)

! id del nodo eléctrico
integer idnodo
common / idnodo / idnodo (maxnod)

! Indice del area del nodo
integer arenod
common / arenod / arenod (maxnod)

! Indice de la region de precios del nodo
integer regnod
common / regnod / regnod (maxnod)

! Subsistema del nodo
integer sisnod
common / sisnod / sisnod(maxnod)

! Disponibilidad del nodo en el intervalo inicial
integer disnodini
common / disnodini / disnodini(maxnod)

! Nodos que cambian de estado de disponibilidad en el intervalo
integer noddisint
common / noddisint / noddisint(10*maxnod)

! Estado de disponibilidad de nodos que cambian de estado en el intervalo
integer estnoddisint
common / estnoddisint / estnoddisint(10*maxnod)

! Apuntador a lista de nodos no disponibles por intervalo
integer apunoddisint
common / apunoddisint / apunoddisint ( maxint+1 )

! Apuntador a lista de nodos no disponibles por intervalo
integer lisnoddisint
common / lisnoddisint / lisnoddisint ( 10*maxram )

! Número reducido de nodos
integer nmnod
common / nmnod / nmnod

integer numnodsis
common / numnodsis / numnodsis ( maxsis )


! Indice reducido del nodo
integer inrnod
common / inrnod / inrnod(maxnod)

! Indice aumentado del nodo
integer inanod
common / inanod / inanod(maxnod)

! Indice aumentado del nodo por subsistema
integer inaumnodsis
common / inaumnodsis / inaumnodsis(maxnod)

! Indice reducido del nodo por subsistema
integer inrednodsis
common / inrednodsis / inrednodsis(maxnod)

! Inicia conectividad del nodo
integer incnod
common / incnod / incnod(maxnod)

! Isla del nodo
integer islnod
common / islnod / islnod(maxnod)

! Disponiblidad del nodo
integer disnod
common / disnod / disnod(maxnod)

! Apuntador a nodos por isla
integer apnois
common / apnois / apnois(maxisl+1)

! Lista de nodos por isla
integer linois
common / linois / linois(maxnod)

! potencia activa del nodo para el intervalo a evaluar
real*8 pcanod
common / pcanod / pcanod(maxnod)

! Potencia activa generada en el nodo
real*8 pgenod
common / pgenod / pgenod (maxnod)

! Pérdidas de potencia activa en los extremos de los nodos
real*8 perpacnod
common / perpacnod / perpacnod ( maxnod, maxint )

! Costo marginal nodal
real*8 MargNodal
common / MargNodal / MargNodal ( maxnod, maxint )

! Costo marginal componente de energía
real*8 MargEnergia
common / MargEnergia / MargEnergia ( maxnod, maxint )

! Termino del costo marginal nodal asociado a perdidas
real*8 MargPerdidas
common / MargPerdidas / MargPerdidas ( maxnod, maxint )

! Termino del costo marginal nodal asociado a restricciones de transmisión de potencia activa
real*8 MargTransmision
common / MargTransmision / MargTransmision ( maxnod, maxint )

! Costo marginal nodal modificado
real*8 MargNodalMod
common / MargNodalMod / MargNodalMod ( maxnod, maxint )

! Costo marginal componente de energía  modificado
real*8 MargEnergiaMod
common / MargEnergiaMod / MargEnergiaMod ( maxnod, maxint )

! Termino del costo marginal nodal asociado a perdidas  modificado
real*8 MargPerdidasMod
common / MargPerdidasMod / MargPerdidasMod ( maxnod, maxint )

! Termino del costo marginal nodal asociado a restricciones de transmisión de potencia activa modificado
real*8 MargTransmisionMod
common / MargTransmisionMod / MargTransmisionMod ( maxnod, maxint )

! angulos de voltaje de nodos por intervalo
real*8 angvolnodint 
common /  angvolnodint  / angvolnodint ( maxnod, maxint )

!::::::::::::::::::::::::::::::::::::::
!        Datos asociados a ramas
!::::::::::::::::::::::::::::::::::::::
! Nombre de la rama
character*30 nomram
common / nomram / nomram (maxram)

! Número de ramas
integer numram
common / numram / numram

! Indice de la rama
integer indram
common / indram / indram (maxram)

! Disponibilidad de la rama para el primer intervalo 
integer disramini
common / disramini / disramini (maxram)

! Estado de disponibilidad de ramas que cambian de estado en el intervalo
integer ramdisint
common / ramdisint / ramdisint(10*maxram)

! Estado de disponibilidad de ramas que cambian de estado en el intervalo
integer estramdisint
common / estramdisint / estramdisint(10*maxram)

! Apuntador a lista de ramas no disponibles por intervalo
integer apuramdisint
common / apuramdisint / apuramdisint ( maxint+1 )

! Apuntador a lista de rams no disponibles por intervalo
integer lisramdisint
common / lisramdisint / lisramdisint ( 10*maxram )

! Disponibilidad de la rama para el intervalo a procesar
integer disram
common / disram / disram (maxram)


! Nodo origen de la rama
integer oriram
common / oriram / oriram (maxram)

! Nodo destino de la rama
integer desram
common / desram / desram (maxram)

! tipo de rama
integer tipram
common / tipram / tipram (maxram)

! Resistencia de la rama
real resram
common / resram / resram (maxram)

! Reactancia de la rama
real rearam
common / rearam / rearam (maxram)

! Suceptancia de la rama
real suceptram
common / suceptram / suceptram (maxram)

! Conductancia de la rama
real conductram
common / conductram / conductram (maxram)

! Flujo de potencia activa de la rama por intervalo
real*8 fluramint
common / fluramint / fluramint (maxram, maxint )

! Perdidas de potencia activa de la rama por intervalo
real*8 perdramint
common / perdramint / perdramint (maxram, maxint )

!:::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a grupos de ramas
!:::::::::::::::::::::::::::::::::::::::::::::::::::

! Nombre del grupo de ramas
character*25 nomgruram
common / nomgruram / nomgruram (maxgruram)

! Numero de grupos de ramas
integer numgruram
common / numgruram / numgruram

! Numero de grupos de ramas por sistema
integer numgruramsis
common / numgruramsis / numgruramsis ( maxsis )

! Indice del grupo de rama
integer indgruram
common / indgruram / indgruram ( maxgruram )

! Bandera que indica si la restriccion será tomada en cuenta en el calculo
integer bangruram, bangruramcopy, RamActiva
common / bangruram / bangruram ( maxgruram )
common / bangruramcopy / bangruramcopy ( maxgruram )
common / RamActiva / RamActiva ( maxgruram )


! Apunta a inicio de información de grupos de ramas
integer ApuEleGruRam
common / ApuEleGruRam / ApuEleGruRam (maxgruram+1)

! lista de elementos de grupo de ramas
integer LisEleGruRam
common / LisEleGruRam / LisEleGruRam (maxelegruram)

! Sentido de aportación del flujo de la rama dentro del grupo
integer SentEleGruRam
common / SentEleGruRam / SentEleGruRam (maxelegruram)

! Potencia mínima del grupo de ramas
real*8 potmingruram
common / potmingruram / potmingruram (maxgruram, maxint)

! Potencia máxima del grupo de ramas
real*8 potmaxgruram
common / potmaxgruram / potmaxgruram (maxgruram, maxint)

! Sensibilidad de grupos de ramas ante inyecciones nodales
real*8 SnsGruRarInyNod
common / SnsGruRarInyNod / SnsGruRarInyNod (maxgruram, maxnod )


! Coeficiente de del lado derecho de la restricción de grupo de ramas
real*8 CoeSnsGruRar
common / CoeSnsGruRar / CoeSnsGruRar ( maxgruram, maxint )

! Sensibilidad de los elementos que forman parte de grupos de ramas ante inyecciones en unidades de rango continuo
real*8 SnsEleGruRamInyNod
common / SnsEleGruRamInyNod / SnsEleGruRamInyNod (maxelegruram, maxnod)

! lado derecho de los elementos que forman parte de grupos de ramas
real*8 CoeSnsRar
common / CoeSnsRar / CoeSnsRar (maxelegruram, maxint)

! Numero reducido de grupos de ramas restringidas
integer nmgruram
common / nmgruram / nmgruram

! apunta a indice aumentado de grupos de ramas restringidas
integer inagruram
common / inagruram / inagruram ( maxgruram )


!:::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a islas eléctricas
!:::::::::::::::::::::::::::::::::::::::::::::::::::
integer numisl
common / numisl / numisl

!:::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a regiones de precios
!:::::::::::::::::::::::::::::::::::::::::::::::::::

! Numero de regiones de precios
integer numregpre
common / numregpre / numregpre 

! Indice de la region de precios
integer indregpre
common / indregpre / indregpre ( maxregpre )

! Nombre largo de la region de precios
character*20 nomregpre
common / nomregpre / nomregpre ( maxregpre )

! Nombre corto de la region de precios
character*3 clvregpre
common / clvregpre / clvregpre ( maxregpre )

! Indice de la region de precios
integer sisregpre
common / sisregpre / sisregpre ( maxregpre )

! Costo marginal regional promedio
real*8 MargRegional
common / MargRegional / MargRegional ( maxregpre, maxint )

! Costo marginal regional energía
real*8 MargRegionalEnergia
common / MargRegionalEnergia / MargRegionalEnergia ( maxregpre, maxint )

! Costo marginal regional energía
real*8 MargRegionalPerdidas
common / MargRegionalPerdidas / MargRegionalPerdidas ( maxregpre, maxint )

! Costo marginal regional transmisión
real*8 MargRegionalTransmision
common / MargRegionalTransmision / MargRegionalTransmision ( maxregpre, maxint )


!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados al jacobiano de flujos de carga
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
! Apunta a siguiente elemento conectado al nodo
integer siguem
common / siguem / siguem ( maxeleybus )

! Indice de nodo adyacente m
integer inbusm
common / inbusm / inbusm ( maxeleybus )


!:::::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a sensibilidades de pérdidas
!:::::::::::::::::::::::::::::::::::::::::::::::::::::

! Sensibilidad de pérdidas ante inyecciones nodales
real*8 SnsPerNod
common / SnsPerNod / SnsPerNod ( maxnod, maxint )

! Pérdidas del intervalo
real*8 PerIntervalo
common / PerIntervalo / PerIntervalo ( maxint )

! Término constante de la sensibilidad de pérdidas del intervalo
real*8 SnsPerIntIny
common / SnsPerIntIny / SnsPerIntIny ( maxint )

! Multiplicador asociado a las restricciones activas de la aproximacion de perdidas
real*8 MulPerdidas 
common / MulPerdidas / MulPerdidas ( 3*maxint )

! Intervalo correspondiente a las restricciones activas de la aproximacion de perdidas
integer LisMulPerInt 
common / LisMulPerInt / LisMulPerInt ( 3*maxint )

! Apuntador a las restricciones activas de la aproximacion de perdidas por intervalo
integer ApuMulPerInt
common / ApuMulPerInt / ApuMulPerInt ( maxsis, 3*maxint )

! Apuntador a las restricciones activas de la aproximacion de perdidas por intervalo
integer IteMulPerInt
common / IteMulPerInt / IteMulPerInt ( 3*maxint )

!:::::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a zonas de carga
!:::::::::::::::::::::::::::::::::::::::::::::::::::::

! Numero de zonas de carga
integer numzoncar
common / numzoncar / numzoncar

! nombre de zonas de carga
character*16 nomzoncar
common / nomzoncar / nomzoncar ( maxzoncar )

! Indice secuencial de la zona de carga
integer indzoncar
common / indzoncar / indzoncar ( maxzoncar )

! Indice de zona de carga en ems (raw)
integer indzoncarems
common / indzoncarems / indzoncarems ( maxzoncar )

! Numero de nodos por zona de carga
integer numnodzoncar
common / numnodzoncar / numnodzoncar ( maxzoncar )

! Indice del subsistema electrico de la zona de carga
integer siszoncar
common / siszoncar / siszoncar ( maxzoncar )

! Indice del nodo por zona de carga
integer indnodzoncar
common / indnodzoncar / indnodzoncar ( maxzoncar, maxnodzoncar )

! Indice del nodo por zona de carga
real*8 facdisnodzoncar
common / facdisnodzoncar / facdisnodzoncar ( maxzoncar, maxnodzoncar, maxint )

! Marginal de zona de carga por intervalo
real*8 MargZonCar
common / MargZonCar / MargZonCar ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de energia
real*8 MargZonCarEnergia
common / MargZonCarEnergia / MargZonCarEnergia ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de perdidas
real*8 MargZonCarPerdidas
common / MargZonCarPerdidas / MargZonCarPerdidas ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de transmision
real*8 MargZonCarTransmision
common /  MargZonCarTransmision /  MargZonCarTransmision ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo
real*8 MargZonCarMod
common / MargZonCarMod / MargZonCarMod ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de energia
real*8 MargZonCarEnergiaMod
common / MargZonCarEnergiaMod / MargZonCarEnergiaMod ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de perdidas
real*8 MargZonCarPerdidasMod
common / MargZonCarPerdidasMod / MargZonCarPerdidasMod ( maxzoncar, maxint )

! Marginal de zona de carga por intervalo de transmision
real*8 MargZonCarTransmisionMod
common /  MargZonCarTransmisionMod /  MargZonCarTransmisionMod ( maxzoncar, maxint )
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!    Informacion de los arreglos que definen el modelo lineal de 
!    flujos optimos
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




! Numero de variables del problema lineal
integer numvar
common / numvar / numvar

! Numero de elementos diferentes de cero del problema lineal
integer numele
common / numele / numele

! Numero de restricciones del problema lineal
integer numres
common / numres / numres

! Elementos diferentes de cero de la matriz de restricciones
real*8 matval
common / matval / matval ( maxele )

! Indice de la columna del elemento diferente de cero
! de la matriz de restricciones
integer matjcol
common / matjcol / matjcol ( maxele )

! Apunta a inicio de informacion del renglon
integer matiniren
common / matiniren / matiniren ( maxres )

! Vector de lados derechos
real*8 ladder
common / ladder / ladder ( maxres )

! Tipo de restriccion ( "E", "L", "G" )
character tipres
common / tipres / tipres ( maxres )

! Coeficientes de la funcion objetivo
real*8 coefunobj
common / coefunobj / coefunobj ( maxvar )

! Tipo de variable ( "C", "B", "I" )
character tipvar
common / tipvar / tipvar ( maxvar )

! Limite superior de la variable
real*8 cotsup
common / cotsup / cotsup ( maxvar )

! Limite inferior de la variable
real*8 cotinf
common / cotinf / cotinf ( maxvar )

! identificador del problema de flujos de carga (PL)
integer(8) llp
common / llp / llp


! vector solucion del problema lineal (variables primales)
real*8 vecsol
common / vecsol / vecsol ( maxvar )

! vector solucion del problema lineal (variables duales)
real*8 dualsol
common / dualsol / dualsol ( maxvar )

! vector multiplicadores asociados a las cotas simples del problema lineal
real*8 yacsol
common / yacsol / yacsol ( maxvar )


!:::::::::::::::::::::::::::::::::::::::::::::::::::::
!        Datos asociados a areas
!:::::::::::::::::::::::::::::::::::::::::::::::::::::
!Numero de areas
integer numarea
common / numarea / numarea

!Indice del area
integer indarea
common / indarea / indarea ( maxare )

!Nombre del area
character*20 nomarea
common / nomrea / nomarea ( maxare )

END MODULE ParGLORed