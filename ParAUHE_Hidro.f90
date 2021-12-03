!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Declaración de variables y parámetros necesarios para           *
!         la asignacion de unidades hidroelectricas                       *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Junio 2013                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
!
	Module ParAuHeHidro

    implicit none

!    include 'C:\AUCHT\Parametros\param.par'  !Windows (comentarizar para Linux)
include 'param.par'  !Linux (comentarizar para Windows)
    
	
	INTEGER*4, PARAMETER :: unlg12_1 = 734   ! Archivo para escritura de debbuger hidro	
	
	real * 8 potfijah, GENUNH, GAMNVI, GAMXVI, NMDVI, QMAXUN, QMINUN, &
	         CARGA, RERARG, RROHIR, NIVINI, ELUTIL, VOUTIL, NAMINO, VMINI, GQC, CARDIS, &
	         Alt_net_asig, qmxefu, COEPER, perdida_ductos, qmxgasto, qmngasto, NAMO, &
	         APOEMB, ESCNTO, ESCNTO_HX, VERTDO, qmxgastodes, qmngastodes, qgasto, A_hatd, &
	         B_hatd, C_hatd, VolEmb, NIVEMB, NDVI, Alt_net_des, GSDES1, nivdes, Alt_net_desmin, &
	         Alt_net_desmax, qmxmod, ApoVaUp, AguaEnViaje, qgastores, vol_util_res
	
	integer DH, AH, CH, nmcuen, dnemva, cueaem, nmembc, DNVIOU, &
	        DNVIIN, nmviasd, nmviasc, CUEAVI, DNUNPH, NPLHID, IVIA, NOUN, cueapl, &
	        nphvi, cueaun, dnphvi, plantas_x_via, MOUNHI, nuti, ITIPOU, embapl, &
	        unidad_x_embalse, local_uni_emb, numuni_x_embalse, viadaun, &
	        VIAINC, embviaconv, DNVICO 
	
	character*12 nomval
	character*6 NOMVIA, fecha1, NOMEMB

	character*9 NOMPLAH
	character*8 fechas, fecha2
	character*11 fecha3
	character*15 fechain, fechafi

    INTEGER arrgent2, arrgent3
!------------------------------------------------------------------------------
!	Número del vaso origen	
	common / arrgent2 / arrgent2 ( nmxvia )
!	Número del vaso destino	
	common / arrgent3 / arrgent3 ( nmxvia )
!------------------------------------------------------------------------------
    
!------------------------------------------------------------------------------
!	Numero de cuencas en el sistema
	common / nmcuen / nmcuen
!------------------------------------------------------------------------------
!	Disponibilidad de unidades hidro
	common / DH / DH ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Asignabilidad de unidades hidro
	common / AH/ AH ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Disponibilidad de unidades hidro
	common / CH / CH ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Potencia de Generacion Fija unidades hidro
	common / POTFIJAH / POTFIJAH ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Vector de salida con el punto base de las unidades hidro
	common / GENUNH / GENUNH ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Apuntador que indica en que localidad esta el primer embalse de la cuenca
	common / dnemva / dnemva ( nmxcue + 1 )
!------------------------------------------------------------------------------
!	Nombre de la cuenca
	common / nomval / nomval ( nmxcue )
!------------------------------------------------------------------------------
!   Nombre del embalse
    common / NOMEMB / NOMEMB ( nmxemb )
!------------------------------------------------------------------------------
!	Indica a que cuenca pertenece el embalse
	common / cueaem / cueaem ( nmxemb )
!------------------------------------------------------------------------------
!	Numero de embalses en la cuenca
	common / nmembc / nmembc ( nmxcue )
!------------------------------------------------------------------------------
!	Apuntador que indica en que localidad empieza la primera via divergente del embalse
	common / DNVIOU / DNVIOU ( nmxemb )
!------------------------------------------------------------------------------
!	Apuntador que indica en que localidad empieza la primera via convergente del embalse
	common / DNVIIN / DNVIIN ( nmxemb )	
!------------------------------------------------------------------------------
!	Numero de vias divergentes del embalse
	common / nmviasd / nmviasd ( nmxemb )	
!------------------------------------------------------------------------------
!	Numero de vias convergentes del embalse
	common / nmviasc / nmviasc ( nmxemb )	
!------------------------------------------------------------------------------
!	Indica a que cuenca pertenece la via divergente
	common / CUEAVI / CUEAVI ( nmxvia )	
!------------------------------------------------------------------------------
!	Apuntador que indica en que localidad empieza la primera unidad de la planta
	common / DNUNPH / DNUNPH ( nmxpla )	
!------------------------------------------------------------------------------
!	Numero total de plantas hidro del sistema
	common / NPLHID / NPLHID
!------------------------------------------------------------------------------
!	Via divergente donde descarga la planta
	common / IVIA / IVIA ( NMXPLA )
!------------------------------------------------------------------------------
!	gasto maximo de la planta de acuerdo al modelo
	common / qmxmod / qmxmod ( NMXPLA )
!------------------------------------------------------------------------------
!	Numero de unidades en la planta
	common / NOUN / NOUN ( NMXPLA ) 
!------------------------------------------------------------------------------
!	Cuenca a la que pertenece la planta
	common / cueapl / cueapl ( NMXPLA ) 
!------------------------------------------------------------------------------
!	Embalse al que pertenece la planta
	common / embapl / embapl ( NMXPLA ) 
!------------------------------------------------------------------------------
!	No. de plantas que desfogan sobre la via
	common / nphvi / nphvi ( nmxvia )
!------------------------------------------------------------------------------
!	Cuenca a la que pertenece la unidad
	common / cueaun / cueaun ( maxuh )  
!------------------------------------------------------------------------------
!	Apuntador que indica en que localidad empieza la primera planta que descarga sobre la via
	common / dnphvi / dnphvi ( nmxvia )  
!------------------------------------------------------------------------------
!	Plantas ordenadas deacuerdo a la vía de desfogue, usa apuntador dnphvi
	common / plantas_x_via / plantas_x_via ( NMXPLA )  
!------------------------------------------------------------------------------
!	Nombre de la planta hidro
	common / NOMPLAH / NOMPLAH ( nmxpla )	
!------------------------------------------------------------------------------
!	Nombre de la via divergente
	common / NOMVIA / NOMVIA ( nmxvia )	
!------------------------------------------------------------------------------
!	Gasto minimo en la via divergente Se lee en m^3/s y queda en m^3/hr
	common / GAMNVI / GAMNVI ( nmxvia )	
!------------------------------------------------------------------------------
!	Gasto maximo en la via divergente Se lee en m^3/s y queda en m^3/hr
	common / GAMXVI / GAMXVI ( nmxvia )	
!------------------------------------------------------------------------------
!	Nivel medio de desfogue de la via divergente se lee en msnm
	common / NMDVI / NMDVI ( nmxvia )	
!------------------------------------------------------------------------------
!	Nivel de desfogue de la via divergente en msnm (calculado a partir de resultados de AU)
	common / NDVI / NDVI ( nmxvia, maxint )	
!------------------------------------------------------------------------------
!	Modelo de la unidad hidro
	common / MOUNHI / MOUNHI ( maxuh )	
!------------------------------------------------------------------------------
!	Numero de modelos hidro distintos presentes en el sistema
	common / nuti / nuti
!------------------------------------------------------------------------------
!	Gasto maximo de la unidad hidro en m^3/s
	common / QMAXUN / QMAXUN ( NMXPMU, NMXTIU ) 
!------------------------------------------------------------------------------
!	Gasto minimo de la unidad hidro en m^3/s
	common / QMINUN / QMINUN ( NMXPMU, NMXTIU ) 
!------------------------------------------------------------------------------
!	Carga (altura) de diseño en m
	common / CARGA / CARGA ( NMXPMU, NMXTIU ) 
!------------------------------------------------------------------------------
!	Reserva rapida hidro
	common / RERARG / RERARG ( maxgrure, MAXINT )
!------------------------------------------------------------------------------ 
!	Reserva rodante hidro
	common / RROHIR / RROHIR ( maxgrure, MAXINT ) 
!------------------------------------------------------------------------------ 
!	Nivel del embalse en condiciones iniciales dado msnm
	common / NIVINI / NIVINI ( nmxemb ) 	
!------------------------------------------------------------------------------ 
!	Nivel del embalse dado msnm (calculado a partir de resultado de AU)
	common / NIVEMB / NIVEMB ( nmxemb, MAXINT ) 	
!------------------------------------------------------------------------------ 
!	volumen de la curva de 9 puntos de volumen contra altura del embalse dato dado en millones de m^3
	common / VOUTIL / VOUTIL ( NMXPME, nmxemb ) 
!------------------------------------------------------------------------------ 
!	altura de la curva de 9 puntos de volumen contra altura del embalse dato dado en msnm
	common / ELUTIL / ELUTIL ( NMXPME, nmxemb ) 		
!------------------------------------------------------------------------------ 
!	nivel de aguas minimo dado en msnm
	common / NAMINO / NAMINO ( nmxemb ) 	
!------------------------------------------------------------------------------ 
!	nivel de aguas maximo dado en msnm
	common / NAMO / NAMO ( nmxemb ) 			
!------------------------------------------------------------------------------ 
!	volumen en condiciones iniciales del embalse dato dado en millones de m^3
	common / VMINI / VMINI ( nmxemb ) 	
!------------------------------------------------------------------------------
!	Coeficientes a multiplicar por la altura de la funcion de generacion contra gasto y altura 
	common / GQC / GQC ( NMXPMU, NMXTIU ) 
!------------------------------------------------------------------------------
!	Carga (altura) de diseño de la unidad por modelo
	common / CARDIS / CARDIS ( nmxtiu ) 
!------------------------------------------------------------------------------
!	Altura neta aproximada para la asignacion de unidades, cte. para todo intervalo
!   calculada por via divergente
	common / Alt_net_asig / Alt_net_asig ( nmxvia ) 
!------------------------------------------------------------------------------
!	Altura neta calculada a partir de la AU y usada para el despacho de unidades
!   calculada por via divergente
	common / Alt_net_des / Alt_net_des ( nmxvia, maxint ) 
!------------------------------------------------------------------------------
!	Altura neta maxima calculada para la AU en base a turbinado maximo
!   calculada por via divergente
	common / Alt_net_desmax / Alt_net_desmax ( nmxvia, maxint ) 
!------------------------------------------------------------------------------
!	Altura neta minima calculada para la AU en base a turbinado minimo
!   calculada por via divergente
	common / Alt_net_desmin / Alt_net_desmin ( nmxvia, maxint ) 
!------------------------------------------------------------------------------
!	Gasto a maxima eficiencia por unidad, cardinalidad por planta porque todas las unidades de la planta
!   tienene el mismo modelo
	common / qmxefu / qmxefu ( NMXPLA ) 
!------------------------------------------------------------------------------
!	Coeficiente de perdidas por planta (igual para todas las unidades de la planta)
	common / COEPER / COEPER ( nmxpla ) 
!------------------------------------------------------------------------------
!	Perdida en ductos por unidad, cardinalidad por planta porque todas las unidades de la planta
!   tienene el mismo modelo
	common / perdida_ductos / perdida_ductos ( NMXPLA )
!------------------------------------------------------------------------------
!	Numero de unidades por embalse
    common / numuni_x_embalse / numuni_x_embalse ( nmxemb ) 
!------------------------------------------------------------------------------
!	Lista de unidades por embalse, usar con numuni_x_embalse, local_uni_emb
    common / unidad_x_embalse / unidad_x_embalse ( maxuh )  	 	
!------------------------------------------------------------------------------
!	Lista de unidades por embalse, usar con numuni_x_embalse, local_uni_emb
    common / local_uni_emb / local_uni_emb ( nmxemb )  	 
!------------------------------------------------------------------------------
!	gasto maximo de la unidad usando altura aproximada para AU en m^3/s
    common / qmxgasto / qmxgasto ( maxuh )  	     
!------------------------------------------------------------------------------
!	gasto minimo de la unidad usando altura aproximada para AU en m^3/s
    common / qmngasto / qmngasto ( maxuh )  
!------------------------------------------------------------------------------
!	arreglo que indica en que via divergente desfoga la unidad
    common / viadaun / viadaun ( maxuh ) 
!------------------------------------------------------------------------------
!	Aportacion neta al embalse
    common / APOEMB / APOEMB ( nmxemb, MAXINT ) 
!------------------------------------------------------------------------------
!	agua en viaje del escenario anterior
    common / AguaEnViaje / AguaEnViaje ( nmxemb, MAXINT ) 
!------------------------------------------------------------------------------
!	Numero de la via convergente al embalse
    common / VIAINC / VIAINC ( nmxemb ) 
!------------------------------------------------------------------------------
!	Escurrimiento neto por embalse y por dia (aportacion natural - otros usos agua
    common / ESCNTO / ESCNTO ( NMXEMB, maxdia )
!------------------------------------------------------------------------------
!	Escurrimiento neto por embalse, por dia, intervalo exahorario (aportacion natural - otros usos agua
    common / ESCNTO_HX / ESCNTO_HX ( NMXEMB, maxdia , 4 )
!------------------------------------------------------------------------------
!	Vertido programado sobre vias
    common / VERTDO / VERTDO ( nmxvia, MAXINT )
!------------------------------------------------------------------------------
!   Lista de embalses con vias convergentes, 0 si no tiene via convergente, no. embalse si tiene via convergente
    common / embviaconv /  embviaconv ( NMXEMB )
!------------------------------------------------------------------------------
!   !Vector que contiene el numero de la via convergente al embalse cuando dicho numero es diferente de cero
    common / DNVICO /  DNVICO ( NMXEMB )
!------------------------------------------------------------------------------
!	gasto maximo de la unidad usando altura real para despacho en m^3/s
    common / qmxgastodes / qmxgastodes ( maxuh, MAXINT )  	     
!------------------------------------------------------------------------------
!	gasto minimo de la unidad usando altura real para despacho en m^3/s
    common / qmngastodes / qmngastodes ( maxuh, MAXINT )  
!------------------------------------------------------------------------------
!	gasto de la unidad usando altura aproximada para AU en m^3/s (resultado AU)
    common / qgasto / qgasto ( maxuh, MAXINT )  
!------------------------------------------------------------------------------
!	Coeficiente independiente de la funcion de generacion hidro (depende de la altura)
    common / A_hatd / A_hatd ( maxuh, MAXINT )  
!------------------------------------------------------------------------------
!	Coeficiente independiente de la funcion de generacion hidro (depende de la altura)
    common / B_hatd / B_hatd ( maxuh, MAXINT )  
!------------------------------------------------------------------------------
!	Coeficiente independiente de la funcion de generacion hidro (depende de la altura)
    common / C_hatd / C_hatd ( maxuh, MAXINT )  
!------------------------------------------------------------------------------
!	Volumen del embalse por hora (resultado AU)
    common / VolEmb / VolEmb ( nmxemb, MAXINT )  
!------------------------------------------------------------------------------
!	Gasto sobre la via de desfogue 9 puntos en millones de metros cubicos
    common / GSDES1 / GSDES1 (NMXPMD , NMXVIA)
!------------------------------------------------------------------------------
!	Elevacion en la via de desfogue 9 puntos en m
    common / nivdes / nivdes (NMXPMD , NMXVIA)
!------------------------------------------------------------------------------
!	Elevacion en la via de desfogue 9 puntos en m
    common / ApoVaUp / ApoVaUp ( nmxemb, maxint )
!------------------------------------------------------------------------------
!	Gasto por unidad en m^3/s de la evaluacion cuadratica
    common / qgastores / qgastores ( maxuh, MAXINT )
!------------------------------------------------------------------------------
!	Vector fecha inicial en año, mes y dia
    common / fechas / fechas ( MAXDIA )
!------------------------------------------------------------------------------
!	Vector fecha inicial en año, mes y dia (sin 20 en el año)	
    common / fecha1 / fecha1 ( MAXDIA )
!------------------------------------------------------------------------------
!	Vector fecha inicial dia de la semana, año, mes y dia (sin 20 en el año)	
    common / fecha2 / fecha2 ( MAXDIA )
!------------------------------------------------------------------------------
!	Vector fecha inicial dia de la semana, dia, mes y año (con :) 
    common / fecha3 / fecha3 ( MAXDIA )
!------------------------------------------------------------------------------
!	Fecha inicial
    common / fechain / fechain
!------------------------------------------------------------------------------
!	Fecha inicial
    common / fechafi / fechafi
!   Volumen util del embalse del resultado lineal    
    common / vol_util_res / vol_util_res ( nmxemb, maxint )
    
    
	
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!Datos de Jose Luis
	
		
!       ................................................................
!       Informacion de sobre vias, embalse y cuencas
!       ................................................................
		 integer TiViAgu, PoliEmb
         integer nmuniemb, UniEmb

         real*8 CIndGLH, CLinGLH, CIndGLHWQ, CLinGLHQ, CLinGLHW
         real*8 ApoNetEmb, MValor, QTvfin, VolMxEmb, VolMnEmb
         real*8 EMxExt, EMnExt, WEmbFin, EFijEmb, TfijEmb, DefiAguEmb, ExceAguEmb

 
!        Tiempos de viaje del agua en las vias
		 common / TiViAgu / TiViAgu ( nmxvia )

!        Volumen turbinado sobre las vias que se desea dejar 
!        al final del escenario de planeacion
		 common / QTvfin / QTvfin ( nmxvia )

!        Coeficientes de los terminos independientes en 
!        la representacion lineal de la generacion Hidro 		 
		 common / CIndGLH / CIndGLH ( maxuh, maxint )

!        Coeficientes de los terminos lineales en la
!        representacion lineal de la generacion Hidro 		 
		 common / CLinGLH / CLinGLH ( maxuh, maxint )

!        Coeficientes de los terminos independientes en 
!        la representacion lineal de la generacion Hidro
!        con respecto a gasto y volumen 		 
		 common / CIndGLHWQ / CIndGLHWQ ( maxuh, maxint )

!        Coeficientes de los terminos lineales en la
!        representacion lineal de la generacion Hidro 	
!        con respecto a gasto	 
		 common / CLinGLHQ / CLinGLHQ ( maxuh, maxint )
		 
!        Coeficientes de los terminos lineales en la
!        representacion lineal de la generacion Hidro 	
!        con respecto a volumen	 
		 common / CLinGLHW / CLinGLHW ( maxuh, maxint )

!        Volumenes maximos permitidos en embalses		 
		 common / VolMxEmb / VolMxEmb ( nmxemb )

!        Volumenes minimos permitidos en embalses		 
		 common / VolMnEmb / VolMnEmb ( nmxemb )

!        Numero de unidades asociadas a los embalses		 
		 common / nmuniemb / nmuniemb ( nmxemb )

!        Unidades asociadas a los embalses		 
		 common / UniEmb / UniEmb ( maxuh, nmxemb )

!        Politicas de operacion en embalses		 
		 common / PoliEmb / PoliEmb ( nmxemb )

!        Costo variable para politica de maxima extraccion en embalses		 
		 common / EMxExt / EMxExt

!        Costo variable para politica de minima extraccion en embalses		 
		 common / EMnExt / EMnExt 

!        Volumen final para embalses con politica de cota final
		 common / WEmbFin / WEmbFin ( nmxemb )

!        Energia a producir para embalses con politica de energia fija
		 common / EFijEmb / EFijEmb ( nmxemb )

!        Volumen a turbinar para embalses con politica de turbinado fijo
		 common / TfijEmb / TfijEmb ( nmxemb )

!        Aportacion neta en los embalses (escurrimientos - otros usos + derrames)		 
		 common / ApoNetEmb / ApoNetEmb ( nmxemb, maxint )

!        Factor de conversion igual a 3600s/h 		 
		 common / MValor / MValor
	
!        Variable ficticia de deficit de agua en los embalses
		 common / DefiAguEmb / DefiAguEmb ( nmxemb, maxint )
	 
!        Variable ficticia de excedente de agua en los embalses
		 common / ExceAguEmb / ExceAguEmb ( nmxemb, maxint )

	
	
	End Module


