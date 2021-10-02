#Este script necesita para correr en Google Cloud
#128GB de memoria RAM
#300GB de espacio en disco Standard Persistent Disk
#8 vCPU

#Feature Engineering con variables extendidas y manuales
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#este script esta pensado para correr en la nube

#limpio la memoria
rm( list=ls() )
gc()

library( "data.table" )
library( "Rcpp" )

#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formual de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')
#------------------------------------------------------------------------------
t0      <-  Sys.time()

vendor <- NA
if( Sys.info()[['sysname']]== "Linux" ) vendor  <- system("sudo dmidecode -s bios-vendor", intern = TRUE)

#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <- ifelse( vendor=="Google", 
                                                "~/buckets/b1/",             #Google Cloud
                                                "~/buckets/b1/crudo/" ) }    #Su propio Linux
)
#defino la carpeta donde trabajo
setwd( directory.root )


karch_generacion  <- "./datasetsOri/paquete_premium.csv.gz"

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread( karch_generacion)

columnas_originales <-  copy(colnames( dataset ))

#acomodo los errores del dataset
dataset[ foto_mes==201701,  ccajas_consultas   := NA ]
dataset[ foto_mes==201702,  ccajas_consultas   := NA ]

dataset[ foto_mes==201801,  internet   := NA ]
dataset[ foto_mes==201801,  thomebanking   := NA ]
dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==201801,  tcallcenter   := NA ]
dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
dataset[ foto_mes==201801,  ccajas_otras   := NA ]

dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos  := NA ]

dataset[ foto_mes==201905,  mrentabilidad     := NA ]
dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201905,  mcomisiones       := NA ]
dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
dataset[ foto_mes==201905,  mactivos_margen  := NA ]
dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

dataset[ foto_mes==201910,  mpasivos_margen  := NA ]
dataset[ foto_mes==201910,  mactivos_margen  := NA ]
dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones       := NA ]
dataset[ foto_mes==201910,  mrentabilidad     := NA ]
dataset[ foto_mes==201910,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201910,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==201910,  ctarjeta_visa_descuentos   := NA ]
dataset[ foto_mes==201910,  ctarjeta_master_descuentos   := NA ]
dataset[ foto_mes==201910,  mtarjeta_visa_descuentos   := NA ]
dataset[ foto_mes==201910,  mtarjeta_master_descuentos    := NA ]
dataset[ foto_mes==201910,  ccajeros_propios_descuentos   := NA ]
dataset[ foto_mes==201910,  mcajeros_propios_descuentos   := NA ]

dataset[ foto_mes==202001,  cliente_vip   := NA ]




#INICIO de la seccion donde se deben hacer cambios con variables nuevas
#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
dataset[ , mv_status02       := Master_status +  Visa_status ]
dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 9, Master_status) , ifelse( is.na(Visa_status), 9, Visa_status) ) ]
dataset[ , mv_status04       := ifelse( is.na(Master_status), 9, Master_status)  +  ifelse( is.na(Visa_status), 9, Visa_status)  ]
dataset[ , mv_status05       := ifelse( is.na(Master_status), 9, Master_status)  +  10*ifelse( is.na(Visa_status), 9, Visa_status)  ]

dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 9, Master_status), 
                                        Visa_status)  ]

dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 9, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
dataset[ , mv_delinquency          := pmax( Master_delinquency, Visa_delinquency, na.rm = TRUE) ]
dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]


#FIN de la seccion donde se deben hacer cambios con variables nuevas

columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )



#grabo con nombre extendido
fwrite( dataset,
        file="./datasets/paquete_premium_ext_corregido.txt.gz",
        sep= "\t" )

#------------------------------------------------------------------------------
#Aqui comienza la creación de variables historicas

#ordeno por  numero_de_cliente y foto_mes
setorder( dataset,  numero_de_cliente, foto_mes )

#Esta es la cantidad de meses que utilizo para la historia
ventana_regresion  <- 6

last <- nrow( dataset )
kcampo_id_idx  <-  match( "numero_de_cliente", names(dataset) )
#----------


#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }


columnas_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )

#agrego al dataset las TENDENCIAS
columnas_originales_a_procesar  <- setdiff( columnas_originales,  columnas_no_procesar  )  

for(  campo  in  columnas_originales_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   dataset[ , paste( campo, "__lag" , sep="" ):= nueva_col[ (4*last +1):(5*last) ]  ]
   
   #Por ahora, no agrego el promedio
   #dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#grabo el archivo completo
#al  agregarle el .gz  al final del nombre del archivo,  data.table sabe que debe comprimir el archivo
fwrite( dataset, 
        file= "./datasets/paquete_premium_hist_corregido.txt.gz", 
        sep="\t" )

gc()
#ahora agrego tendencias TAMBIEN  para las variables nuevas

columnas_extendidas_a_procesar  <- setdiff( columnas_extendidas,  columnas_no_procesar  )  

for(  campo  in  columnas_extendidas_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   dataset[ , paste( campo, "__lag" , sep="" ):= nueva_col[ (4*last +1):(5*last) ]  ]
   
   #Por ahora, no agrego el promedio
   #dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#grabo el archivo completo
#al  agregarle el .gz  al final del nombre del archivo,  data.table sabe que debe comprimir el archivo
fwrite( dataset, 
        file= "./datasets/paquete_premium_exthist_corregido.txt.gz", 
        sep="\t" )



t1      <-  Sys.time()
tiempo  <-  as.numeric(  t1 - t0, units = "secs")

cat( "El Feature Engineering ha corrido en :", tiempo, "  segundos.\n" )


#limpio la memoria
rm( list=ls() )
gc()

#quit( save="no")