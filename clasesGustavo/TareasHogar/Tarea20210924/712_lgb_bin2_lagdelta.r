#Necesita para correr en Google Cloud
#32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria2   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Entrena en UN SOLO mes [202011]
#Usa variables historicas de lag de orden uno y  delta lag

#Optimizacion Bayesiana de hiperparametros de  lightgbm
#usa el interminable  5-fold cross validation
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript         <- "712_lgb_bin2_lagdelta"

karch_dataset    <- "./datasetsOri/paquete_premium.csv.gz"
kmes_apply       <- 202101  #El mes donde debo aplicar el modelo
kmes_train_hasta <- 202011  #Obvimente, solo puedo entrenar hasta 202011

kmes_train_desde <- 202011  #Entreno desde 202011, o sea solo entreno en noviembre-2020

kcanaritos  <-   90
kBO_iter    <-  100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.02 , upper=    0.06),
         makeNumericParam("feature_fraction", lower=    0.1  , upper=    0.4),
         makeIntegerParam("min_data_in_leaf", lower= 1000L   , upper= 8000L),
         makeIntegerParam("num_leaves",       lower=  100L   , upper= 1024L),
         makeNumericParam("prob_corte",       lower=    0.040, upper=    0.055)
        )

campos_malos  <- c( )   #Intencionalmente VACIO

ksemilla_azar  <- 102191  #Aqui poner la propia semilla
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

PROB_CORTE  <- 0.025

fganancia_logistic_lightgbm   <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #aqui esta el inmoral uso de los pesos para calcular la ganancia correcta
  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1 & vpesos > 1, 48750, -1250 ) )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  gc()
  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 999983,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 1/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( 999983 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )


  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds  
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL

   #si tengo una ganancia superadora, genero el archivo para Kaggle
   if(  ganancia > GLOBAL_ganancia_max )
   {
     GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-

     set.seed(ksemilla_azar)

     modelo  <- lightgbm( data= dtrain,
                          param= param_completo,
                          verbose= -100
                        )

    #calculo la importancia de variables
    tb_importancia  <- lgb.importance( model= modelo )
    fwrite( tb_importancia, 
            file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
            sep="\t" )

     prediccion  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )

     Predicted  <- as.integer( prediccion > x$prob_corte )

     entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                      "Predicted"= Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
             sep= "," )
   }

   #logueo 
   xx  <- param_completo
   xx$iteracion_bayesiana  <- GLOBAL_iteracion
   xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
   loguear( xx,  arch= klog )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )
kbayesiana  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_BOlog.txt" )
kimp        <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
 tabla_log  <- fread( klog)
 GLOBAL_iteracion  <- nrow( tabla_log ) -1
 GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
}


#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

campos_lags  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01", "numero_de_cliente","foto_mes", campos_malos) )

#Hago feature Engineering en este mismo script
#agreglo los lags de orden 1
setorderv( dataset, c("numero_de_cliente","foto_mes") )
dataset[ , paste0( campos_lags, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= campos_lags]

#agrego los deltas de los lags, con un "for" nada elegante
for( vcol in campos_lags )
{
   dataset[,  paste0(vcol, "_delta1") := get( vcol)  - get(paste0( vcol, "_lag1"))]
}


#Agrego los canaritos
if( kcanaritos > 0 )
{
  for( i  in 1:kcanaritos)  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]
}


#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes==kmes_apply ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]



#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[  foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta , campos_buenos, with=FALSE]),
                        label=  dataset[ foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta, clase01],
                        weight=  dataset[ foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                        free_raw_data= TRUE
                      )

#elimino el dataset para liberar memoria RAM
rm( dataset )
gc()

#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



#apagado de la maquina virtual, pero NO se borra
system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE)

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


