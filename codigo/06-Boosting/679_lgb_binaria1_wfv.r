#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local

#8 vCPU

#clase_binaria1   1={BAJA+2}    0={BAJA+1,CONTINUA}
#Optimizacion Bayesiana de hiperparametros de  lightgbm con Walk Forward Validation
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

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

kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "671_lgb_binaria1_MM"
karch_generacion  <- "./datasetsOri/paquete_premium.csv.gz"

kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower= 0.01 , upper=    0.1),
  makeNumericParam("feature_fraction", lower= 0.2  , upper=    1.0),
  makeIntegerParam("min_data_in_leaf", lower= 0    , upper= 8000),
  makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L),
  makeNumericParam("prob_corte",       lower= 0.020, upper=    0.035)
)

campos_malos  <- c( "mpasivos_margen" )   #aqui se deben cargar todos los campos culpables del Data Drifting

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
  
  gan  <- sum( (probs > PROB_CORTE  ) *
                 ifelse( vlabels== 1, 48750, -1250 ) )
  
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
  
  PROB_CORTE <<- x$prob_corte   #asigno la variable global
  
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
  #param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  
  #param_completo  <- c( param_basicos, param_variable, x )
  
  param_completo  <- param_basicos
  
  
  set.seed( 999983 )
  
  
  t0   <-  Sys.time()
  ####
  #Inicializo totales de gnancia, auc e iteraciones
  gan_add = 0
  best_iter_add = 0
  
  #determino total de meses
  total_meses <- length(meses)
  
  for ( meswfv in total_meses + offsets_wfv){
    
    mes_test <- meses[meswfv]
    mes_hasta <- meses[(meswfv-2)]
    mes_desde <- meses[(meswfv-1-meses_entrenamiento)]
    
    print(paste("Entreno de",mes_desde,"a",mes_hasta,"y testeo en",mes_test))        
    
    #dejo los datos en el formato que necesita LightGBM
    dtrain  <- lgb.Dataset( data= data.matrix(  dataset[foto_mes >= mes_desde & foto_mes <= mes_hasta,campos_buenos, with=FALSE]),
                            label= dataset[foto_mes >= mes_desde & foto_mes <= mes_hasta,clase01] )
    
    #dejo los datos en el formato que necesita LightGBM
    dtest  <- lgb.Dataset( data= data.matrix(  dataset[foto_mes == mes_test,campos_buenos, with=FALSE]),
                           label= dataset[foto_mes == mes_test,clase01] )
    
    modelo  <- lgb.train( data= dtrain,
                         eval= fganancia_logistic_lightgbm,
                         valids=list(valid=dtest),
                         param= param_completo,
                         verbose= -100
    )
    
    
    
    #Obtengo ganancia paso
    gan <- max(unlist(modelo$record_evals$valid$ganancia$eval))
    best_iter <- which.max(unlist(modelo$record_evals$valid$ganancia$eval))
    
    #Acumulo en totales de WFV
    gan_add = gan/length(offsets_wfv) + gan_add
    best_iter_add = best_iter/length(offsets_wfv) + best_iter_add
    
  }
  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")  
  
  ganancia  <- unlist(modelo$record_evals$valid$ganancia$eval)[ modelo$best_iter ]
  
  ganancia_normalizada  <-  ganancia
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelo$best_iter)  #esta es la forma de devolver un parametro extra
  
  param_completo$num_iterations <- as.integer(best_iter_add)  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL
  
  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if(  ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-
    
    set.seed(ksemilla_azar)
    
    
    mes_hasta <- meses[(total_meses-2)]
    mes_desde <- meses[(total_meses-1-meses_entrenamiento)]
    
    print(paste("SALIDA PREDICCION! Entreno de",mes_desde,"a",mes_hasta,"y testeo en",mes_test))        
    
    #dejo los datos en el formato que necesita LightGBM
    dtrain  <- lgb.Dataset( data= data.matrix(  dataset[foto_mes >= mes_desde & foto_mes <= mes_hasta,campos_buenos, with=FALSE]),
                            label= dataset[foto_mes >= mes_desde & foto_mes <= mes_hasta,clase01] )    
    
    
    modelo  <- lgb.train( data= dtrain,
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
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  loguear( xx,  arch= klog )
  
  return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_", kscript, ".txt" )
kimp        <- paste0("./work/E",  kexperimento, "_", kscript, "_" )
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


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread(karch_generacion)  

#cargo los datos donde voy a aplicar el modelo
dapply  <- dataset[foto_mes==202101] #leo los datos donde voy a aplicar el modelo


#Genera lista de meses
meses <- sort(unique(dataset$foto_mes))



#Esta seccion se puede hacer para reducir el dataset.
#Tener en cuenta que debe tener los 2 meses finales (202011+202101) mas todos los 
#meses que se entrenen en walk forward considerando la cantidad de meses en cada 
#entrenamiento. Ejemplo: Se hace Walk forward 
#con Sep 2020 + Oct 2020 + Nov 2020 y se entrena sobre 12
#meses necesito conservar 17 meses

#2 Meses finales Dic 2021 + Nov 2020
#3 Meses para cubrir hasta el primer walk forward 
#12 meses de entrenamiento

#Reduzco el dataset
meses_a_usar <- 17
meses_entrenamiento <- 12
total_meses <- length(meses)
mes_corte <- meses[total_meses-meses_a_usar]
dataset <- dataset[foto_mes>= mes_corte]

#redeterino lista de meses
meses <- sort(unique(dataset$foto_mes))
total_meses <- length(meses)

#Prueba Walk Forward Validation
#Determino total de meses
total_meses <- length(meses)

#Defino sobre que meses (offset) voy a hacer el WFV se recomienda definirlo y NO TOCARLO MAS para que los resultados 
#sean comparables
######
offsets_wfv <- c(-2,-3,-4)

for ( meswfv in total_meses + offsets_wfv){
  print(meswfv)
  mes_test <- meses[meswfv]
  mes_hasta <- meses[(meswfv-2)]
  mes_desde <- meses[(meswfv-1-meses_entrenamiento)] #ATENCION!!! Resta 12 porque entreno en HASTA 12 meses. Si se amplia el espacio hay que modificar ese numero por el maximo a usar
  print(paste("Entreno de",mes_desde,"a",mes_hasta,"y testeo en",mes_test))
}
############


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

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
#if( vendor=="Google" ) { system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE) }

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


#quit( save="no" )


