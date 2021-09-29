#esta version se diferencia del script 790 en que max_bin esta fijado en 31
#Los resultados cambian
#la idea del experimento es la misma
#Parto del script  672

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1/")


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

EstimarGananciaCrossVal  <- function( x )
{
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
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  param_completo  <- c( param_basicos, x )

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
  
  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#trabajo con binaria2   pos={BAJA+2, NAJA+1}
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#Aqui van los campos malos que utilice en el script 672
campos_malos  <- c( "mpasivos_margen" )

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos ) )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01,
                        weight=  dataset[ , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)])

#Aqui se deben cargar los mejores hiperparametros encontrados en el 5-fold cross validation del script 672
#Los copio exactamente de MI corrida
param_ganadores  <- list( objective= "binary",
                          num_iterations= 321,
                          learning_rate=  0.0320743773214109,
                          min_data_in_leaf=  2757,
                          num_leaves= 896,
                          feature_fraction= 0.289474922282189,
                          prob_corte= 0.0504070224757321,
                          seed= 999983,
                          max_bin= 31,              #agregado del script 791
                          feature_pre_filter= FALSE
                         )

#genero el modelo con los parametros ganadores
modelo  <- lgb.train( data= dtrain,
                      param= param_ganadores
                    )


#aplico el modelo a los datos sin clase, 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
#Aqui estoy cortando por la probabilidad   0.0504070224757321
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > param_ganadores$prob_corte)  ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file= "./kaggle/german_may_obediente.csv", sep= "," )

EstimarGananciaCrossVal( param_ganadores )  #Esto es nuevo, para German



#Esta corrida da en el Public Leaderboard una humilde ganancia de 20.96719
#que hoy estaria en la posicion 50 del Public Leaderboard, y en el privado tambien en la posicion 50
#totalmente despreciable ante la codicia actual
#peeero

#Esta seccion es la que da origen a la dedicacion  a German May
#¿Que pasaria si NO HAGO CASO  y pruebo cortar con otras probabilidades?
#Un experimento NO se le niega a nadie
#Me permito experimentar cosas que van en contra de mi intuicion
#Voy subiendo a Kaggle las salidas


xgerman  <- param_ganadores  #creo xgerman para no ir tocando  param_ganadores original

#Probabilidad de corte  0.045
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.045 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_45.csv", sep= "," )
xgerman$prob_corte <- 0.045
EstimarGananciaCrossVal( xgerman )

#Probabilidad de corte  0.040
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.040 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_40.csv", sep= "," )
xgerman$prob_corte <- 0.040
EstimarGananciaCrossVal( xgerman )


#Probabilidad de corte  0.035
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.035 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_35.csv", sep= "," )
xgerman$prob_corte <- 0.035
EstimarGananciaCrossVal( xgerman )


#Probabilidad de corte  0.030
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.030 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_30.csv", sep= "," )
xgerman$prob_corte <- 0.030
EstimarGananciaCrossVal( xgerman )


#Probabilidad de corte  0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.025 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_25.csv", sep= "," )
xgerman$prob_corte <- 0.025
EstimarGananciaCrossVal( xgerman )


#Probabilidad de corte  0.020
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.020 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_20.csv", sep= "," )
xgerman$prob_corte <- 0.020
EstimarGananciaCrossVal( xgerman )


#Probabilidad de corte  0.015
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.015 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_15.csv", sep= "," )
xgerman$prob_corte <- 0.015
EstimarGananciaCrossVal( xgerman )


#Si fueron subiendo de a uno los archivos a Kaggle, seguramente estan MUY sorprendidos
#Cometo la infidencia de contar el Private Leaderboard para llevar paz al alma de German May
#  Prob                  gan_public    gan_private    gan_xval
#  0.0504070224757321     21.01719      19.18015      14420000
#  0.045                  21.67137      19.79800      14363750    #disminuye la gan en xval, pero aumenta en el futuro, tanto Public como Private
#  0.040                  22.90890      20.43728      14110000
#  0.035                  23.79226      21.18548      13648750
#  0.030                  23.62558      21.76405      13393750
#  0.025                  21.79221      21.39977      12352500
#  0.020                  21.07135      20.51763      11467500
#  0.015                  18.65046      18.18194       9697500


#German May,  todo esto muestra que la mejor probabilidad de corte  para cross validation
# no es la mejor probabilidad de corte para los datos del futuro
#  con lo cual, dado que estamos en Kaggle, y solo para determinar la prob. de corte, CONVIENE mirar el Public Leaderboard
#  lamento profundamente que la realidad sea mas compleja que "Construya modelos exitosos en diez pasos"

#Finalmente,  German May,  vamos a ver esto en la clase del viernes en la medida que ANTES experimentes
#Experimenta ordenadamente con otros scripts
#Recorda que solo tenes 20 tiros diarios a Kaggle
#Si German no experimenta, el tema no se ve en clase


#Larga vida a la Creatividad y la Experimentacion   https://www.youtube.com/watch?v=0mtctl8ba4g

