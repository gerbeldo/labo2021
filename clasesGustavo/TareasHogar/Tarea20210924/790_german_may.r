#Parto del script  672

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1/")

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
                        label= dataset$clase01 )

#Aqui se deben cargar los mejores hiperparametros encontrados en el 5-fold cross validation del script 672
#Los copio exactamente de MI corrida
param_ganadores  <- list( objective= "binary",
                          num_iterations= 321,
                          learning_rate=  0.0320743773214109,
                          min_data_in_leaf=  2757,
                          num_leaves= 896,
                          feature_fraction= 0.289474922282189,
                          prob_corte= 0.0504070224757321,
                          seed= 999983
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


#Esta corrida da en el Public Leaderboard una humilde ganancia de 20.96719
#que hoy estaria en la posicion 50 del Public Leaderboard, y en el privado tambien en la posicion 50
#totalmente despreciable ante la codicia actual
#peeero

#Esta seccion es la que da origen a la dedicacion  a German May
#¿Que pasaria si NO HAGO CASO  y pruebo cortar con otras probabilidades?
#Un experimento NO se le niega a nadie
#Me permito experimentar cosas que van en contra de mi intuicion
#Voy subiendo a Kaggle las salidas

#Probabilidad de corte  0.045
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.045 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_45.csv", sep= "," )


#Probabilidad de corte  0.040
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.040 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_40.csv", sep= "," )


#Probabilidad de corte  0.035
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.035 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_35.csv", sep= "," )


#Probabilidad de corte  0.030
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.030 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_30.csv", sep= "," )

#Probabilidad de corte  0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.025 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_25.csv", sep= "," )

#Probabilidad de corte  0.020
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.020 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_20.csv", sep= "," )

#Probabilidad de corte  0.015
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion >  0.015 )  ) #genero la salida
fwrite( entrega, file= "./kaggle/german_may_15.csv", sep= "," )


#Si fueron subiendo de a uno los archivos a Kaggle, seguramente estan MUY sorprendidos
#Cometo la infidencia de contar el Private Leaderboard para llevar paz al alma de German May
#  Prob                  gan_public    gan_private
#  0.0504070224757321     20.96719      18.37301     #la corrida original
#  0.045                  21.81304      18.88729
#  0.040                  22.58389      19.46050
#  0.035                  23.02974      20.41228
#  0.030                  23.85892      21.66941
#  0.025                  23.30058      21.62119
#  0.020                  22.14221      20.71049
#  0.015                  19.34214      17.80695


#Finalmente,  German May,  vamos a ver esto en la clase del viernes en la medida que ANTES experimentes
#Experimenta ordenadamente con otros scripts
#Recorda que solo tenes 20 tiros diarios a Kaggle
#Si German no experimenta, el tema no se ve en clase


#Larga vida a la Creatividad y la Experimentacion   https://www.youtube.com/watch?v=0mtctl8ba4g

