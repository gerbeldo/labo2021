#Este script esta dedicado a mostrar el concepto de "Meseta de Ganancia" a Santiago Dellachiesa

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

set.seed( 999983 )

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#trabajo con binaria2   pos={BAJA+2, NAJA+1}
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


dataset[  , azar:= runif(nrow(dataset)) ]  #para dividir a la mitad

#Aqui van los campos malos que utilice en el script 672
campos_malos  <- c( "mpasivos_margen" )

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos ) )

#Defino training como el 50% de los datos
#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[  azar < 0.5, campos_buenos, with=FALSE]),
                        label= dataset[ azar < 0.5, clase01],
                        weight=  dataset[  azar < 0.5, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)])

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


#aplico el modelo al 50% que NO use para entrenar
prediccion  <- predict( modelo, 
                        data.matrix( dataset[ azar>=0.5, campos_buenos, with=FALSE ]))


#me armo un dataset para analizar como cambia la ganancia segun 
#la probabilidad de corte que asigno el modelo

tb_analisis  <- dataset[  azar>=0.5 ,  c("clase_ternaria"),  with=FALSE ]
tb_analisis[   ,  ganancia :=  ifelse( clase_ternaria=="BAJA+2", 48750,  -1250 ) ]

tb_analisis[  , prob := prediccion ]

setorder( tb_analisis,  -prob )   #ordeno por prob descendente, siempre elijo a los de mas prob
tb_analisis[  , ganancia_acumulada :=  cumsum( ganancia ) ]

#calculo cuanto registros estaria enviando
registros_corte  <- nrow(  tb_analisis[  prob > param_ganadores$prob_corte ] )

tope  <- registros_corte*2.2

#grafico segun la cantidad de registros
pdf( "./work/la_meseta_de_Santiago.pdf" )

plot( 1:tope,
      tb_analisis[ 1:tope, ganancia_acumulada],
      main= "Meseta de Ganancias de Santiago",
      xlab= "registros elegidos",
      ylab= "ganancia acumulada",
      type= "l",
      lwd=  2,
      col= "blue"
    )

abline( v=registros_corte, col="darkgreen", lty=1, lwd=1)
dev.off()


#Ahora visto desde las probabilidades, para que no me proteste Santiago
#aqui hay mas carpinteria para que salga a escala

pdf( "./work/la_meseta_prob_de_Santiago.pdf" )
plot( 1:tope,
      rev(tb_analisis[ 1:tope, ganancia_acumulada]),
      main= "Meseta Prob de Ganancias de Santiago",
      xlab= "probabilidad",
      ylab= "ganancia acumulada",
      type= "l",
      lwd=  2,
      col=  "red",
      xaxt="n"
    )

axis(1, at=1:tope, labels=rev( tb_analisis[ 1:tope, round(prob, digits=3)] ))

abline( v=tope-registros_corte, col="darkgreen", lty=1, lwd=1)
dev.off()


