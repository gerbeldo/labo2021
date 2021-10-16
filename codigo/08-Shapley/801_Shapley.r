#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU
# LightGBM  totalmente puro, con parametros por default, sin quitar ninguna variable

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1/")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#paso la clase a binaria2 que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param= list( objective= "binary",
                                   max_bin= 31,
                                   learning_rate= 0.032,
                                   feature_fraction= 0.29,
                                   min_data_in_leaf= 2750,
                                   num_leaves= 900,
                                   num_iterations= 320)
                    )


#aplico el modelo a los datos sin clase, 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])
                      )

Shapley  <- predict( modelo, 
                     data.matrix( dapply[, campos_buenos, with=FALSE ]),
                     predcontrib= TRUE   # genera valores Shapley
                    )

Shapley  <- as.data.table( Shapley )  #convierto la matrix a mi querido data.table

#Asigno los nombres a cada columna de la matrix Shapley
setnames( Shapley,  c( paste( campos_buenos, "_shap" ), "baseline_shap" ) )

Shapley[  , suma_shap := rowSums( .SD ) ]  #sumo todo, incluyendo baseline
Shapley[  , prob_shap := 1/( 1 + exp( -suma_shap) ) ]  #funcion logistica

Shapley[  , prob := prediccion ]  #La prediccion del modelo

Shapley[  , numero_de_cliente :=  dapply$numero_de_cliente ]
Shapley[  , foto_mes  :=  dapply$foto_mes ]


#genero el archivo para Kaggle
fwrite( Shapley, 
        file= "./work/Shapley_01.txt", 
        sep= "\t" )
