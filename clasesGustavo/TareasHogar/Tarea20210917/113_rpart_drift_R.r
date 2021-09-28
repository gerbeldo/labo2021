#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

setwd("~/buckets/b1/")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202011.csv")

param  <- list( "cp"= -1,
                "minsplit"=   50,
                "minbucket"=  10,
                "maxdepth"=    6 )

#genero el modelo QUITANDO el campo  -mrentabilidad_annual

modelo  <- rpart("clase_ternaria ~ . -mrentabilidad_annual",
                  data= dtrain,
                  xval= 0,
                  contro= param )

#aplico el modeloA  a los datos de 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

prediccion  <- predict( modelo, dapply, type= "prob")

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[ , list( numero_de_cliente, Predicted)  ]

fwrite( entrega, file="./kaggle/113_rpart_drift_R.csv", sep="," )
#ahora debo subir la prediccion a Kaggle y ver como me fue
