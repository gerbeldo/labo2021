#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

setwd("~/buckets/b1/")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202011.csv")

#genero el modelo
modeloA  <- rpart("clase_ternaria ~ .", 
                  data= dtrain, 
                  xval= 0, 
                  cp= -1 )

#aplico el modeloA  a los datos de 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

prediccionA  <- predict( modeloA, dapply , type = "prob")

dapply[ , prob_baja2 := prediccionA[, "BAJA+2"] ]
dapply[ , Predicted := as.numeric(prob_baja2 > 0.025) ]

entregaA  <- dapply[   , list( numero_de_cliente, Predicted)  ]

fwrite( entregaA, file="./kaggle/111_rpart_default.csv", sep="," )
#ahora debo subir la prediccion a Kaggle y ver como me fue
