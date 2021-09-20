#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/crudo/")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202011.csv")
#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")



#cargar aqui los parametros
param1 <- list( "cp"= -1,
                "minsplit"=  200,
                "minbucket"= 100,
                "maxdepth"=    6 )

param2 <- list( "cp"= -1,
                "minsplit"=  50,
                "minbucket"= 10,
                "maxdepth"=   6 )


# quito   mrentabilidad_annual   --------------------------------------------------------
formulita <- "clase_ternaria ~ . -mrentabilidad_annual"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_R.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_R.csv"), 
        sep= "," )

# quito   mactivos_margen   --------------------------------------------------------
formulita <- "clase_ternaria ~ . -mactivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_A.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_A.csv"), 
        sep= "," )
        
        
# quito   mpasivos_margen   --------------------------------------------------------
formulita <- "clase_ternaria ~ . -mpasivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_P.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_P.csv"), 
        sep= "," )

# quito   mrentabilidad_annual , mactivos_margen   --------------------------------------------------------
formulita <- "clase_ternaria ~ . -mrentabilidad_annual -mactivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_RA.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_RA.csv"), 
        sep= "," )


# quito   mrentabilidad_annual , mpasivos_margen---------------------------------------------------
formulita <- "clase_ternaria ~ . -mrentabilidad_annual -mpasivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_RP.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_RP.csv"), 
        sep= "," )


# quito   mactivos_margen , mpasivos_margen---------------------------------------------------
formulita <- "clase_ternaria ~ . -mactivos_margen -mpasivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_AP.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_AP.csv"), 
        sep= "," )


# quito   mrentabilidad_annual  mactivos_margen , mpasivos_margen------------------------

formulita <- "clase_ternaria ~ . -mrentabilidad_annual -mactivos_margen -mpasivos_margen"

modelo1  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param1 )

prediccion1  <- predict( modelo1, dapply , type= "prob") #aplico el modelo

entrega1  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                 "Predicted" = as.numeric(prediccion1[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega1, 
        file= paste0( "./kaggle/param1_RAP.csv"), 
        sep= "," )

modelo2  <- rpart(formulita,
                  data= dtrain,
                  xval= 0,
                  control= param2 )

prediccion2  <- predict( modelo2, dapply , type= "prob") #aplico el modelo

entrega2  <- as.data.table( list( "numero_de_cliente" = dapply$numero_de_cliente,
                                  "Predicted" = as.numeric(prediccion2[, "BAJA+2"] > 0.025)) ) #genero la salida
#genero el archivo para Kaggle
fwrite( entrega2, 
        file= paste0( "./kaggle/param2_RAP.csv"), 
        sep= "," )
