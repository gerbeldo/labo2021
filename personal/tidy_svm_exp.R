library(data.table)
library(tidymodels)
library(themis)
library(kernlab)

#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
#defino la carpeta donde trabajo
setwd( directory.root )

campos_malos  <- c("foto_mes", "tpaquete3", "numero_de_cliente", "clase_ternaria")

karch_dataset    <- "./datasets/dataset_epic_v007.csv.gz"   #este dataset se genero en el script 811_dataset_epic.r

kapply_mes       <- c(202101)  #El mes donde debo aplicar el modelo

ktest_mes_hasta  <- 202011  #Esto es lo que uso para testing
ktest_mes_desde  <- 202011

ktrain_subsampling  <- 0.1   #el undersampling que voy a hacer de los continua

ktrain_mes_hasta    <- 202010  #Obviamente, solo puedo entrenar hasta 202011
ktrain_mes_desde    <- 201901  
ktrain_meses_malos  <- c( 202006 )  #meses que quiero excluir del entrenamiento


kgen_mes_hasta    <- 202011   #La generacion final para Kaggle, sin undersampling
kgen_mes_desde    <- 201901


ksemilla_azar  <- 4376261  #Aqui poner la propia semilla
#------------------------------------------------------------------------------

#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes %in% kapply_mes ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

setorder(dataset, cols = "foto_mes")

# elimino meses no utiles para train
dataset <- dataset[!foto_mes %in% c(202012, 202101)]

#Defino los datos donde entreno, con subsampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )
dataset[    foto_mes>= ktrain_mes_desde  &
              foto_mes<= ktrain_mes_hasta & 
              !( foto_mes %in% ktrain_meses_malos ) & 
              ( clase01==1 | vector_azar < ktrain_subsampling ),  
            entrenamiento:= 1L ]  #donde entreno

dataset_eff <- dataset[entrenamiento == 1]

dataset_eff[, (campos_malos) := NULL]



#------------------------------------------------------------------------------

split <- initial_time_split(dataset_eff, prop = 3/4, strata = clase01)

train <- training(split)
test <- testing(split)

set.seed(1697)
folds <- vfold_cv(train, v = 3)


#------------------------------------------------------------------------------
data_pre_proc <-
  recipe(clase01 ~ ., data = train) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune()) %>%
  step_downsample(clase01)



svm_mod <-
  svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab")


svm_wflow <-
  workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(data_pre_proc)

svm_set <- parameters(svm_wflow)
svm_set

# set space for searching number of principal components
svm_set <- 
  svm_set %>% 
  update(num_comp = num_comp(c(0L, 500L)))


all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)


set.seed(12)
search_res <-
  svm_wflow %>% 
  tune_bayes(
    resamples = folds,
    # To use non-default parameter ranges
    param_info = svm_set,
    # Generate five at semi-random to start
    initial = 5,
    iter = 50,
    # How to measure performance?
    metrics = metric_set(roc_auc),
    control = control_bayes(no_improve = 30, verbose = TRUE)
  )

stopCluster(cl)