# stacking 01

data <- fread("~/buckets/b1/datasetsOri/paquete_premium.csv.gz")

instance <- 2

for (f in dir(paste0("~/labo2021/personal/stacking_exp/stacking_0", instance))) {

  dataset <- copy(data)
  
  # source dataset generation script
  source(f)
  
  # generate dataset
  correr_todo(palancas, dataset)
  
  # model
  source("~/labo2021/personal/stacking_exp/822_epic.r")
  
}





