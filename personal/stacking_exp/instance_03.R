# stacking 03
library(data.table)
data <- fread("~/buckets/b1/datasetsOri/paquete_premium.csv.gz")

base_script_dir <- "~/labo2021/personal/stacking_exp/stacking_03"


for (f in dir(base_script_dir)) {
  
  dataset <- copy(data)
  
  # source dataset generation script
  source(paste0(base_script_dir, "/", f))
  
  # generate dataset
  correr_todo(palancas, dataset)
  
  # model
  source("~/labo2021/personal/stacking_exp/822_epic.r")
  
}





