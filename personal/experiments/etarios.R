source("~/labo2021/personal/experiments/811_dataset_epic_etarios.R")

add_grupo_etario <- function(dataset) {
  dataset[, etario := fcase(
    cliente_edad <= 35, 0L,
    cliente_edad > 35 & cliente_edad < 50, 1L,
    cliente_edad >= 50, 2L
  )]
}


divide_and_etario <- function(palancas, dataset) {
  
  add_grupo_etario(dataset)
  
  etarios <- unique(dataset[, etario])
  
  for (i in etarios) {
    
    data <- dataset[etario == i]
    
    correr_todo(palancas, data)

    fwrite(data,
           file = paste0("./datasets/etario_", i, "_", palancas$version, ".csv.gz"),
           logical01 = TRUE,
           sep = ",")
    
  }
}


divide_and_etario(palancas, dataset)



# TESTING
# dataset <- data.table(cliente_edad = c(1, 20 , 15, 35, 36, 45, 49, 50, 51, 60))
# 
# add_grupo_etario(dataset)
# 
# dataset[etario == 1L]
# 
# write_datasets(dataset)
