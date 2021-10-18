library(data.table)

e1023 <- fread("~/Downloads/kaggle_E1023_E1023_822_epic_095.csv")
e1024 <- fread("~/Downloads/kaggle_E1024_E1024_822_epic_049.csv")
e1025 <- fread("~/Downloads/kaggle_E1025_E1025_822_epic_134.csv")

sum(nrow(e1023), nrow(e1024), nrow(e1025))

kaggle <- rbindlist(list(e1023, e1024, e1025))

fwrite(kaggle, "~/Downloads/e1023_e0124_e1025_etarios.csv")
