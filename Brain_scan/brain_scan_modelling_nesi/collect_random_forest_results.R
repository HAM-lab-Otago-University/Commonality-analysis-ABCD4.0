args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"

data_input <- readRDS(file = paste0(datafolder, "/random_forest_data/", dataset))

train_list <- data_input[["train_list"]]

batch_ids <- names(train_list)

results <- list()

for(batch_idx in 1:length(batch_ids)){
tmp <- readRDS(file = paste0(datafolder, "random_forest_results/",data_name, ".",batch_idx,".RDS" ))
  results[[batch_ids[batch_idx]]] <- tmp
}

saveRDS(results, file = paste0(datafolder, "collect_random_forest_results/",data_name,"_results.RDS"))
