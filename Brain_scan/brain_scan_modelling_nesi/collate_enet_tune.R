args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"

data_input <- readRDS(file = paste0(datafolder, "/data/", dataset))

batch_ids <- unique(data_input$SITE_ID_L)

results <- list()

for(batch_idx in 1:length(batch_ids)){
tmp <- readRDS(file = paste0(datafolder, "enet_results/",data_name, ".",batch_idx,".RDS" ))
  results[[batch_ids[batch_idx]]] <- tmp
}

saveRDS(results, file = paste0(datafolder, "collected_enet_results/",data_name,".enet_results.RDS"))
