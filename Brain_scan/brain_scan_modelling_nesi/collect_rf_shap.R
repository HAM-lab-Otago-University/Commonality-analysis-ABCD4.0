## library path
.libPaths(new='/nesi/project/uoo03493/Rpackages')
#for parallel processing

library(tidyverse)


args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"

results <- list()

for(batch_idx in 1:length(batch_ids)){
tmp <- readRDS(file = paste0(datafolder, "rf_shap_results/",data_name, "_shap_",batch_idx,".RDS" ))
site_name <- tmp[["test_data"]][["SITE_ID_L"]]%>% unique()
results[[site_name]]] <- tmp
}

saveRDS(results, file = paste0(datafolder, "collect_rf_shap_results/",data_name,"_shap_results.RDS"))
