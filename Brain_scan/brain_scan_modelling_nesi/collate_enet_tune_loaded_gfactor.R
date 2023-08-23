## library path
.libPaths(new='/nesi/project/uoo03493/Rpackages')
#for parallel processing

library(tidyverse)


args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"

data_input <- readRDS(file = paste0(datafolder, "/data/", dataset))

batch_ids <- unique(data_input$SITE_ID_L)

results <- list()

for(batch_idx in 1:length(batch_ids)){
tmp <- readRDS(file = paste0(datafolder, "enet_results_loaded_gfactor/",data_name, ".",batch_idx,".RDS" ))
  results[[batch_ids[batch_idx]]] <- tmp
}

### make sure the names of the list and the real list are the same

test_input <- purrr::map(results,"test_followup_data")
site_tibble <- purrr::map(test_input,~select(.x,SITE_ID_L)%>% unique())%>% do.call(rbind,.)

names(results)<- site_tibble$SITE_ID_L

saveRDS(results, file = paste0(datafolder, "collect_enet_results_loaded_gfactor/",data_name,".enet_results.RDS"))
