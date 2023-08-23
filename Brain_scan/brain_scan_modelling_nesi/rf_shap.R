## library path
.libPaths(new='/nesi/project/uoo03493/Rpackages')
#for parallel processing

library(tidymodels)
library(tidyverse)
library(doParallel)

registerDoParallel(strtoi(Sys.getenv('SLURM_CPUS_PER_TASK')))


datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"
#datafolder <- "/media/Data/Yue script/stacking_gfactor_modelling/"

source(paste0(datafolder,"r_functions.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]

batch_idx <- strtoi(Sys.getenv('SLURM_ARRAY_TASK_ID'))
#batch_idx <- 1

#print(batch_idx)

# number of threads to use for parallel xgboost
# passed into set_engine()
#threads <- strtoi(Sys.getenv("SLURM_CPU_PER_TASK"))

print("Setup complete!")
message("Setup complete!")

list_input_all_site <- readRDS(file = paste0(datafolder, "collect_random_forest_results/", dataset))

list_input <- list_input_all_site[[batch_idx]]


model_fit_input <- list_input[["train_pred"]][["model_final_fit"]]

#model_fit_input

train_data <- list_input[["train_data"]]

#train_data

test_data <- list_input[["test_data"]]

#test_data


subj_info <-  c("SUBJECTKEY","EVENTNAME","SITE_ID_L")   

subj_info_resp <-  c(subj_info,"gfactor")


processed_train_data <-  train_data%>%
                       select(-all_of(subj_info_resp))%>%
                      as.data.frame()


##need to test this works for all the models
model_pred_fun <- function(object, newdata) {
  pred_results <- predict(object, new_data = newdata)
return(pred_results$.pred)
  }  


model_shap <- model_fit_input %>% 
   fastshap::explain(X =processed_train_data,
                     nsim= 1000, 
                     pred_wrapper =model_pred_fun,
                     .parallel=TRUE
                     )


output_list <- list(train_data = train_data,
                    test_data = test_data,
                   model_shap = model_shap)

data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

saveRDS(output_list, paste0(datafolder, "rf_shap_results/" ,data_name, "_shap_",batch_idx,".RDS" ))

