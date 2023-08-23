
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

list_input <- readRDS(file = paste0(datafolder, "random_forest_data/", dataset))

train_list <- list_input[["train_list"]]
test_list <- list_input[["test_list"]]


train_input <- train_list[[batch_idx]]%>% drop_na()

test_input <- test_list[[batch_idx]]%>% drop_na()


subj_info <-  c("SUBJECTKEY","EVENTNAME","SITE_ID_L")   

subj_info_resp <-  c(subj_info,"gfactor")

features <- train_input %>% select(-all_of(subj_info_resp))%>% colnames()


train_select <-select(train_input,-all_of(subj_info))

test_select <- select(test_input,-all_of(subj_info))

recipe_scale_seperate <-recipe_prep(train_input=train_select) 

random_forest_fit_scale_seperate <-  random_forest_tuning(recipe_input = recipe_scale_seperate) 

random_forest_fit_wf_scale_seperate <- random_forest_fit_scale_seperate[["rf_wf_final"]]


rf_pred_scale_seperate <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                                           wf_input = random_forest_fit_wf_scale_seperate,
                                                           test_data = test_select)


rf_pred_scale_seperate_train <- model_final_fit(recipe_input = recipe_scale_seperate, 
                                                           wf_input = random_forest_fit_wf_scale_seperate,
                                                           test_data = train_select)

output_list <- list(random_forest_fit = random_forest_fit_scale_seperate,
                    test_pred = rf_pred_scale_seperate,
                    train_pred = rf_pred_scale_seperate_train,
                    train_data = train_input,
                   test_data = test_input)

data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

saveRDS(output_list, paste0(datafolder, "random_forest_results/" ,data_name, ".",batch_idx,".RDS" ))

