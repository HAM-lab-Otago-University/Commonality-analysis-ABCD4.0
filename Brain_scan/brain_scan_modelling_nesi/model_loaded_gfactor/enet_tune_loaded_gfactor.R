
## library path
.libPaths(new='/nesi/project/uoo03493/Rpackages')
#for parallel processing

library(tidymodels)
library(tidyverse)
library("sva")
library(doParallel)


registerDoParallel(strtoi(Sys.getenv('SLURM_CPUS_PER_TASK')))


datafolder <- "/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/"
#datafolder <- "/media/Data/Yue script/stacking_gfactor_modelling/"

source(paste0(datafolder,"r_functions.R"))

args <- commandArgs(trailingOnly = TRUE)
dataset <- args[1]
resp_data_set <- "gfactor_scale_seperate.RData"

#dataset <- "negfacevsneutface_ROI_.RDS"

#print(dataset)

batch_idx <- strtoi(Sys.getenv('SLURM_ARRAY_TASK_ID'))
#batch_idx <- 1

#print(batch_idx)

# number of threads to use for parallel xgboost
# passed into set_engine()
#threads <- strtoi(Sys.getenv("SLURM_CPU_PER_TASK"))

print("Setup complete!")
message("Setup complete!")

data_input <- readRDS(file = paste0(datafolder, "data/", dataset))
resp_input <- readRDS(file = paste0(datafolder, "gfactor/", resp_data_set))

site_vec <- names(resp_input)

site_input <- site_vec[batch_idx]

### comput and process gfactor

gfactor_list <- resp_input[[site_input]]
data_split <- split_func(data_input = data_input, site = site_input)


subj_info <-  c("SUBJECTKEY","EVENTNAME","SITE_ID_L")   

features <- data_input %>% select(-all_of(subj_info))%>% colnames()

### processing features

features_data_list <- data_processing_cross_sites_seperate(split_input = data_split)

train_baseline_features <-filter(features_data_list,EVENTNAME=="baseline_year_1_arm_1"& fold =="train")

test_baseline_features <-filter(features_data_list,EVENTNAME=="baseline_year_1_arm_1"& fold =="test")

train_followup_features <- filter(features_data_list,EVENTNAME=="2_year_follow_up_y_arm_1"& fold =="train")

test_followup_features <- filter(features_data_list,EVENTNAME=="2_year_follow_up_y_arm_1"& fold =="test")

train_baseline_data <- left_join(gfactor_list[["output_train_baseline"]],train_baseline_features, by = "SUBJECTKEY")%>% drop_na()

test_baseline_data <- left_join(gfactor_list[["output_test_baseline"]],test_baseline_features, by = "SUBJECTKEY")%>% drop_na()

train_followup_data <- left_join(gfactor_list[["output_train_followup"]],train_followup_features, by = "SUBJECTKEY")%>% drop_na()

test_followup_data <- left_join(gfactor_list[["output_test_followup"]],test_followup_features, by = "SUBJECTKEY")%>% drop_na()


train_baseline_select <-select(train_baseline_data,"gfactor",all_of(features))

test_baseline_select <- select(test_baseline_data,"gfactor",all_of(features))

train_followup_select <- select(train_followup_data,"gfactor",all_of(features))

test_followup_select <- select(test_followup_data,"gfactor",all_of(features))

### enent model fitting

baseline_recipe_scale_seperate <-recipe_prep(train_input=train_baseline_select) 

enet_fit_baseline_scale_seperate <-  enet_tuning(recipe_input = baseline_recipe_scale_seperate) 

enet_fit_wf_scale_seperate <- enet_fit_baseline_scale_seperate[["enet_wf_final"]]


enet_pred_baseline_scale_seperate <- model_final_fit(recipe_input = baseline_recipe_scale_seperate, 
                                                           wf_input = enet_fit_wf_scale_seperate,
                                                           test_data = test_baseline_select)


enet_pred_baseline_scale_seperate_train <- model_final_fit(recipe_input = baseline_recipe_scale_seperate, 
                                                           wf_input = enet_fit_wf_scale_seperate,
                                                           test_data = train_baseline_select)

followup_recipe_scale_seperate <-recipe_prep(train_input=test_baseline_select)

enet_fit_followup_scale_seperate <- enet_tuning(recipe_input = followup_recipe_scale_seperate) 

enet_fit_wf_followup_scale_seperate <- enet_fit_followup_scale_seperate[["enet_wf_final"]]


enet_pred_followup_scale_seperate <- model_final_fit(recipe_input = followup_recipe_scale_seperate, 
                                                           wf_input = enet_fit_wf_followup_scale_seperate,
                                                           test_data = test_followup_select) 


enet_pred_followup_scale_seperate_train <- model_final_fit(recipe_input = followup_recipe_scale_seperate, 
                                                           wf_input = enet_fit_wf_followup_scale_seperate,
                                                           test_data = train_followup_select)

output_list <- list(baseline_enet_fit = enet_fit_baseline_scale_seperate,
                    baseline_test_pred = enet_pred_baseline_scale_seperate,
                    baseline_train_pred = enet_pred_baseline_scale_seperate_train,
                    followup_enet_fit = enet_fit_followup_scale_seperate,
                    followup_test_pred = enet_pred_followup_scale_seperate,
                   followup_train_pred = enet_pred_followup_scale_seperate_train,
                   train_baseline_data=train_baseline_data,
                   test_baseline_data=test_baseline_data,
                   train_followup_data=train_followup_data,
                   test_followup_data=test_followup_data)

data_name <- gsub(dataset, pattern = "\\.RDS", replacement = "" )

saveRDS(output_list, paste0(datafolder, "enet_results_loaded_gfactor/" ,data_name, ".",batch_idx,".RDS" ))
