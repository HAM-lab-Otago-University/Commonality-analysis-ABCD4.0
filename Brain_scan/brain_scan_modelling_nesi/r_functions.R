### function to make data splits based on site
split_func  <- function(site,data_input)
{ 
  train_indices <- which(data_input$SITE_ID_L != site)
  test_indices <- which(data_input$SITE_ID_L == site)
  
  indices <-
    list(analysis   = train_indices, 
         assessment = test_indices)
  split <- make_splits(indices, data_input)
  return(split)}
## scale train and test seperately and apply the mean and sd of the baseline  to the followup

scale_train <- function(baseline_data,followup_data){
  ### select features
  baseline_data_tibble <- baseline_data %>% select_if(is.numeric)
  followup_data_tibble <- followup_data %>% select_if(is.numeric)
  
  scaled_baseline_data <- baseline_data_tibble %>% scale()
  
  mean_baseline_data <- baseline_data_tibble %>% summarise(across(everything(), mean))
  sd_baseline_data <- baseline_data_tibble %>% summarise(across(everything(), sd))
  
  # Standardize the test sample
  scaled_followup_data <- followup_data_tibble %>%
    rowwise() %>%
    mutate((across(everything())-mean_baseline_data)/sd_baseline_data) %>%
    ungroup()
  
  output_baseline_data <- baseline_data %>% select_if(is.character)%>%
    bind_cols(scaled_baseline_data)
  output_followup_data <- followup_data %>% select_if(is.character)%>%
    bind_cols(scaled_followup_data)
  return(list(output_baseline_data = output_baseline_data,
              output_followup_data=output_followup_data))
}
## compute and processing gractor
gfactor_cross_sites_seperate <- function(split_input){
  ### select the variables
  
  train_baseline <- training(split_input)%>%
    filter(EVENTNAME=="baseline_year_1_arm_1") %>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()
  
  test_baseline <- testing(split_input)%>%
    filter(EVENTNAME=="baseline_year_1_arm_1") %>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()
  
  train_followup <- training(split_input)%>%
    filter(EVENTNAME=="2_year_follow_up_y_arm_1") %>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()
  
  test_followup <- testing(split_input)%>%
    filter(EVENTNAME=="2_year_follow_up_y_arm_1") %>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na() 
  
  scaled_train_list <- scale_train(baseline_data = train_baseline,
                                   followup_data = train_followup)
  scaled_test_list <- scale_train(baseline_data = test_baseline,
                                  followup_data = test_followup)
  
  
  NeuroCog2ndOrder.Fit <- lavaan::cfa(model = NeuroCog2ndOrder, 
                                      data = scaled_train_list[["output_baseline_data"]],estimator="MLR")
  
  second_order_train_baseline <- lavaan::lavPredict(NeuroCog2ndOrder.Fit, 
                                                    newdata = scaled_train_list[["output_baseline_data"]])
  
  second_order_test_baseline <- lavaan::lavPredict(NeuroCog2ndOrder.Fit, 
                                                   newdata = scaled_test_list[["output_baseline_data"]])
  
  
  second_order_train_followup <- lavaan::lavPredict(NeuroCog2ndOrder.Fit, 
                                                    newdata = scaled_train_list[["output_followup_data"]])
  
  second_order_test_followup <- lavaan::lavPredict(NeuroCog2ndOrder.Fit, 
                                                   newdata = scaled_test_list[["output_followup_data"]])
  
  
  scaled_data_list <- list(output_train_baseline = scaled_train_list[["output_baseline_data"]], 
                           output_test_baseline = scaled_test_list[["output_baseline_data"]],
                           output_train_followup=scaled_train_list[["output_followup_data"]],
                           output_test_followup=scaled_test_list[["output_followup_data"]])
  
  second_order_pred <- list(output_train_baseline = second_order_train_baseline, 
                            output_test_baseline = second_order_test_baseline,
                            output_train_followup=second_order_train_followup,
                            output_test_followup=second_order_test_followup)
  
  ###use this gfactor as response for all the other combat models
  ### response do not needs to be combat
  gfactor_data_list <- map2(.x =scaled_data_list,.y = second_order_pred, ~mutate(.x,gfactor = .y[,4])%>%drop_na())
  
  ### get the gfactor for the final output
  gfactor_list <- gfactor_data_list %>% map(.,~select(.,all_of(c("SUBJECTKEY","gfactor"))))
  
  
  scaled_train_gfactor <- scale_train(baseline_data = gfactor_list[["output_train_baseline"]],
                                      followup_data = gfactor_list[["output_train_followup"]])
  scaled_test_gfactor <- scale_train(baseline_data = gfactor_list[["output_test_baseline"]],
                                     followup_data = gfactor_list[["output_test_followup"]])
  
  
  scaled_gfactor_list <- list(output_train_baseline = scaled_train_gfactor[["output_baseline_data"]], 
                              output_test_baseline = scaled_test_gfactor[["output_baseline_data"]],
                              output_train_followup=scaled_train_gfactor[["output_followup_data"]],
                              output_test_followup=scaled_test_gfactor[["output_followup_data"]])
  
  return(scaled_gfactor_list)
}



IQR_remove_vec <- function(data_split,x){
  outlier_tibble <- data_split%>%
    select(all_of(x))%>%
    mutate_at(vars(all_of(x)), ~ ifelse(
      .x > quantile(.x, na.rm = TRUE)[4] + 3 * IQR(.x, na.rm = TRUE) |
        .x < quantile(.x, na.rm = TRUE)[2] - 3 * IQR(.x, na.rm = TRUE), 
      TRUE, FALSE))
  data_split <- data_split %>% mutate(outlier_indicator = rowMeans(outlier_tibble))
  data_split_outlier <- filter(data_split,outlier_indicator > 0.05) ##threshold to remove outliers
  data_split_select <- data_split %>% filter(! SUBJECTKEY %in% data_split_outlier$SUBJECTKEY)%>%
    select(-outlier_indicator)
  return(data_split_select)
}

batch_adjustOne_vec <- function(data_fold, x){
  ols_data <- select(data_fold,all_of(x))
  ols_matrix <- as.matrix(t(ols_data))
  dimnames(ols_matrix)<- NULL
  data_fold$SITE_ID_L <- as.factor(data_fold$SITE_ID_L)##have to be changed into factor before combat
  data_fold$SITE_ID_L <- droplevels(data_fold$SITE_ID_L)##drop the empty levels of a factor
  ols_matrix_com <- ComBat(dat = ols_matrix,batch = data_fold$SITE_ID_L)
  ols_data_com <- data.frame(t(ols_matrix_com))
  #%>%
  # scale()### scale the observations after combat
  colnames(ols_data_com) <- colnames(ols_data)
  data_resp <- data_fold%>%select(-starts_with(x))
  data_output <- bind_cols(data_resp,ols_data_com)
  
  return(data_output)
}


batch_adjustOne_ref_vec <- function(data_fold, x,test_data_fold){
  ### create a new fold variable and combine train and test together
  data_fold <- data_fold %>% mutate(fold = "train")
  test_data_fold <- test_data_fold %>% mutate(fold = "test")
  data_fold_all <- bind_rows(data_fold,test_data_fold)
  
  ols_data <- select(data_fold_all,all_of(x))
  ols_matrix <- as.matrix(t(ols_data))
  dimnames(ols_matrix)<- NULL
  data_fold_all$fold <- as.factor(data_fold_all$fold)##have to be changed into factor before combat
  data_fold_all$fold <- droplevels(data_fold_all$fold)##drop the empty levels of a factor
  
  ols_matrix_com <- ComBat(dat = ols_matrix,batch = data_fold_all$fold, ref.batch = "train")
  ols_data_com <- data.frame(t(ols_matrix_com))
  #%>%
  # scale()### scale the observations after combat
  colnames(ols_data_com) <- colnames(ols_data)
  data_resp <- data_fold_all%>%select(-starts_with(x))
  data_output <- bind_cols(data_resp,ols_data_com)
  
  data_output_test <- filter(data_output, fold == "test")%>%
    select(-fold)
  
  return(data_output_test)
}



### only processing the features, gfactor, the respone, is computed in the other function
data_processing_cross_sites_seperate <- function(split_input){
  ### select the variables
  
  train_baseline <- training(split_input)%>%
    filter(EVENTNAME=="baseline_year_1_arm_1") %>%
    select(all_of(features),all_of(subj_info))%>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()%>%
    IQR_remove_vec(x = features)
  
  test_baseline <- testing(split_input)%>%
    filter(EVENTNAME=="baseline_year_1_arm_1") %>%
    select(all_of(features),all_of(subj_info))%>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()%>%
    IQR_remove_vec(x = features)
 
  train_followup <- training(split_input)%>%
    filter(EVENTNAME=="2_year_follow_up_y_arm_1") %>%
    select(all_of(features),all_of(subj_info))%>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()%>%
    IQR_remove_vec(x = features)
  
  test_followup <- testing(split_input)%>%
    filter(EVENTNAME=="2_year_follow_up_y_arm_1") %>%
    select(all_of(features),all_of(subj_info))%>%
    distinct(SUBJECTKEY, .keep_all = TRUE)%>% 
    drop_na()%>%
    IQR_remove_vec(x = features)
  
  scaled_train_list <- scale_train(baseline_data = train_baseline,
                                   followup_data = train_followup)
  scaled_test_list <- scale_train(baseline_data = test_baseline,
                                  followup_data = test_followup)
  
  
  scaled_data_list <- list(output_train_baseline = scaled_train_list[["output_baseline_data"]], 
                           output_test_baseline = scaled_test_list[["output_baseline_data"]],
                           output_train_followup=scaled_train_list[["output_followup_data"]],
                           output_test_followup=scaled_test_list[["output_followup_data"]])
  
  
  ### scale and combat features
  
  
  ### scale and combat
  ## only select features and subject ID
  
  scaled_data_list_select <-scaled_data_list%>% map(.,~                                                     
                                                      select(.,all_of(c("SUBJECTKEY","SITE_ID_L")),all_of(features))) 
  
  train_baseline_combat_train <- batch_adjustOne_vec(data_fold = scaled_data_list_select[["output_train_baseline"]],
                                                     x = features)
  
  
  train_followup_combat_train <- batch_adjustOne_vec(data_fold = scaled_data_list_select[["output_train_followup"]],
                                                     x = features)
  
  
  scaled_train_list_combat_train <- scale_train(baseline_data = train_baseline_combat_train,
                                                followup_data = train_followup_combat_train)
  scaled_test_list_combat_train <- scale_train(baseline_data = scaled_data_list_select[["output_test_baseline"]],
                                               followup_data = scaled_data_list_select[["output_test_followup"]])
  
  
  scaled_data_list_combat_train <- list(output_train_baseline = scaled_train_list_combat_train[["output_baseline_data"]], 
                                        output_test_baseline = scaled_test_list_combat_train[["output_baseline_data"]],
                                        output_train_followup=scaled_train_list_combat_train[["output_followup_data"]],
                                        output_test_followup=scaled_test_list_combat_train[["output_followup_data"]])
  
  
  test_baseline_combat_ref <- batch_adjustOne_ref_vec(data_fold = scaled_data_list_combat_train[["output_train_baseline"]],
                                                      x = features,
                                                      test_data_fold=scaled_data_list_select[["output_test_baseline"]])
  
  
  test_followup_combat_ref <- batch_adjustOne_ref_vec(data_fold = scaled_data_list_select[["output_train_followup"]],
                                                      x = features,
                                                      test_data_fold = scaled_data_list_select[["output_test_followup"]])
  
  
  scaled_data_list_combat_ref <- list(train_baseline = train_baseline_combat_train,
                                      test_baseline = test_baseline_combat_ref,
                                      train_followup = train_followup_combat_train,
                                      test_followup = test_followup_combat_ref)
  
  
  ### join the data with variables
  scaled_data_list_combat_ref_all_event <- map2(.x =scaled_data_list_combat_ref,.y=scaled_data_list,
                                                ~mutate(.x,EVENTNAME= .y[["EVENTNAME"]])%>%drop_na())
  
  
  output_data_train_baseline <- scaled_data_list_combat_ref_all_event[["train_baseline"]]%>% 
    mutate(fold = "train")
  
  output_data_test_baseline <- scaled_data_list_combat_ref_all_event[["test_baseline"]]%>% 
    mutate(fold = "test")
  
  output_data_train_followup <- scaled_data_list_combat_ref_all_event[["train_followup"]]%>% 
    mutate(fold = "train")
  
  output_data_test_followup <- scaled_data_list_combat_ref_all_event[["test_followup"]]%>% 
    mutate(fold = "test")
  
  output_data <- bind_rows(output_data_train_baseline,output_data_test_baseline,output_data_train_followup,output_data_test_followup)
  return(output_data)
}

recipe_prep <- function(train_input=train_gfactor_scan_enet){
  norm_recipe <- recipe( as.formula("gfactor~."), data = train_input) %>%
    update_role(all_of(features), new_role = "predictor")%>%
    update_role("gfactor", new_role = "outcome" )%>%
    step_dummy(all_nominal()) %>%
    prep(training = train_input, retain = TRUE)
  return(norm_recipe)
}


### fit the elastic net model 
enet_tuning <- function(recipe_input){
  set.seed(123) 
  train_input <- recipe_input %>% bake(new_data=NULL)
  tuning_cv_folds <- train_input  %>%
    vfold_cv(v = 10)
  
  ## mtry is the number of predictors to sample at each split
  ## min_n (the number of observations needed to keep splitting nodes)
  model_tune <-linear_reg(penalty =tune(),  
                          mixture = tune()) %>%
    set_mode("regression") %>%
    set_engine("glmnet")
  
  tune_wf <- workflow() %>%
    add_recipe(recipe_input) %>%
    add_model(model_tune)
  
  ## automate generate grid for hyperparameters
  
  model_grid <- 
    model_tune %>% 
    extract_parameter_set_dials(tune_wf)%>% 
    ## update the values of the parameters
    update(penalty =penalty(range = c(-10,1), trans = log10_trans()))%>%
    update(mixture =mixture(range = c(0,1)))%>%
    grid_regular(levels = c(200,11))
  
  tune_ctrl <- control_grid(save_pred = TRUE, verbose = TRUE
                            ,parallel_over = "everything"
  )
  
  
  #start <- Sys.time()
  tune_res <- tune_grid(
    tune_wf,
    resamples = tuning_cv_folds,
    metrics = metric_set(rmse),
    grid = model_grid,
    control= tune_ctrl
  )
  
  best_tune <- select_best(tune_res, 
                           metric = "rmse")
  
  best_tuned_param <- show_best(tune_res, 
                                metric="rmse")
  
  enet_final_wf <- tune_wf %>% finalize_workflow(best_tune)
  return(list(enet_wf_final = enet_final_wf, 
              best_enet_model = best_tune,
              best_enet_forest_param = best_tuned_param))
}


### predict the enet model
model_final_fit <- function(recipe_input,
                           wf_input,
                           test_data){
  train_input <- recipe_input %>% 
    bake(new_data=NULL)
  
  ##baked recipe scale the test data with the mean and sd in the training data
  test_input <-  bake( recipe_input,
                       new_data=test_data)
  
  model_final_fit <- 
    wf_input%>%
    parsnip::extract_spec_parsnip()%>%
    parsnip::fit(data = train_input, formula= as.formula("gfactor~."))
  
  model_predict <- predict(model_final_fit, 
                           new_data = test_input %>% 
                             drop_na() ) %>%
    rename(model_predict = .pred) %>% 
    bind_cols(test_input%>% drop_na())  
  
  ##processing output
  
  output_list <- vector("list",length=2)
  names(output_list) <- c(paste0("model","_final_fit"),
                          paste0("model","_predict"))
  
  output_list[[paste0("model","_final_fit")]] <- model_final_fit
  output_list[[paste0("model","_predict")]] <- model_predict
  return(output_list)
}
### random forest tuning

random_forest_tuning <- function(recipe_input){
  
  set.seed(123) 
  train_input <- recipe_input %>% bake(new_data=NULL)
  tuning_cv_folds <- train_input  %>%
    vfold_cv(v = 10)
  
  ## mtry is the number of predictors to sample at each split
  ## min_n (the number of observations needed to keep splitting nodes)
  model_tune <-rand_forest(mtry = tune(),
                           trees = 500,
                           min_n = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  tune_wf <- workflow() %>%
    add_recipe(recipe_input) %>%
    add_model(model_tune)
  

  ## automate generate grid for hyperparameters
  
  model_grid <- 
    model_tune %>% 
    extract_parameter_set_dials(tune_wf)%>% 
    ## update the values of the parameters
    update(min_n =min_n(range = c(2,2000)))%>%
    update(mtry =mtry(range = c(1, 90)))%>%
    grid_latin_hypercube(size = 3000)
  
  
  tune_ctrl <- control_grid(save_pred = TRUE, verbose = TRUE
                            ,parallel_over = "everything"
  )
  #library(doFuture)
  #registerDoFuture()
  #plan(multisession(workers = 30))
  start <- Sys.time()
  
  #library(doParallel)
  #registerDoParallel(strtoi(Sys.getenv('SLURM_CPUS_PER_TASK')))    
  
  #doParallel::registerDoParallel(cores = 20)
  
  tune_res <- tune_grid(
    tune_wf,
    resamples = tuning_cv_folds,
    metrics = metric_set(rmse),
    grid = model_grid,
    control= tune_ctrl
  )
  end <- Sys.time()
  
  ##check how much of the grid it went through
  #tune_res %>% 
  #  collect_metrics()%>%
  #    print()
  
  
  
  best_tune <- select_best(tune_res, 
                           metric = "rmse")
  
  best_tuned_param <- show_best(tune_res, 
                                metric="rmse")
  
  rf_final_wf <- tune_wf %>% finalize_workflow(best_tune)
  
    return(list(rf_wf_final = rf_final_wf, 
              best_rf_model = best_tune,
              best_rf_param = best_tuned_param))
}

metric_compute_site <- function(data_input,site_input){
   cor_model <- cor(data_input$model_predict,
                     data_input$gfactor,
                     use = "pairwise.complete.obs")
  
  tradrsq_model <- yardstick::rsq_trad(data=data_input, 
                                       truth=.data$gfactor, 
                                       estimate=.data$model_predict)
  
  mae_model <- yardstick::mae(data=data_input, 
                                       truth=.data$gfactor, 
                                       estimate=.data$model_predict)
  
  rmse_model <- yardstick::rmse(data=data_input, 
                                       truth=.data$gfactor, 
                                       estimate=.data$model_predict)
return(tibble(correlation=cor_model,  tradrsq= tradrsq_model$.estimate ,MAE= mae_model$.estimate, RMSE=rmse_model$.estimate,
              site = unique(site_input$SITE_ID_L)))
 } 