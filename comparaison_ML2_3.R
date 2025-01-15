

# grande dimension : p grand
# données massives : n grand


  # problème, bases de données et objectifs

  
  library(glmnet)
  library(rpart)
  library(randomForest)
  library(caret)
  library(tidyverse)
  library(gbm)
  library(xgboost)
  library(rpart.plot)
  library(bestglm)
  library(fastcluster)
  library(dbscan)
  library(fpc) # ça fait aussi du dbscan
  library(mclust)
  library(FactoMineR)
  library(magrittr)
  
  library(tidymodels)

  library(arrow)  

  dataini <- read_parquet("C:/Users/cepe-s4-09/Desktop/ravary_2025-01-13/readmission_avc.parquet")
  
  
  dataini

  
  colSums(is.na(dataini))  
  
  dataset <- dataini %>%
    filter(!is.na(id_D)) %>%
    mutate(target = as.factor(if_else(id_D == "", 0, 1))) %>% 
    mutate_at(c("modeEntree", "modeSortie", "sexe"), as.character) %>%
    mutate(nbda = if_else(is.na(nbda), 0, nbda)) %>%
    filter(modeSortie != 9) %>%
    select(-c(id, id_D))
  
  
  
  dataini %>% 
    group_by(nbda) %>%
    summarise(b = n())
  
  
  # Data Split
  
  dataset_split <- initial_split(dataset, prop = 0.9, strata = target)

  set.seed(42)
  
  train_val_set <- training(dataset_split)
  
  test_set = testing(dataset_split)

  train_val_split <- initial_split(train_val_set, prop = .8, strata = target)
  
  train_set = training (train_val_split)
  
  val_set = testing(train_val_split)
  
  cat(dim(train_set), dim(val_set), dim(test_set))
  
  # 2. Features Engineering  : Recipes library
  
  rec_basic = recipe(data=train_set, target ~ .)

  prep(rec_basic)  

  rec_basic <- recipe(data= train_set, target~.) %>%
    step_impute_mode(sexe) %>%
    step_impute_mean(age) %>%
    step_normalize(age) %>%
    step_dummy(modeEntree) %>%
    step_other(dp, threshold = 0.03) %>%
    step_dummy(dp)
  
  juice(prep(rec_basic))

  # Version générale
  
  rec_basic <- recipe(data = train_set, target ~ .) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_other(dp, threshold = 0.03) %>%step_other(ghm2, threshold = 0.01) %>%
    step_dummy(all_nominal_predictors())

  juice(prep(rec_basic))  

  rec_interaction <- rec_basic %>% step_interact(age~duree)
  
  rec_spline <- rec_interaction %>% step_ns(age)
  
  # 3. Model avec parsnip
  
  # 3.1 modèle logistique
  
  log_mod <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  log_wf <- workflow() %>%
    add_recipe(rec_basic) %>%
    add_model(log_mod)

  log_fitted <- log_wf %>% fit(train_set)
  
  log_fitted %>% predict(val_set)
  log_fitted %>% predict(val_set, type = "prob")  

  log_pred <- val_set %>% select(target)%>%
    bind_cols(
      log_fitted %>% predict(val_set),
      log_fitted %>% predict(val_set, type =  'prob')
    )
  
  # log_pred
  
  accuracy(log_pred, truth = target, .pred_class)
  
  roc_auc(log_pred, truth = target, .pred_0)
  
  roc_auc(log_pred, truth = target, .pred_1, event_level = "second")

  
  # 3.2 RF wf
  
  library(ranger)

  rf_mod <- rand_forest(
    trees = tune(),
    # mtry = tune(),
    min_n = tune()
  ) %>%
    set_engine("ranger") %>%
    set_mode("classification")

  rf_wf <- workflow() %>%
  add_recipe(rec_basic) %>%
    add_model(rf_mod)

  rf_grid <- expand_grid(
    trees = c(50, 100, 200),
    min_n = c(1, 2, 5)
  )
  
  set.seed(42)
  
  cv_split <- vfold_cv(train_val_set, v = 5, strata = target)

  rf_grid_fitted <- tune_grid(
    rf_wf,
    resamples = cv_split,
    grid = rf_grid,
    metrics = metric_set(accuracy, roc_auc, f_meas)
  )    
  
  names(rf_grid_fitted)
  
  summary(rf_grid_fitted)

  show_best(rf_grid_fitted, metric = "roc_auc")
  
  select_best(rf_grid_fitted, metric = "roc_auc")
  
  collect_metrics(rf_grid_fitted)

  # 3.5 xgboost
  
  library("xgboost")

  gb_mod <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune() )  %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  gb_wf <- workflow() %>%
    add_recipe(rec_basic) %>%
    add_model(gb_mod)

  grid_gb <- expand_grid(
    trees = c(100, 200),
    learn_rate= c(0.01, 0.1, 0.2),
    tree_depth = c(2, 3, 5)
  )  

  gb_grid_fitted <- tune_grid(
    gb_wf,
    grid = grid_gb,
    resamples = cv_split,
    metrics = metric_set(accuracy, roc_auc, f_meas),
    control = control_grid(save_pred =  TRUE)
  )  
  
  # 4 Workflowset
  
  recipes <- list(
    basic = rec_basic,
    inter = rec_interaction,
    spline = rec_spline
  )
  
  models <- list(
    log = log_mod,
    rf = rf_mod,
    gb = gb_mod
  )
  
  wf_set <- workflow_set(
    preproc = recipes,
    models = models
  )
  
  set.seed(42)
  
  wf_set_fitted <- wf_set %>%
    workflow_map(resamples = cv_split,
                 metrics = metric_set(accuracy, roc_auc, f_meas),
                 verbose = TRUE,
                 grid = 20,
                 fn = "tune_grid")
  
  