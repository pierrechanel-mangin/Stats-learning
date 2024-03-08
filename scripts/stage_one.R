# Data split ----
set.seed(234) # Reproducibility
stage1_split <- initial_split(intersections_df, prop = 3/4, strata = acc_bin)
stage1_train <- training(stage1_split)
stage1_test <- testing(stage1_split)
# CV: 10-folds, 5-repeats
stage1_folds <- vfold_cv(stage1_train, v = 10, repeats = 5, strata = acc_bin) 

# Data preprocessing (recipes) ----
## Foundational recipe to be used for others
base <- 
  recipe(acc_bin ~., data = stage1_train) |> 
  step_rm(acc) |> 
  # Imputing missing observations with nearest observations
  step_impute_knn(land_use, neighbors = 3, impute_with = imp_vars(x, y)) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, x, y), new_role = "ID") |> 
  add_role(contains(variables$geometry_data), new_role = "geometry_data") |> 
  add_role(contains(variables$traffic_data), new_role = "traffic_data") |>
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) 

## Logistic regression recipes
lm_rec <- 
  base |> 
  # May differ for boosting
  step_dummy(all_factor_predictors(), one_hot = FALSE) |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") 
# Adding both Interactions and Transformations (it)
lm_it_rec <- 
  lm_rec |> 
  step_interact(terms = ~ matches("_X"):has_role("traffic_data")) |> 
  step_poly(has_role("traffic_data"), degree = 2)

## boost recipe
boost_rec <- 
  base |> 
  step_dummy(all_factor_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  step_poly(has_role("traffic_data"), degree = 2)

## Random Forest recipe
randf_rec <- 
  base |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  step_poly(has_role("traffic_data"), degree = 2)

# Model Specification ----

## Logistic Regression spec
log_reg <-
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

## Random Forest spec
rf <-
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = 5L
  ) %>% 
  set_engine("ranger",  
             importance = "impurity") %>% 
  set_mode("classification")

## XGBoost spec
boost <-
  boost_tree(
    mtry = tune(), trees = tune(), 
    tree_depth = tune(), learn_rate = tune(),
    min_n = 5L, loss_reduction = 0.0, sample_size = 1.0, stop_iter = 20L
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Workflow set
stage1_sets <- 
  bind_rows(workflow_set(list(baselm = lm_rec, advlm = lm_it_rec), list(glmnet = log_reg)),
            workflow_set(list(base_rf = randf_rec), list(ranger = rf)),
            workflow_set(list(boost = boost_rec), list(xgb = boost)))

## Parallel processing
tictoc::tic()
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

# Workflow map
stage1_result <- 
  workflow_map(
    stage1_sets, 
    resamples = stage1_folds, 
    fn = "tune_grid",
    grid = 30, # regular grid
    seed = 123, 
    verbose = TRUE,
    metrics = metric_set(accuracy, roc_auc, recall, precision, f_meas)
  )

plan(sequential) # Explicitly close multisession workers

tictoc::toc()

# Export model results
#saveRDS(stage1_result, "./output/stage1_result")

# Finalize best model ----
stage1_result <- readRDS("./output/stage1_result")

best_highprec <- 
  extract_workflow_set_result(stage1_result, id = "boost_xgb") |> 
  # One std error approach. tree_depth as complexity measure
  select_by_one_std_err(metric = "roc_auc", tree_depth)

stage1_finalft <- 
  extract_workflow(stage1_sets, id = "boost_xgb") |> 
  finalize_workflow(best_highprec) |> 
  last_fit(stage1_split, 
           metrics = metric_set(accuracy, roc_auc, sensitivity, specificity, recall, precision), 
           add_validation_set = TRUE)

# Stage 1 predictions ----
stage1_pred <-  
  stage1_finalft |> 
  extract_workflow() |> 
  predict(new_data = intersections_df, type = "prob") |> 
  mutate(
    int_no = intersections_df$int_no,
    .pred_class = if_else(.pred_0 >= 0.5, factor("0"), factor("1"))
  ) |> 
  select(int_no, .pred_0, .pred_1, .pred_class)

write_parquet(stage1_pred, "./output/stage1_pred.parquet")
