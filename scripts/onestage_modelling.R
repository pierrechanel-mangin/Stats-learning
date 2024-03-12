# Loading packages needed and dataset
source("./scripts/modelling_setup.R")

# Data split ----
set.seed(456) # Reproducibility
inter_split <- initial_split(intersections_df, prop = 3/4)
inter_train <- training(inter_split)
inter_test <- testing(inter_split)
# CV: 10-folds, 5-repeats
inter_folds <- vfold_cv(inter_train, v = 10, repeats = 5) 

# Data preprocessing steps ----
base <- 
  recipe(acc ~., data = inter_train) |> 
  step_rm(acc_bin) |> 
  # Imputing missing observations with nearest observations
  step_impute_knn(land_use, neighbors = 3, impute_with = imp_vars(x, y)) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, x, y), new_role = "ID") |> 
  add_role(contains(variables$geometry_data), new_role = "geometry_data") |> 
  add_role(contains(variables$traffic_data), new_role = "traffic_data") |>
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) 

## Poisson regression recipes
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

## Boosting recipe
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

# Model spec ----
## Logistic Regression spec
pois_reg <-
  poisson_reg(penalty = tune(), mixture = tune()) %>% 
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
  set_mode("regression")

## XGBoost spec
boost <-
  boost_tree(
    mtry = tune(), trees = tune(), 
    tree_depth = tune(), learn_rate = tune(),
    min_n = 5L, loss_reduction = 0.0, sample_size = 1.0, stop_iter = 20L
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# Model estimations
## Workflow set
model_sets <- 
  bind_rows(workflow_set(list(baselm = lm_rec, advlm = lm_it_rec), list(glmnet = pois_reg)),
            workflow_set(list(base_rf = randf_rec), list(ranger = rf)),
            workflow_set(list(boost = boost_rec), list(xgb = boost)))

## Parallel processing
# tictoc::tic()
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoFuture()
# cl <- parallel::makeCluster(all_cores)
# plan(cluster, workers = cl)

# Workflow map ----
# model_result <- 
#   workflow_map(
#     model_sets, 
#     resamples = inter_folds, 
#     fn = "tune_grid",
#     grid = 30, # regular grid
#     seed = 123, 
#     verbose = TRUE,
#     metrics = metric_set(rmse, mae, poisson_log_loss)
#   )
# 
# plan(sequential) # Explicitly close multisession workers
# 
# tictoc::toc()
# saveRDS(model_result, "./output/onestage_result")


onestage_result <- readRDS("./output/onestage_result")

# -------
onestage_rf <- 
  extract_workflow(model_sets, id = "base_rf_ranger") |> 
  finalize_workflow(extract_workflow_set_result(onestage_result, id = "base_rf_ranger") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", trees)) |>
  
  last_fit(inter_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)
onestage_boost <- 
  extract_workflow(model_sets, id = "boost_xgb") |> 
  finalize_workflow(extract_workflow_set_result(onestage_result, id = "boost_xgb") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", tree_depth)) |> 
  last_fit(inter_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)
onestage_glm <- 
  extract_workflow(model_sets, id = "baselm_glmnet") |>
  finalize_workflow(extract_workflow_set_result(onestage_result, id = "baselm_glmnet") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", desc(penalty))) |> 
  last_fit(inter_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)


allmodels_pred_onestage <- 
  map(list(onestage_glm, onestage_rf, onestage_boost), \(object)
      extract_workflow(object) |> 
        predict(new_data = intersections_df) |> 
        mutate(int_no = intersections_df$int_no,
               acc = intersections_df$acc) |> 
        select(int_no, acc, .pred)) |> 
  set_names(c("Poisson Regression", "Random Forest",  "XGBoost")) |> 
  list_rbind(names_to = "model_type")|> 
  mutate(testing_data = if_else(int_no %in% inter_test$int_no, "Testing", "Training"))

 
  ggplot(allmodels_pred_onestage, aes(acc, .pred, color = testing_data)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) +
  geom_smooth(method = "lm", formula = "y~x", se = TRUE) +
  geom_abline(slope = 1, linetype = 2) +
  facet_wrap(~ model_type) +
  theme_minimal() + 
  ggtitle("Predicted against actual number of accidents (two-stage)") +
  xlab("Truth") + ylab("Prediction") + labs(colour="Data split")




