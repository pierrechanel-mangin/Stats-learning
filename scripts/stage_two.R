# Loading packages needed and dataset (common to both stages)
source("./scripts/modelling_setup.R")

# Stage two dataset
stage1_pred <- read_parquet("./output/stage1_pred.parquet")
stage2_df <- 
  intersections_df |> 
  left_join(select(stage1_pred, int_no, pred_class), by = "int_no", 
            relationship = "one-to-one")|> 
  filter(pred_class=="1") |> 
  select(-pred_class)

# Data split ----
set.seed(123) # Reproducibility
stage2_split <- initial_split(stage2_df, prop = 3/4)
stage2_train <- training(stage2_split)
stage2_test <- testing(stage2_split)
# CV: 10-folds, 5-repeats
stage2_folds <- vfold_cv(stage2_train, v = 10, repeats = 5) 

base <- 
  recipe(acc ~., data = stage2_train) |> 
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
# transform only
lm_trans_rec <- 
  lm_rec |> 
  step_poly(has_role("traffic_data"), degree = 2)

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

# Workflow set
stage2_sets <- 
  bind_rows(workflow_set(list(baselm = lm_rec, translm = lm_trans_rec, advlm = lm_it_rec), list(glmnet = pois_reg)),
            workflow_set(list(base_rf = randf_rec), list(ranger = rf)),
            workflow_set(list(boost = boost_rec), list(xgb = boost)))

## Parallel processing
# tictoc::tic()
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoFuture()
# cl <- parallel::makeCluster(all_cores)
# plan(cluster, workers = cl)

# Workflow map ----
# stage2_result <- 
#   workflow_map(
#     stage2_sets, 
#     resamples = stage2_folds, 
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
#saveRDS(stage2_result, "./output/stage2_result")

# Results ----
stage2_result <- readRDS("./output/stage2_result")
autoplot(stage2_result, select_best = TRUE, rank_metric = "rmse", metric = c("rmse", "mae"))


best_lowglmnet <- 
  extract_workflow_set_result(stage2_result, id = "translm_glmnet") |> 
  # One std error approach. tree_depth as complexity measure
  select_by_one_std_err(metric = "rmse", desc(penalty))

stage2_finalft <- 
  extract_workflow(stage2_sets, id = "translm_glmnet") |> 
  finalize_workflow(best_lowglmnet) |> 
  last_fit(stage2_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)

# Stage 2 predictions ----
stage2_pred <-  
  stage2_finalft |> 
  extract_workflow() |> 
  predict(new_data = stage2_df) |> 
  mutate(int_no = stage2_df$int_no) |> 
  select(int_no, .pred)

# write_parquet(stage2_pred, "./output/stage2_pred.parquet")


# Extra  (ignore) ------------
stage2_rf <- 
  extract_workflow(stage2_sets, id = "base_rf_ranger") |> 
  update_model(rf) |> # forgot to initially add the importance argument
  finalize_workflow(extract_workflow_set_result(stage2_result, id = "base_rf_ranger") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", trees)) |> 
  last_fit(stage2_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)
stage2_boost <- 
  extract_workflow(stage2_sets, id = "boost_xgb") |> 
  finalize_workflow(extract_workflow_set_result(stage2_result, id = "boost_xgb") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", tree_depth)) |> 
  last_fit(stage2_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)

stage2_glm <- 
  extract_workflow(stage2_sets, id = "baselm_glmnet") |>
  finalize_workflow(extract_workflow_set_result(stage2_result, id = "baselm_glmnet") |> 
                      # One std error approach. tree_depth as complexity measure
                      select_by_one_std_err(metric = "rmse", desc(penalty))) |> 
  last_fit(stage2_split, 
           metrics = metric_set(rmse, mae, poisson_log_loss), 
           add_validation_set = TRUE)


allmodels_pred_stage2 <- 
  map(list(stage2_finalft, stage2_rf, stage2_boost), \(object)
      extract_workflow(object) |> 
        predict(new_data = stage2_df) |> 
        mutate(int_no = stage2_df$int_no,
               acc = stage2_df$acc) |> 
        select(int_no, acc, .pred)) |> 
  set_names(c("Poisson Regression", "Random Forest",  "XGBoost")) |> 
  list_rbind(names_to = "model_type") |> 
  mutate(testing_data = if_else(int_no %in% stage2_test$int_no, "Testing", "Training"))


  ggplot(allmodels_pred_stage2, aes(acc, .pred, color = testing_data))+
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2))+
  geom_abline(slope = 1, linetype = 2)+
  facet_wrap(~model_type)+
  theme_minimal() + 
  ggtitle("Predicted against actual number of accidents (two-stage)") +
  xlab("Truth") + ylab("Prediction") + labs(colour="Data split")

  
  library(vip)
  coef_stage2 <-
    stage2_finalft |>
    extract_workflow() |>
    tidy() |>
    filter(term != "(Intercept)") |>
    mutate(selected = if_else(estimate == 0, "No", "Yes")) |>
    select(term, selected)
  p2 <- stage2_finalft |> 
    extract_fit_parsnip() |> 
    vi() |> 
    filter(Importance != 0) |> 
    ggplot(aes(Variable, Importance, color = Sign)) +
    geom_point()+
    coord_flip() + 
    theme_minimal() +
    ggtitle("Variables selected in Stage 2 (coefficient size and sign)")