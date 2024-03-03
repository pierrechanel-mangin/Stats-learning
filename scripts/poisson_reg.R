# Package installation ----
# install.packages(c("tidyverse", "arrow", "tidymodels", "poissonreg"))

# Loading packages ----
library(tidyverse) # data manipulations
library(arrow) # .parquet files
library(tidymodels) # modelling framework
library(glmnet) # regularized regression
library(poissonreg) # tidymodels wrapper for poisson glmnet
library(ranger) # Random forest
library(xgboost) # XGBoost
library(doFuture) # parallel processing

library(embed) # recipe step for sparse pca

# all pred
# pca on geometric data or traffic or both
# poisson, NB, offset (log(traffic10000))

base <- recipe(acc ~., data = inter_train) |> 
  # Converting into date features
  step_date(date, features = c("dow", "month")) |>
  # Imputing missing observations with nearest observations
  step_impute_knn(c(starts_with("date_"), land_use), neighbors = 2, impute_with = imp_vars(x, y)) |> 
  step_rm(date) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, x, y), new_role = "ID") |>
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |> 
  # May differ for boosting
  step_dummy(all_factor_predictors()) |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  update_role(matches(variables$geometry_data), new_role = "geometry_data") |> 
  update_role(matches(variables$traffic_data), new_role = "traffic_data") |> 
  update_role(matches(variables$safety_measures), new_role = "safety_measures")

pca_traff <- 
  base |> 
  step_pca_sparse(
    has_role("traffic_data"),
    predictor_prop = 0.8,
    num_comp = 3,
    prefix = "traffic_PC",
    id = "sparse pca traff"
  )

pca_geom <- 
  base |> 
  step_pca_sparse(
    has_role("geometry_data"),
    predictor_prop = 0.8,
    num_comp = 3,
    prefix = "geom_PC",
    id = "sparse pca geom"
  )

pca_both <- 
  base |> 
  step_pca_sparse(
    has_role("geometry_data"),
    predictor_prop = 0.8,
    num_comp = 3,
    prefix = "geom_PC",
    id = "sparse pca geometry"
  ) |> 
  step_pca_sparse(
    has_role("traffic_data"),
    predictor_prop = 0.8,
    num_comp = 3,
    prefix = "traffic_PC",
    id = "sparse pca traffic"
  )

base_int <- 
  base |> 
  step_interact(terms = ~ has_role("safety_measures"):has_role("traffic_data"))

base_trans <- 
  base |> 
  step_poly(has_role("traffic_data"), degree = tune())

base_both <- 
  base |> 
  step_interact(terms = ~ has_role("safety_measures"):has_role("traffic_data")) |> 
  step_poly(has_role("traffic_data"), degree = tune())

# Models
pois <-
  poisson_reg(
    mode = "regression",
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine(engine = "glmnet")

pois_nb <-
  poisson_reg(
    mode = "regression",
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine(engine = "glmnet", 
             family = MASS::negative.binomial(theta = 2.305))

poisson_set <- 
  workflow_set(list(base = base, interact = base_int, transf = base_trans, base_it = base_both,
                  pca_traff = pca_traff, pca_geom = pca_geom, pca_both = pca_both), 
             list(poisson = pois))

tictoc::tic()
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

poisson_result <- 
  workflow_map(
    poisson_set, 
    resamples = inter_folds, 
    fn = "tune_grid",
    grid = 30, # regular grid
    seed = 123, 
    verbose = TRUE,
    metrics = metric_set(rmse, mae, poisson_log_loss)
  )
tictoc::toc()

# Poisson regression with transformation
test_wk <- workflow(base_trans, pois)
tictoc::tic()
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

test_tune <- tune_grid(
  test_wk,
  inter_folds,
  grid = 30,
  metrics = metric_set(rmse, mae, poisson_log_loss),
  control = control_grid(verbose = TRUE,
                         save_pred = TRUE)
)
tictoc::toc()
final_pois <- 
  finalize_workflow(test_wk, select_best(test_tune, metric = "rmse")) |> 
  last_fit(inter_split)
pred <- 
  final_pois |> 
  extract_workflow() |> 
  predict(intersections_df) |> 
  bind_cols(acc = intersections_df$acc)

# Graph of predicted vs. true values
ggplot(pred, aes(acc, .pred)) + 
  geom_point(alpha = 0.5)+
  geom_abline(slope = 1) +
  theme_minimal()+ 
  ggtitle("Predicted against actual number of accidents") +
  xlab("Truth") + ylab("Prediction")

# Model coefficients
final_pois |> 
  extract_fit_parsnip() |> 
  tidy() 

# saveRDS(test_tune, "./output/poisson_reg_transform")
