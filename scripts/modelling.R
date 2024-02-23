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


# Data split ----
landuse_df <- read_parquet("./processed_data/land_use.parquet")
stops_df <- read_parquet("./processed_data/stm_stops.parquet") |> select(-stop_code_id)
intersections_df <- 
  read_parquet("./processed_data/data_final.parquet") |> 
  left_join(landuse_df, by = "int_no") |> 
  left_join(stops_df, by = "int_no")
set.seed(123)
inter_split <- initial_split(intersections_df)
inter_train <- training(inter_split)
inter_test <- testing(inter_split)
inter_folds <- vfold_cv(inter_train, v = 10, repeats = 5) # 10-folds, 5-repeats

# Data preprocessing (recipes) ----
# need more preprocessing recipes for different models 
# and preprocessing steps

base_rec <- 
  recipe(acc ~ ., data = inter_train) |> 
  # Create date features
  step_date(date, features = c("dow", "month")) |>
  # imputing missing dates with nearest observations
  step_impute_knn(c(starts_with("date_"), land_use), neighbors = 2, impute_with = imp_vars(x, y)) |> 
  step_select(-date) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, starts_with("rue"), borough, x, y), new_role = "ID") |>  
  step_unknown(all_nominal_predictors(), new_level = "NA")

lm_rec <- 
  base_rec |>
  step_dummy(all_factor_predictors(), one_hot = FALSE) |>
  step_nzv(all_predictors()) |>
  step_interact(terms = ~matches("ln_c"):c(median_X1:lt_prot_re_X1))

boost_rec <- 
  base_rec |> 
  step_dummy(all_factor_predictors(), one_hot = TRUE)


# Model specifications ----
pois <-
  poisson_reg(
    engine = "glmnet",
    mode = "regression",
    penalty = tune(),
    mixture = tune()
  )

pois_rel <-
  poisson_reg(
    mode = "regression",
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine(engine = "glmnet",
             relax = TRUE)

random <-
  rand_forest(
    engine = "ranger",
    mode = "regression",
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  )

boosting <- 
  boost_tree(
    engine = "xgboost",
    mode = "regression",
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  )

# Workflow sets ----
model_sets <- 
  bind_rows(workflow_set(list(base = base_rec), list(random)),
          workflow_set(list(lm = lm_rec), list(poisson_reg = pois, 
                                               poisson_relreg = pois_rel)),
          workflow_set(list(xg = boost_rec), list(boosting)))

## Parallel processing ----
tictoc::tic()
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

models_result <- 
  workflow_map(
    model_sets, 
    resamples = inter_folds, 
    fn = "tune_grid",
    grid = 30, # regular grid
    seed = 123, 
    verbose = TRUE,
    metrics = metric_set(rmse, mae, poisson_log_loss)
    )
tictoc::toc()
# What's the best model?
autoplot(models_result, select_best = TRUE)
rank_results(models_result, rank_metric = "rmse", select_best = TRUE)

# Looks like xgboost provides best model, followed closely by random forest

# Tuning Grid ----





