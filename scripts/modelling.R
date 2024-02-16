# Package installation ----
# install.packages(c("tidyverse", "arrow", "tidymodels", "poissonreg"))

# Loading packages ----
library(tidyverse) # data manipulations
library(arrow) # .parquet files
library(tidymodels) # modelling framework
library(poissonreg) # regularized poisson
library(xgboost) # 
library(doFuture)

# Data split ----
set.seed(123)
intersections_df <- read_parquet("./data/data_final.parquet")
inter_split <- initial_split(intersections_df)
inter_train <- training(inter_split)
inter_test <- testing(inter_split)
inter_folds <- vfold_cv(inter_train, v = 5, repeats = 5)

# Data preprocessing (recipes) ----
# need more preprocessing recipes for different models 
# and preprocessing steps

base_lm <- 
  recipe(acc ~ ., data = inter_train) |> 
  # Create date features
  step_date(date, features = c("dow", "month")) |>
  # imputing missing dates with nearest observations
  step_impute_knn(starts_with("date_"), neighbors = 2, impute_with = imp_vars(x, y)) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, starts_with("rue"), borough, x, y, date), new_role = "ID") |>  
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  step_dummy(all_factor_predictors(), one_hot = FALSE) |> 
  step_nzv()

# Model specifications ----
poisson <-
  poisson_reg(
    engine = "glmnet",
    mode = "regression",
    penalty = tune(),
    mixture = tune()
  )
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
  workflow_set(preproc = list(base_lm),
               models = list(poisson, random, boosting)
               )

## Parallel processing ----
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

models_result <- 
  workflow_map(
    model_sets, 
    resamples = inter_folds, 
    fn = "tune_grid",
    grid = 1, 
    seed = 123, 
    verbose = TRUE
    )

# What's the best model?

# Tuning Grid ----





