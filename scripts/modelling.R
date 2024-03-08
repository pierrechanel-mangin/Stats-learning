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


# Data loading ----
landuse_df <- read_parquet("./processed_data/land_use.parquet")
stops_df <- read_parquet("./processed_data/stm_stops.parquet") |> select(-stop_code_id)
intersections_df <- 
  # Main dataset
  read_parquet("./processed_data/data_final.parquet") |> 
  # Joining with land_use for each intersections 
  # NAs will be imputed in preprocessing below
  left_join(landuse_df, by = "int_no") |> 
  # Joining with STM stops
  left_join(stops_df, by = "int_no") |>
  # Lumping factors with too little observations
  mutate(
    borough = case_when(
      borough %in% c("Île-Bizard-Sainte-Geneviève", "Dorval", "Kirkland", "Beaconsfield", "Pierrefonds-Roxboro", "Lachine", "Dollard-des-Ormeaux") ~ "West-Island",
      borough %in% c("Montréal-Est", "Pointe-aux-Trembles-Rivières-des-Prairies", "Anjou") ~ "Montreal_greaterRDP",
      borough %in% c("Lasalle", "Verdun") ~ "LaSalle_Verdun",
      borough %in% c("Mont-Royal", "Saint-Laurent") ~ "Saint-Laurent_TMR",
      borough %in% c("Côte-des-Neiges-Notre-Dame-de-Graces", "Côte-Saint-Luc", "Hampstead", "Outremont") ~ "CDN-NDG_greater",
      borough %in% c("Sud-Ouest", "Westmount") ~ "Sud-Ouest_Westmount",
      .default = as.character(borough)
    ),
    land_use = case_when(
      land_use %in% c("grand_espace_vert_ou_récréation", "conservation") ~ "dominante_résidentielle",
      land_use %in% c("grande_emprise_ou_grande_infrastructure_publique") ~ "industrie",
      .default = as.character(land_use)
    ),
    across(c(land_use, borough), ~as.factor(.)),
    acc_bin = if_else(acc == 0, factor(0), factor(1))
  ) |> 
  # dropping variables 
  # "all_red_an": too little obs in minority class
  # "half_phase": replace with corrected "new_half_r"
  # rue_1 & rue_2: not used in model
  select(-c(all_red_an, half_phase, rue_1, rue_2, date))

# data type
dict <- readxl::read_xlsx("./references/Dictionnaire_final.xlsx")
variables <- 
  map(unique(dict$TYPE), \(x) 
      dict |> 
        filter(TYPE == x) |>
        pull(NOM)) |> 
  set_names(unique(dict$TYPE))


# Data split ----
set.seed(123) # Reproducibility
inter_split <- initial_split(intersections_df, prop = 3/4)
inter_train <- training(inter_split)
inter_test <- testing(inter_split)
# CV: 10-folds, 5-repeats
inter_folds <- vfold_cv(inter_train, v = 10, repeats = 5) 

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
  set_engine(engine = "glmnet")

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
  step_dummy(all_factor_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  update_role(matches(variables$geometry_data), new_role = "geometry_data") |> 
  update_role(matches(variables$traffic_data), new_role = "traffic_data") |> 
  update_role(matches(variables$safety_measures), new_role = "safety_measures")
base_trans <- 
  base |> 
  step_poly(has_role("traffic_data"), degree = 2)

boost_wk <- workflow(preprocessor = base_trans, spec = boosting)
grid <- 
  extract_parameter_set_dials(boost_wk) |> 
  update("mtry" = mtry(range(1, 20))) |> 
  grid_max_entropy(size = 50)

tictoc::tic()
all_cores <- parallel::detectCores(logical = FALSE)
registerDoFuture()
cl <- parallel::makeCluster(all_cores)
plan(cluster, workers = cl)

best_boost <- tune_grid(
  boost_wk,
  inter_folds,
  grid = grid,
  metrics = metric_set(rmse, mae, poisson_log_loss),
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE)
)
tictoc::toc()

final_boost <- 
  finalize_workflow(boost_wk, select_by_one_std_err(best_boost, metric = "mae", trees)) |> 
  last_fit(inter_split)
pred_boost <- 
  final_boost |> 
  extract_workflow() |> 
  predict(intersections_df) |> 
  bind_cols(acc = intersections_df$acc, int_no = intersections_df$int_no)

# saveRDS(best_boost, "./output/boosting_transform")

