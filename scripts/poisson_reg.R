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
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |> 
  # May differ for boosting
  step_dummy(all_factor_predictors()) |> 
  step_nzv(all_predictors())

library(embed)

pca_traff <- 
  base |> 
  step_pca_sparse(
    c(any_of(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100") ), -acc),
    predictor_prop = tune(),
    num_comp = tune(),
    id = "sparse pca"
  )

pca_geom <- 
  base |> 
  step_pca_sparse(
    c(any_of(c("tot_crossw", "number_of",  "avg_crossw", "tot_road_w", "total_lane", "of_exclusi", "commercial", "curb_exten")), -acc),
    predictor_prop = tune(),
    num_comp = tune(),
    id = "sparse pca"
  )

pca_both <- 
  base |> 
  step_pca_sparse(
    c(any_of(c("tot_crossw", "number_of",  "avg_crossw", "tot_road_w", "total_lane", "of_exclusi", "commercial", "curb_exten")), -acc),
    predictor_prop = tune(),
    num_comp = tune(),
    id = "sparse pca geom2"
  ) |> 
  step_pca_sparse(
    c(any_of(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100") ), -acc),
    predictor_prop = tune(),
    num_comp = tune(),
    id = "sparse pca traff2"
  )

base_int <- 
  base |> 
  step_interact(terms = ~ matches(c("all_pedest", "median", "green_stra", "half_phase", "any_ped_pr", "ped_countd", "lt_protect", "lt_restric", "lt_prot_re", "parking", "any_exclus", "all_red_an",   "new_half_r")):matches(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100")))

base_trans <- 
  base |> 
  step_poly(c(any_of(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100") ), -acc), degree = tune())

base_both <- 
  base |> 
  step_interact(~matches(c("all_pedest", "median", "green_stra", "half_phase", "any_ped_pr", "ped_countd", "lt_protect", "lt_restric", "lt_prot_re", "parking", "any_exclus", "all_red_an",   "new_half_r")):matches(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100") )) |> 
  step_poly(c(starts_with(c("pi","fi","fli", "fri","fti","cli","cri","cti","acc", "ln_pi","ln_fi","ln_fli", "ln_fri","ln_fti","ln_cli", "ln_cri","ln_cti","north_veh", "north_ped","east_veh","east_ped", "south_veh","south_ped","west_veh", "west_ped","traffic_10000", "ped_100") ), -acc), degree = tune())

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



