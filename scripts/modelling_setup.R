# Package installation ----
# install.packages(c("tidyverse", "arrow", "tidymodels", "glmnet", "poissonreg", "ranger", "xgboost", "doFuture"))

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
stops_df <- read_parquet("./processed_data/stm_stops.parquet") 
intersections_df <- 
  # Main dataset
  read_parquet("./processed_data/data_final.parquet") |> 
  # Joining with land_use for each intersections 
  # NAs will be imputed in preprocessing below
  left_join(landuse_df, by = "int_no", relationship = "one-to-one") |> 
  # Joining with STM stops
  left_join(stops_df, by = "int_no", relationship = "one-to-one") |>
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
  select(-c(all_red_an, half_phase, rue_1, rue_2, date, stop_code_id))

# data type
dict <- readxl::read_xlsx("./references/Dictionnaire_final.xlsx")
variables <- 
  map(unique(dict$TYPE), \(x) 
      dict |> 
        filter(TYPE == x) |>
        pull(NOM)) |> 
  set_names(unique(dict$TYPE))