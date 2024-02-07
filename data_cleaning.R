library(tidyverse)
library(arrow)

data <- 
  read_delim("./data/data_final.csv", delim = ";", show_col_types = FALSE) |> 
  mutate(int_no = as.character(int_no),
         across(c(all_pedest, median:parking, any_exclus, all_red_an), as_factor),
         date = dmy(date_),
         borough = case_when(
           borough == "C¶te-des-Neiges-Notre-Dame-de-Graces" ~ "Côte-des-Neiges-Notre-Dame-de-Graces",
           borough == "Pointe-aux-Trembles-RiviÞres-des-Prairies" ~ "Pointe-aux-Trembles-Rivières-des-Prairies",
           borough == "C¶te-Saint-Luc" ~ "Côte-Saint-Luc",
           borough == "?le-Bizard-Sainte-GeneviÞve"  ~ "Île-Bizard-Sainte-Geneviève",
           borough == "MontrÚal-Nord"  ~ "Montréal-Nord",
           borough == "MontrÚal-Est"  ~ "Montréal-Est",
           borough == "St-LÚonard"  ~ "St-Léonard",
           .default = borough
         )) |> 
  select(-c(date_, `...60`,`...61`)) # column 60-61 are duplicates I think

# extra cleaning
data$rue_2[data$rue_2=="P"] <- "Paul-Émile-Lamarche"
# discreptancy between french and english names
data$rue_2[data$int_no == "1038"] <- "800 Ouest face au Loblaws"
# Note: Most discreptancy are encoding errors or rue_1=street_2 and rue_2=street_1. 
# These are not worth addressing since we will be dropping the problematic english version of names
# duplicate intersection number
data$int_no[data$int_no == "9151"][1] <- "9150" #int_no is not used
# MV for ln_distdt, replace with 0. distdt is 0 (ln(0)=-inf), intersection 344 is located downtown
data$ln_distdt[is.na(data$ln_distdt)] <- 0

# final dataset
data <- select(data, -c(street_1, street_2))

write_parquet(data, "./data/data_final.parquet")
