#Local shop price simulator
rm(list = ls())
library(dplyr)
central_data <- rio::import("data/raw/central_abastos_2026-02-13.csv")

# Set seed for reproductibility
set.seed(111)

# Define number os simualtions
n_sim <- 100

# Price simulation
local_prices_sim <- central_data |>
  slice(rep(1:n(), each = n_sim)) |>
  group_by(product_name) |>
  mutate(sim_id = row_number()) |>
  ungroup() |>
  mutate(
    margin = runif(n(), min = 0.15, max = 0.40),
    price_per_unit = price_freq * (1 + margin) * (1 + rnorm(n(), 
                                                   mean = 0, 
                                                   sd = 0.03))) |>
  mutate(
    seller = paste0("local_market_", round(margin * 100, 1))) |>
  select(product_name, 
         price_per_unit, 
         quantity, 
         unit, 
         seller, 
         date, 
         margin, 
         sim_id)
    
# Export dataframe
file_path <- paste0("data/processed/local_market_data_", Sys.Date(), ".csv")
rio::export(local_prices_sim, file_path)