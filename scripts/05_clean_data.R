# Create master dataframe and filter the selected products
rm(list = ls())
library(dplyr)
library(stringr)
library(knitr)

# Load raw datasets
raw_files <- list.files("data/raw", pattern = "data", full.names = TRUE)
raw_dfs <- raw_files |> purrr::map_dfr(read.csv)

processed_dfs <- raw_dfs |>
  mutate(
    tier = case_when(seller %in% c("lacomer", "walmart") ~ "supermarket",
                          seller %in% c ("mercado_alternativo", 
                                         "dilmun", 
                                         "consuma", 
                                         "elbuencampo") ~ "agroecological"),
    date = as.Date(date),
    margin = NA_real_,
    price_per_unit = NA_real_)

# Load local market dataset
local_data <- rio::import("data/processed/local_market_data_2026-02-13.csv")
local_data$tier <- "local"
local_data$date <- as.Date(local_data$date)

# Bind all data
master_df <- bind_rows(processed_dfs, local_data)

# Export master dataframe
df_date <- master_df$date[1]
master_path <- paste0("data/processed/master_df_", df_date,".csv")
rio::export(master_df, master_path)

# Filter the selected products
# ---- filter-selected-products

# List of products to analyze
selected_products <- rio::import("data/selected_products.csv")

# Create character string to search for selected products in master dataframe
search_words <- paste0(selected_products$product_name, collapse = "|")

# Filter data to remove noise 
noise_wrds <- c("flor","4x","brote","hallowen","brujita","fantasia",
                "te de","sin cáscara","polvo","de colores", "naturesweet")
varieties <- c("uva", "valenciano", "morado", "macho", "calabaza bola", 
               "pay","castilla","coliflor","flor","germinado","hoja","baby",
               "cherry","heirloom","jugo","microgreens","cambray","papalo",
               "pimiento","dominico", "campari", "butternut", "acorn", 
               "criolla", "de rabo", "blood orange", "pink navel",
               "eureka", "té", "japonesa", "eureka")
noise <- paste0("\\b(", 
                   paste(c(noise_wrds, 
                           varieties), 
                         collapse = "|"), 
                   ")\\b")


selected_products <- master_df |>
  mutate(product_name = str_to_lower(product_name) |> str_squish()) |>
  filter(str_detect(product_name, search_words)) |>
  filter(!str_detect(product_name, noise))

# Create column product_group
selected_products <- selected_products |>
  mutate(product_group = case_when(
    str_detect(product_name, "aguacate|aguacate hass") ~ "aguacate",
    str_detect(product_name, "calabaza|calabacita|calabacitas") ~ "calabaza",
    str_detect(product_name, "cebolla|cebolla blanca") ~ "cebolla",
    str_detect(product_name, "chayote") ~ "chayote",
    str_detect(product_name, "espinaca") ~ "espinaca",
    str_detect(product_name, "jitomate|tomate saladette|tomate bola") ~ "jitomate",
    str_detect(product_name, "limon|limón") ~ "limon",
    str_detect(product_name, "manzana") ~ "manzana",
    str_detect(product_name, "melon|melón") ~ "melon",
    str_detect(product_name, "naranja") ~ "naranja",
    str_detect(product_name, "nopal|nopales") ~ "nopal",
    str_detect(product_name, "papaya") ~ "papaya",
    str_detect(product_name, "\\bpapa\\b") ~ "papa",
    str_detect(product_name, "platano|plátano") ~ "platano",
    str_detect(product_name, "tomate verde|tomatillo") ~ "tomate",
    str_detect(product_name, "zanahoria") ~ "zanahoria", 
    TRUE ~ product_name
  )
  )


# ----

# ---- price-normalization

# Conversion data (piezas or manojo to kilograms)
conv_data <- rio::import("data/units_conversion.csv")
df_w_conv <- selected_products |> 
  left_join(conv_data, by = "product_group") |>
  mutate(price_per_unit = case_when(
    tier %in% c("agroecological", "supermarket") & 
      str_detect(unit, "manojo|pza") ~ (pieces_per_kg * price),
    tier %in% c("agroecological", "supermarket") & 
      str_detect(unit, "kg") ~  (price/quantity),
    tier %in% c("agroecological", "supermarket") & 
      str_detect(unit, "g") ~ (price/quantity)*1000,
    TRUE ~ price_per_unit
  )
  ) |>
  select(product_name, product_group, price_per_unit, tier, seller, date, margin)

# ----

# Export selected products dataframe
slctd_prods_path <- paste0("data/processed/selected_products_", df_date,".csv")
rio::export(df_w_conv, slctd_prods_path)
