# Helper functions

# Load required packages
library(rvest)
library(lubridate)
library(dplyr)
library(chromote)
library(readr)
library(stringr)

# Extract data from 'la comer'
std_units <- function(u) {
  u <- str_to_lower(u)
  case_when(
    str_detect(u, "pz|pieza") ~ "pza",
    str_detect(u, "kg|^k$") ~ "kg",
    str_detect(u, "g|gr|gramos") ~ "g",
    TRUE ~ u
  )
}

infer_qty <- function(qty_extracted, unit_std) {
  qty_numeric <- as.numeric(str_extract(qty_extracted, "\\d+\\.?\\d*"))
  
  case_when(
    !is.na(qty_numeric) ~ qty_numeric,
    str_detect(unit_std, "pza|kg|manojo|cabeza|canasta") ~ 1,
    TRUE ~ NA_real_
  )
}

lacomer_data <- function(raw_html){
  # Read html
  html_rvest_dept <- read_html(raw_html)
  # Extract product nodes
  nodes_products <- html_rvest_dept |> 
    html_nodes(".li_mosaic")
  # Dataframe with raw data
  raw_df <- tibble(raw_product_name = nodes_products |> 
                     html_element("strong.ng-binding") |> 
                     html_text2(),
                   price = nodes_products |> 
                     html_element(".precio_normal") |> 
                     html_text2() |> 
                     readr::parse_number() |>
                     as.numeric(),
                   unit_raw = nodes_products |> 
                     html_element(".li_producto .text-center p.ng-binding") |> 
                     html_text2() |> stringr::str_split_i("\n|ngIf", 4))
  # Clean product name
  clean_df <- raw_df |> mutate(
    product_name = str_replace_all(raw_product_name,
                                    "[^[:alpha:][:space:]]", 
                                    "") |> str_trim(),
    unit = unit_raw |> 
      str_replace_all("[^[:alpha:]]", "") |> 
      str_replace_all("(?i)grs|gr", "g") |> 
      str_trim(),
    quantity = as.numeric(readr::parse_number(unit_raw)),
    seller = "lacomer",
    date = Sys.Date()
  ) |> 
    select(product_name, price, quantity, unit, date, seller)
  clean_df
}


# Extract product data from Mercado Alternativo pages

mercalter_data <- function(html_raw){
  # Extract product data
  raw_df <- tibble(
    raw_text = html_raw |> 
      html_elements(".product__grid__title") |> 
      html_text2(),
    price = html_raw |> 
      html_elements(".product__grid__price") |> 
      html_text2() |> 
      readr::parse_number() |>
      as.numeric()
  )
  
  clean_df <- raw_df |>
    separate(raw_text,
             into = c("product_name", "unit_info"),
             sep = "-|\\|",
             extra = "merge",
             fill = "right") |>
    mutate(
      quantity = as.numeric(str_extract(unit_info, "\\d+")),
      unit = str_extract(unit_info, "[a-zA-Záéíóúñ]+"),
      unit = std_units(unit),
      quantity = (ifelse(is.na(quantity), 1, quantity)),
      product_name = str_trim(product_name),
      seller = "mercado_alternativo",
      date = Sys.Date()
    ) |>
    select(product_name, price, quantity, unit, date, seller)
  clean_df
}

scroll <- function(){
  get_height <- function() {
    chrome_session$Runtime$evaluate("document.body.scrollHeight")$result$value
  }
  repeat {
    old_height <- get_height()
    chrome_session$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight)")
    Sys.sleep(4) 
    new_height <- get_height()
    if (new_height == old_height) {
      break
    }
  }
}

# ---- dilmun-function
dilmun_data <- function(raw_html){
  product_nodes <- raw_html_1 |> 
    html_nodes(".product-card")
  
  raw_df <- tibble(
    raw_text = product_nodes |>
      html_elements(".product-card-title") |> 
      html_text2(),
    price = product_nodes |> 
      html_elements(".price-item-regular") |> 
      html_text2() |>
      readr::parse_number() |>
      as.numeric()
  )
  
  clean_df <- raw_df |>
    mutate(
      quantity = case_when(
        str_detect(raw_text, "(?i)manojo") ~ "1",
        TRUE ~ str_extract(raw_text, "\\d+\\.?\\d*(?=\\s*(kg|g|k|pz|pieza|pza))")
      ) |> as.numeric(),
      unit = case_when(
        str_detect(raw_text, "(?i)kg") ~ "kg",
        str_detect(raw_text, "(?i)g") ~ "g",
        str_detect(raw_text, "(?i)pieza|pz") ~ "pza",
        TRUE ~ str_extract(raw_text, "(?i)manojo") |> 
          str_to_lower()
      ),
      product_name = raw_text |>
        str_remove_all("\\(.*?\\)") |>
        str_remove_all("(?i)\\d+\\.?\\d*\\s*(kg|g|k|pz|pieza|pza|manojo)") |>
        str_remove_all(
          "(?i)aprox\\.?|en domo|domo|en manojo|importado|Proyecto Distinto|Campo Vivo"
        ) |>
        str_remove_all("\\s+[a-zA-Z]$") |>
        str_squish() |> 
        str_remove_all("[[:punct:]]$"),
      date = Sys.Date(),
      seller = "dilmun" 
    ) |>
    select(product_name, price, quantity, unit, date, seller)
  clean_df
}
#----

consuma_data <- function(raw_html){
  product_nodes <- raw_html |> html_nodes(".product-warp-item")
  raw_df <- tibble(
    raw_text = product_nodes |> 
      html_elements(".name") |> 
      html_text2(),
    price = product_nodes 
    |> html_element(".price") |> 
      html_text2() |>
      readr::parse_number() |> 
      as.numeric()
  )
  clean_df <- raw_df |>
    mutate(
      text_limpio = str_replace_all(raw_text, "[\r\n\t]", "") |> str_trim(),

      quantity = case_when(
        str_detect(text_limpio, "(?i)manojo|pza|pieza|cabeza") & !str_detect(text_limpio, "\\d") ~ "1",
        TRUE ~ str_extract(text_limpio, "\\d+\\.?\\d*(?=\\s*(?i)(kg|gr|g|pz|pza|piezas))")
      ) |> 
        as.numeric(),
      unit = case_when(
        str_detect(text_limpio, "(?i)manojo") ~ "manojo",
        str_detect(text_limpio, "(?i)cabeza") ~ "cabeza",
        str_detect(text_limpio, "(?i)pza|pieza") & !str_detect(text_limpio, "\\d") ~ "pza",
        TRUE ~ str_extract(text_limpio, "(?i)kg|gr|g|pz|pza|piezas") |> str_to_lower()
      ) |> 
        str_replace_all("gr|g", "grs") |> 
        str_replace_all("piezas|pz", "pza"),
      
      product_name = text_limpio |>
        str_remove_all("\\(.*?\\)|\\[.*?\\]") |>
        str_remove_all("(?i)\\d+\\.?\\d*\\s*(kg|gr|g|pz|pza|piezas|manojo|cabeza)") |>
        str_remove_all("(?i)\\b(domo|manojo|pza|piezas|orgánicos|fresca|fresco)\\b") |>
        str_remove_all("\\s+[a-zA-Z]$") |>
        str_squish() |>
        str_remove_all("[[:punct:]]$"),
      date = Sys.Date(),
      seller = "consuma") |>
    select(product_name, price, quantity, unit, date, seller)
}

consuma_data <- function(raw_html){
  product_nodes <- raw_html |> html_nodes(".product-warp-item")
  raw_df <- tibble(
    raw_text = product_nodes |> 
      html_elements(".name") |> 
      html_text2(),
    price = product_nodes 
    |> html_element(".price") |> 
      html_text2() |>
      readr::parse_number() |> 
      as.numeric()
  )
  
  clean_df <- raw_df |>
    mutate(
      text_limpio = str_replace_all(raw_text, "[\r\n\t]", "") |> str_trim(),
      quantity = case_when(
        str_detect(text_limpio, "(?i)manojo|pza|pieza|cabeza") & !str_detect(text_limpio, "\\d") ~ "1",
        TRUE ~ str_extract(text_limpio, "\\d+\\.?\\d*(?=\\s*(?i)(kg|gr|g|pz|pza|piezas))")
      ) |>
        as.numeric(),
      unit = case_when(
        str_detect(text_limpio, "(?i)manojo") ~ "manojo",
        str_detect(text_limpio, "(?i)cabeza") ~ "cabeza",
        str_detect(text_limpio, "(?i)kg|\\d+k\\b") ~ "kg",
        str_detect(text_limpio, "(?i)grs|gr|\\d+g\\b") ~ "g",
        str_detect(text_limpio, "(?i)pz|pieza|pza") ~ "pza",
        TRUE ~ NA_character_
      ),
      product_name = text_limpio |>
        str_remove_all("\\(.*?\\)|\\[.*?\\]") |>
        str_remove_all("(?i)\\d+\\.?\\d*\\s*(kg|gr|g|pz|pza|piezas|manojo|cabeza)") |>
        str_remove_all("(?i)\\b(domo|manojo|pza|piezas|orgánicos|fresca|fresco)\\b") |>
        str_squish() |>
        str_remove_all("(?i)\\s+(pzas|pza|pzs|pz|s)$") |> 
        str_remove_all("[[:punct:]]$"),
      date = Sys.Date(),
      seller = "consuma") |>
    select(product_name, price, quantity, unit, date, seller)
  clean_df
}

buencampo_data <- function(raw_html){
  raw_df <- tibble(
    raw_text = raw_html |> 
      html_elements(".t4s-product-title") |> 
      html_text2(),
    price = raw_html |> 
      html_nodes(".t4s-product-price") |> 
      html_text2() |> 
      readr::parse_number() |>
      as.numeric()
  )
  
  clean_df <- raw_df |>
    mutate(
      unit_raw = str_extract(raw_text, "(?<=\\().*?(?=\\))") |> coalesce(raw_text),
      unit = unit_raw |> 
        str_remove_all("[0-9.]") |> 
        str_trim() |> 
        str_to_lower(),
      unit = std_units(unit),
      quantity = str_extract(unit_raw, "\\d+\\.?\\d*") |> as.numeric(),
      quantity = case_when(
        !is.na(quantity) ~ quantity,
        str_detect(unit, "pza|kg|manojo") ~ 1,
        TRUE ~ NA_real_
      ),
      product_name = raw_text |> 
        str_remove_all("\\(.*?\\)") |> 
        str_squish(),
      date = Sys.Date(),
      seller = "elbuencampo"
    ) |> 
    select(product_name, price, quantity, unit, date, seller)
}

walmart_data <- function(raw_html){
  raw_df <- tibble(
    price = raw_html |> 
      html_elements('div[data-automation-id="product-price"]') |> 
      html_element('div.b.black') |>
      html_text2() |> 
      readr::parse_number() |>
      as.numeric(),
    product_name_raw = raw_html |> 
      html_elements('span[data-automation-id="product-title"]') |>
      html_text2()
  )
  clean_df <- raw_df |>
    mutate(
      vende_por_kilo = str_detect(product_name_raw, "(?i)por kilo"),
      vende_por_pieza = str_detect(product_name_raw, "(?i)por pieza"),
      quantity_raw = product_name_raw |>
        str_extract("\\d+\\.?\\d*") |>
        as.numeric(),
      unit = case_when(
        vende_por_kilo ~ "kg",
        vende_por_pieza ~ "pza",
        TRUE ~ str_extract(product_name_raw, "(?i)kg|gr|gms|g|pz|pza|pieza|manojo") |> 
          str_to_lower() |> 
          str_replace_all("gr|gms", "g") |> 
          str_replace_all("pieza|pz", "pza") |> 
          str_squish()
        ),
      product_name = product_name_raw |>
        str_remove_all("(?i)\\d+\\.?\\d*\\s*(kg|gr|gms|g|pz|pza|pieza|mx)") |> 
        str_remove_all("(?i)\\b(charola con|domo con|empaque con|por kilo|por pieza)\\b") |>
        str_squish(),
      quantity = case_when(
        str_detect(product_name_raw, "(?i)por kilo|por pieza") ~ 1,
        !is.na(quantity_raw) ~ quantity_raw, 
        is.na(quantity_raw) & unit %in% c("kg", "pza") ~ 1, 
        TRUE ~ NA_real_),
      seller = "walmart",
      date = Sys.Date()
    ) |>
    select(product_name, price, quantity, unit, date, seller)
  clean_df
}