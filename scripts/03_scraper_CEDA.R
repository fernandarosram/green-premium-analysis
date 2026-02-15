# Extract prices from sustainable markets
# Clean the environment
rm(list = ls())
gc()

# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(chromote)
# Load helper functions
source("00_helpers.R")

# Launch browser
Sys.setenv(CHROMOTE_HEADLESS = "0")
my_user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
# Create a new browser session
chrome_session <- ChromoteSession$new()
chrome_session$Network$setUserAgentOverride(userAgent = my_user_agent)

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Navigate to page
chrome_session$Page$navigate("https://www.economia-sniim.gob.mx/nuevo/consultas/mercadosnacionales/preciosdemercado/agricolas/consultafrutasyhortalizas.aspx")
Sys.sleep(5)
destino_value <- 100
chrome_session$Runtime$evaluate(
  expression = paste0("
    (function() {
      const selectElement = document.querySelector('#ddlDestino');
      if (selectElement) {
        selectElement.value = '", destino_value, "';
        const event = new Event('change', { bubbles: true });
        selectElement.dispatchEvent(event);
        return true;
      }
      return false;
    })()
  ")
)

presentacion_value <- 2
chrome_session$Runtime$evaluate(
  expression = paste0("
    (function() {
      const selectElement = document.querySelector('#ddlPrecios');
      if (selectElement) {
        selectElement.value = '", presentacion_value, "';
        const event = new Event('change', { bubbles: true });
        selectElement.dispatchEvent(event);
        return true;
      }
      return false;
    })()
  ")
)
  
Sys.sleep(2)
chrome_session$Runtime$evaluate(
  expression = "document.querySelector('#btnBuscar').click();"
)
  Sys.sleep(4)
  #extraer tabla usando selector
  res <- chrome_session$Runtime$evaluate("document.querySelector('#tblResultados').outerHTML")
  raw_html <- res$result$value
  #convertir a objeto rvest
  rvest_html <- read_html(raw_html)
  filas <- rvest_html |> html_nodes(".Datos2") |> html_text2()
  
  tabla <- data.frame(matrix(filas, ncol = 8, byrow = TRUE))
  names_df <- rvest_html |> html_nodes(".titDATtab2") |> html_text2()
  names(tabla) <- names_df
  raw_df <- tibble(
    product_name =  tabla$Producto,
    unit_raw = tabla$Presentación,
    price_freq = as.numeric(tabla$`Precio Frec`),
    price_min = as.numeric(tabla$`Precio Mín`),
    price_max = as.numeric(tabla$`Precio Max`))
  clean_df <- raw_df |>
    mutate(
      product_name = case_when(
        str_detect(product_name, "Cebolla Bola") ~ "cebolla blanca",
        TRUE ~ product_name),
      unit = case_when(
        str_detect(unit_raw, "(?i)kg|Kilogramo") ~ "kg",
        str_detect(unit_raw, "(?i)Pieza|(?i)Docena|(?i)Ciento") ~ "pza",
        TRUE ~ "unidad" ),
      quantity = 1,
      price_freq_per_unit = price_freq/quantity,
      price_min_per_unit = price_min/quantity,
      price_max_per_unit = price_max/quantity,
      date = Sys.Date(),
      seller = "central_abastos")
  
file_path <- paste0("data/raw/central_abastos_",Sys.Date(),".csv")  
rio::export(clean_df, file_path)
