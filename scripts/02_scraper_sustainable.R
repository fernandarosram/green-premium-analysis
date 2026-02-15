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
source("scripts/00_helpers.R")

# Create empty list to save results
sustainable_list <- list()

##### Mercado Alternativo
# Read html
html_raw <- read_html("https://mercadoalternativo.org/collections/frutas")
# Create empty list to save page results
mercadoalternativo_list <- list()
# Save page 1 data
mercadoalternativo_list[[1]] <- mercalter_data(html_raw)
# If there is more than 1 page, extract data from next pages
pags_num <- html_raw |> 
  html_elements(".pagination-custom__num") |> 
  html_text2()
if (length(pags_num) > 1){
  for (i in (2:length(pags_num))){
    # Create page link
    link <- paste0("https://mercadoalternativo.org/collections/frutas?page=", i)
    html_raw <- read_html(link)
    mercadoalternativo_list[[i]] <- mercalter_data(html_raw)
  }
}
sustainable_list[[1]] <-  bind_rows(mercadoalternativo_list)


##### Dilmun
# Launch browser
Sys.setenv(CHROMOTE_HEADLESS = "0")
my_user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
# Create a new browser session
chrome_session <- ChromoteSession$new()
chrome_session$Network$setUserAgentOverride(userAgent = my_user_agent)

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Navigate to page
chrome_session$Page$navigate("https://dilmun.mx/collections/1-frutas_y_verduras")
Sys.sleep(5)
# Extract html
raw_html <- chrome_session$Runtime$evaluate(
  "document.documentElement.outerHTML")$result$value
raw_html_1 <- read_html(raw_html)
pags_links <- raw_html_1 |> 
  html_elements(".pagination-link") |>  
  html_attr("href")
# Extract data from first page
dilmun_list <- list()
dilmun_list[[1]] <- dilmun_data(raw_html_1)

# Navigate to next pages
for (i in seq(pags_links)[-1]){
  next_p_link <- paste0("https://dilmun.mx/",pags_links[i])
  chrome_session$Page$navigate(next_p_link)
  Sys.sleep(5)
  # Extract html
  raw_html <- chrome_session$Runtime$evaluate(
    "document.documentElement.outerHTML")$result$value
  raw_html_1 <- read_html(raw_html)
  # Extract data from page
  dilmun_list[[i]] <- dilmun_data(raw_html_1)
}

# Extract and save data
sustainable_list[[2]] <-  bind_rows(dilmun_list)

#### Consumaconciencia
# Extract html
raw_html <- read_html("https://consumaconciencia.com/categoria-producto/alimentos/vegetales/")
clean_df <- consuma_data(raw_html)
consuma_list <- list()
consuma_list[[1]] <- clean_df
# Check for more pages
pags_links <- raw_html |> 
  html_elements(".page-numbers") |> 
  html_attr("href")
pags_tot <- length(pags_links)
if (pags_tot > 1){
  for (i in (2:pags_tot)){
    link <- paste0(
      "https://consumaconciencia.com/categoria-producto/alimentos/vegetales/page/", 
      i)
    raw_html <- read_html(link)
    clean_df <- consuma_data(raw_html)
    consuma_list[[i]] <- clean_df
  }
}
sustainable_list[[3]] <- bind_rows(consuma_list)


#####   EL BUEN CAMPO

department_names <- c("frutas", "verduras")
buencampo_list <- list()
for (x in seq_along(department_names)){
  link_pag <- paste0( "https://www.elbuencampo.com/collections/", department_names[x])
  raw_html <- read_html(link_pag)
  list_id <- paste0(department_names[x], "_1")
  buencampo_list[[list_id]] <-  buencampo_data(raw_html)
  
  pags_links <- raw_html |> 
    html_elements(".t4s-pagination__item.link") |> 
    html_attr("href")
  
  if (length(pags_links) >= 1){
    num_pags <- stringr::str_split_i(pags_links[length(pags_links)], "=", 2)
    for (i in (2:num_pags)){
      link_pag <- paste0("https://www.elbuencampo.com/collections/", department_names[x], "?page=", i)
      raw_html <- read_html(link_pag)
      list_id <- paste0(department_names[x], "_", i)
      buencampo_list[[list_id]] <-  buencampo_data(raw_html)
    }
  }
}
sustainable_list[[4]] <- bind_rows(buencampo_list)

  
df_sust <- bind_rows(sustainable_list)

df_path <- paste0("data/raw/sustainable_markets_data_", 
                  Sys.Date(), 
                  ".csv")
rio::export(df_sust, 
            df_path)
