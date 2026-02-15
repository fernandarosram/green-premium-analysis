# Extract prices from 'frutas y verduras' from retail supermarket
# Clean the environment
rm(list = ls())
gc()

# Load required packages
library(rvest)
library(lubridate)
library(dplyr)
library(chromote)
library(readr)
library(stringr)
library(glue)

# Load helper functions
source("scripts/00_helpers.R")

# Launch browser
Sys.setenv(CHROMOTE_HEADLESS = "0")
my_user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"

# Create a new browser session
chrome_session <- ChromoteSession$new()
chrome_session$Network$setUserAgentOverride(userAgent = my_user_agent)

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Create empty list to save data
supermarket_list <- list()
# Go to webpage
chrome_session$Page$navigate(
  "https://www.lacomer.com.mx/lacomer/#!/pasillos/252/13?succId=252&succFmt=100")
# Wait until webpage is loaded
Sys.sleep(5)
# Extract raw html
html_raw <- chrome_session$Runtime$evaluate(
  "document.documentElement.outerHTML")$result$value
# Read html
html_rvest <- read_html(html_raw)
# Extract links from departments
links_dept<- html_rvest |> 
  html_elements(".depto_link") |> 
  html_attr("href")
# Get department names
name_depts <- links_dept |> 
  URLdecode() |>
  str_split_i("\\/", 3)
# Select only links that match 'frutas' or 'verduras'
links_fyv_id <- grep("frutas|verduras|legumbres", 
                     name_depts, 
                     ignore.case = TRUE)
links_fyv <- links_dept[links_fyv_id]
# Loop to extract prices from selected departments 
# Create empty list to save results
lacomer_list <- list()
for (i in seq_along(links_fyv)){
  # Create link
  link_path <- paste0("https://www.lacomer.com.mx/lacomer/", 
                      links_fyv[i])
  # Go to link
  chrome_session$Page$navigate(link_path)
  # Wait until webpage is loaded
  Sys.sleep(5)
  # Extract raw html
  html_raw_dept <- chrome_session$Runtime$evaluate(
    "document.documentElement.outerHTML")$result$value
  # Save data in list
  current_depto_data <- list()
  current_depto_data[[1]] <- lacomer_data(html_raw_dept)
  # Search for more pages in department
  pages_section_html <- read_html(html_raw_dept) |> 
    html_element(".paginator-container")
  
  n_pags <- pages_section_html |> 
    html_elements(".ng-binding") |> 
    html_text2()
  
  # If there is more than one page in the department, then extract the 
  # data from each page
  if(length(n_pags) > 1){
    for(e in 2:length(n_pags)){
      # Click to go to next page
      chrome_session$Runtime$evaluate(
        expression = "Array.from(document.querySelectorAll('a')).
        find(el => el.textContent === 'Siguiente').click();"
      )
      Sys.sleep(10)
      # Extract raw html
      html_raw_dept <- chrome_session$Runtime$evaluate(
        "document.documentElement.outerHTML")$result$value
      current_depto_data[[e]] <- lacomer_data(html_raw_dept)
    }
  }
  lacomer_list[[i]] <- bind_rows(current_depto_data)
}

supermarket_list[[1]] <- bind_rows(lacomer_list)

# Save data# Save data# Save data
# file_name_lc <- paste0("../data/lacomer_", Sys.Date(), ".csv")
# 
# rio::export(supermarket_list[[1]], 
#             file_name_lc)
##### 02. Walmart
# Launch browser
Sys.setenv(CHROMOTE_HEADLESS = "0")
my_user_agent <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"

# Create a new browser session
chrome_session <- ChromoteSession$new()
chrome_session$Network$setUserAgentOverride(userAgent = my_user_agent)

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Create empty list to save data
walmart_list <- list()
# Go to webpage
chrome_session$Page$navigate(
  "https://www.walmart.com.mx/content/frutas-y-verduras/120007")
# Wait until webpage is loaded
Sys.sleep(3)
department_names <- c("Frutas", "Verduras", "Jitomates, Aguacates y básicos")
for (dept in department_names){
  chrome_session$Page$navigate(
    "https://www.walmart.com.mx/content/frutas-y-verduras/120007")
  Sys.sleep(3)
  js_expr <- glue("Array.from(document.querySelectorAll('button')).
        find(el => el.textContent === '{dept}').click();")
  chrome_session$Runtime$evaluate(
    expression = js_expr
  )
  Sys.sleep(4)
  
  html_raw_dept <- chrome_session$Runtime$evaluate(
    "document.documentElement.outerHTML")$result$value
  
  html_raw_dept <- read_html(html_raw_dept)
  html_subdepts_links <- html_raw_dept |> 
    html_nodes("a[data-dca-id='L:0099F98E09']")|> 
    html_attr("href")
  subdept_names <- str_split_i(html_subdepts_links, "/", 5)
  subdept_ids <- which(subdept_names %in% 
                         c("aderezos-y-jugos-naturales", 
                           "bactericidas", 
                           "ensaladas-y-empacados"))
  html_subdepts_links_sel <- html_subdepts_links[-subdept_ids]
  subdept_list <- list()
  for(i in seq(html_subdepts_links_sel)){
    link <- paste0("https://www.walmart.com.mx", html_subdepts_links_sel[i])
    chrome_session$Page$navigate(link)
    Sys.sleep(5)
    chrome_session$Runtime$evaluate("Array.from(document.querySelectorAll('button')).
        find(el => el.textContent === 'Marca').click();")
    Sys.sleep(2)
    chrome_session$Runtime$evaluate("
    var spans = document.querySelectorAll('span');
    var target = Array.from(spans).find(el => el.textContent === 'Frutas Y Verduras Frescas');
    if (target) { target.click(); }")
    Sys.sleep(1)
    chrome_session$Runtime$evaluate("(() => {
    const buttons = Array.from(document.querySelectorAll('button'));
    const target = buttons.find(btn => btn.innerText.includes('Ver resultados'));
    if (target) {
      target.click();
    }
  })()")
    Sys.sleep(5)
    html_raw_subdept <- chrome_session$Runtime$evaluate(
      "document.documentElement.outerHTML")$result$value
    html_rvest_subdept <- read_html(html_raw_subdept)
    subdept_list[[i]] <- walmart_data(html_rvest_subdept)
  }
  walmart_list[[dept]] <- bind_rows(subdept_list)
}

supermarket_list[[2]] <- bind_rows(walmart_list)

df_supermarket <- bind_rows(supermarket_list)

df_path <- paste0("data/raw/supermarket_data_", 
                         Sys.Date(), 
                         ".csv")
rio::export(df_supermarket, 
            df_path)
