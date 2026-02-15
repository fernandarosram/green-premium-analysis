# 🌱 Quantifying the Green Premium in Mexico City 

## Project Overview

This project analyzes the economic accessibility of agroecological food systems in Mexico City by quantifying the "Green Premium" — the price difference between sustainable and conventional food markets.

Using web scraping, data normalization, and stochastic modeling, the analysis evaluates:

-   Price volatility across market tiers

-   Median price gaps between agroecological and conventional products

-   Labor-equivalent affordability (days of minimum wage required)

## 🔍 Research Questions

-   Which market tier exhibits the highest price volatility?

-   What is the percentage premium of agroecological products?

-   How many days of minimum-wage labor are required to afford a monthly basic basket?

## 🛠️ Technical Stack

-   Language: R

-   Web Scraping: chromote, rvest

-   Data Wrangling: tidyverse

-   Simulation: Monte Carlo-style modeling

-   Visualization: ggplot2

-   Reporting: Quarto

## 📊 Data Sources

-   Industrial Retailers: Walmart, La Comer

-   Agroecological Markets: Dilmun, Mercado Alternativo, Consuma Conciencia, El Buen Campo

-   Wholesale Proxy: Central de Abasto (SNIIM)

-   Data were standardized to price per kilogram and filtered to match the Mexican Canasta Básica.

## ⚙️ Methodology Summary 

1.  Web Scraping Pipeline

    -   Extracted product-level pricing data across heterogeneous site architectures.

    -   Handled JavaScript-rendered pages via headless browser automation.

2.  Data Cleaning & Normalization

    -   Regex filtering to remove false positives.

    -   Unit standardization (grams → kilograms).

    -   Price normalization to MXN/kg.

3.  Stochastic Modeling

    -   Wholesale price bounds were used to simulate local market price distributions.

    -   Thousands of simulations were run to estimate expected value and variance.

4.  Economic Translation

    -   Monthly basket costs were benchmarked against the 2026 Mexican daily minimum wage.

    -   Affordability expressed in:

        -   \% of monthly wage

        -   Equivalent labor days

## 📈 Key Findings

-   Agroecological products show a consistent price premium.

-   Supermarkets exhibit the highest price volatility (CV).

-   Local markets provide the lowest median prices.

-   A minimum-wage worker must dedicate significantly more labor days to afford the agroecological basket.

🚧 Limitations

-   Prices are time-sensitive and may fluctuate daily.

-   Product equivalence across tiers may not fully reflect quality differences.

-   Informal market prices are modeled via wholesale proxies.
