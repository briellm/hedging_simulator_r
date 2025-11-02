# Shiny Stock Analysis Application

## Overview

This Shiny application provides an interactive platform for exploring and visualizing stock data from the S&P 500 index. Users can select any company from the S&P 500, specify a custom date range, and generate dynamic visualizations of historical stock prices. The app combines financial data retrieval with modern data visualization to create an intuitive analytical experience.

---

## Features

- **Dynamic Data Retrieval:** Uses `tidyquant` to access live financial data from Yahoo Finance.  
- **Interactive Visualization:** Displays time-series plots of stock prices using `ggplot2`.  
- **S&P 500 Company Selector:** Users can choose from all S&P 500 companies via a searchable dropdown.  
- **Customizable Date Range:** Allows users to filter stock data over any chosen time window.  
- **Clean and Responsive Interface:** Designed for clarity and professionalism using Shiny’s fluid layout system and custom CSS styling.

---

## Technologies Used

- **Language:** R  
- **Framework:** Shiny  
- **Packages:**
  - `tidyquant` – for financial data retrieval and integration with `quantmod`
  - `quantmod` – for quantitative financial modeling
  - `ggplot2` – for plotting and visualization
  - `dplyr` – for data manipulation
  - `scales` – for axis scaling and date formatting

---

## Installation and Setup

1. **Ensure R and RStudio are installed.**  
   You can download R from [https://cran.r-project.org/](https://cran.r-project.org/) and RStudio from [https://posit.co/download/rstudio/](https://posit.co/download/rstudio/).

2. **Install Required Packages:**
   ```r
   install.packages(c("shiny", "tidyquant", "quantmod", "ggplot2", "dplyr", "scales"))



