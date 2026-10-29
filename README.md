# ST558 Project 2
This repository contains the files for ST558 Project 2.

## US Superstore Data Explorer

This Shiny app explores sales, profits, and purchasing activity in the US Superstore data set. Its purpose is to help users compare product categories, customer segments, and regions, and identify which products generate the most revenue and profit.

Users can filter the data by region, category, and numeric ranges, then explore contingency tables, numerical summaries, product rankings, and six types of plots. The app also allows users to download the filtered data as a CSV file. 

You can check out the Shiny App [here!](https://ajoshy2.shinyapps.io/ST558_Project2/)

You can also view the exploratory EDA Quarto Document [here.](https://amalbiz.github.io/ST558_Project2/)

## Main Files

- `app.R`: Shiny application.
- `app_prep.qmd`: Static exploratory data analysis.
- `US Superstore data.xls`: Dataset.

## Running the App

If you want to run the app locally, install the following packages as necessary:
Paste the following code in your R console.

```r
install.packages(c("shiny", "tidyverse", "readxl", "DT", "shinycssloaders", "janitor"))
