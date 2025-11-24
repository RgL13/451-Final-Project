# 451-Final-Project

## Introduction
This project presents an interactive Shiny application designed to help users explore global health and disease burden data across countries and continents.
The app allows users to visualize patterns, compare regions, and analyze trends using data from the Global Burden of Disease (GBD) study.

Specifically, this dashboard investigates:

- Regional disease burden: Are certain regions (e.g., Southern Africa, Asia, Europe) consistently associated with higher death rates across diseases?

- Top diseases in a selected country: Among user-selected diseases, which top five have the highest burden in a specific country?

- Gender-based differences in cardiovascular disease (CVD): How do male and female CVD prevalence rates compare in Latin America and the Caribbean?

The goal is to deliver a clear, interactive, and user-friendly platform for exploring global health inequalities and disease trends.

## Installation
The data used in this application were downloaded from the Global Burden of Disease (GBD) results tool provided by the Institute for Health Metrics and Evaluation (IHME). 

You can access the dataset at: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-public/72889dab7d9d7ad1999610984120a1b9

Use the left-hand filters on the website to select the disease categories, measures, locations, age groups, and years you want to export.

After downloading the CSV file:

- Place the dataset in your project directory.

- Make sure the file path in your Shiny app matches the actual dataset location.

## Usage
To run the Shiny application locally:

- Open the project folder in RStudio.

- Ensure that everything(app.r and datasets) are all in the same directory.

- Install required packages if needed

- Runapp in the server.R file
