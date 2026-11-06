# Predictive Analysis of Physical Activity Research Output in Africa

Forecasting the future landscape of physical activity research across Africa.

This project was completed by **Rushad Nihal** as part of his **final thesis at Arizona State University (ASU)**.

## Overview

The project studies how physical activity research output in six African countries (South Africa, Nigeria, Ethiopia, Ghana, Kenya and Cameroon) relates to national economic, social and research-capacity indicators. The aim is to build a country-year dataset that can support forecasting of future research output.

## What this repository contains

| File | Purpose |
|---|---|
| `optimized pipeline.R` | End-to-end R script that builds the analysis dataset and runs the exploratory analysis, section by section |
| `LICENSE` | MIT License |

The script is organized into sections. Section 1 prepares the publication data. Sections 2 to 8 each merge one country-level indicator and then look at its relationship with research output.

1. **Research data preparation:** cleans a bibliographic dataset of physical activity studies in Africa and computes cumulative publications per country over time.
2. **GDP** (constant US$)
3. **Gender Inequality Index (GII)**
4. **Human Development Index (HDI)**
5. **Inequality-adjusted HDI (IHDI)**
6. **Total population** (World Bank)
7. **R&D researchers per million people** (Our World in Data)
8. **Adult literacy rate** (World Bank)

For Sections 2 to 6, the script draws trend plots, plots each indicator against cumulative publications, and computes the per-country Pearson correlation and a simple linear regression of cumulative publications on the indicator. Sections 7 and 8 merge their indicator and plot its trend.

Research expenditure as a share of GDP was not used, because reliable values were not available for 5 of the 6 countries.

## Status

This repository currently holds the data integration and exploratory analysis. Forecasting models are not included yet.

## Data

The input files are not stored in this repository. To run the script you need to supply:

| File | Content |
|---|---|
| `RESEARCH_AFRO_Rushad.csv` | Bibliographic dataset of physical activity research in Africa (country, year, title, study design) |
| `Africa_GDP.csv` | GDP by country and year |
| `Gender Inequality Index.csv` | UNDP Gender Inequality Index |
| `hdr-data.xlsx` | UNDP Human Development Report data (HDI) |
| `ihdidata.xlsx` | UNDP inequality-adjusted HDI |
| `API_SP.POP.TOTL_DS2_en_excel_v2_115066.xls` | World Bank total population |
| `API_SE.ADT.LITR.ZS_DS2_en_csv_v2_220.csv` | World Bank adult literacy rate |

Section 7 downloads R&D researchers per million people directly from Our World in Data, so it needs an internet connection.

## Running the analysis

Requires R with these packages: `dplyr`, `tidyr`, `readr`, `readxl`, `stringr`, `ggplot2`, `scales`, `broom` and `jsonlite`.

1. Put the input files above in the working directory.
2. Run `optimized pipeline.R` from top to bottom. The sections build on one shared data frame (`AfricaDataset`), and Sections 4 and 5 restrict it to years with valid index values, so the order matters.

## Author

Rushad Nihal, Arizona State University

## License

Released under the MIT License. See `LICENSE`.
