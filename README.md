# crime-rates-r-shiny-viz
## Description
A narrative visualisation of crime trends in NSW using R Shiny. Please see the documentation PDF file for the user guide of the R Shiny application.

The raw data sources used for the project were:
1. NSW Recorded Crime by Offence: Monthly data on all criminal incidents recorded by police; geographic breakdown by LGA. NSW Bureau of Crime Statistics and Research.  Retrieved from  
	https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx
2. NSW Local Government Areas - PSMA Administrative Boundaries. Department of Industry, Innovation and Science.  Retrieved from 
https://data.gov.au/data/dataset/f6a00643-1842-48cd-9c2f-df23a3a1dc1e
3. ERP by LGA (ASGS 2017), 2001 to 2017. Australian Bureau of Statistics.  Retrieved from http://stat.data.abs.gov.au/Index.aspx?DataSetCode=ABS_ERP_LGA2017
4. Local government areas of New South Wales by Region. Wikipedia.  Retrieved from  https://en.wikipedia.org/wiki/Local_government_areas_of_New_South_Wales

Significant data wrangling which included cleaning, merging datasets, data aggregation and data reshaping was required for each of these visualisations. The data wrangling for the project was done in R. The transformed datasets used in the application are as follows:
1. Annual Crime Rate in NSW.csv
2. LGA-Region Mapping.csv
3. Crime Rate by Region Recent Trends.csv
4. Crime Rate by LGA Recent Trends.csv
5. Offence Counts.csv
6. Offence Rates.csv
7. NSW_LGA_POLYGON_shp (LGAs shapefile)
8. Exploratory_Tab_Data.csv

## R Packages required
* ggplot2
* shiny
* ggvis
* mapview
* leaflet
* leafpop
* rgdal
* RColorBrewer
* scales
* sf
* terra
