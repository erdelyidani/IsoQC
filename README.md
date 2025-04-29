# IsoQC
IsoQC is an outlier detection app specialized for observations of isotopes in precipitation.

Instructions
The IsoQC app (app.r) allows users to upload their own datasets to analyze stable isotope records from precipitation samples. The data upload module supports MS Excel (.xlsx, .xls) and CSV (.csv) with a maximum file size limit of 500 MB. To ensure proper functionality, the uploaded data must contain specific columns: Station (measurement station name or identifier), Date (observation date in a standard format such as DD/MM/YYYY or DD-MM-YYYY), Longitude and Latitude (geographic coordinates in decimal degrees), Altitude, d18O (‰) and d2H (‰). Date values should be properly formatted in the case of MS Excel files, and if necessary, users can manually specify the date format before finalizing the upload in the case of CSV files. Longitude and latitude must be within valid geographic ranges, and missing values are allowed. The file should not contain extra header rows, with only the first row dedicated to column names.
A test file (data.xlsx) is also uploaded.

Technical details:
RStudio version used: 2023.03.0+386

Library versions:

shiny v1.7.5.1
plotly v4.10.3
readxl v1.4.3
dplyr v1.1.3
geosphere v1.5-18
sf v1.0-14
lubridate v1.9.3
tidyr v1.3.0
zoo v1.8-12
leaflet 2.2.0
bslib v0.5.1
markdown v1.13

Live version:
https://erdelyidani.shinyapps.io/IsoQC/
