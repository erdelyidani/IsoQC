# IsoQC
IsoQC is an outlier detection app specialized for observations of isotopes in precipitation.

Instructions
The IsoQC app (app.r) allows users to upload their own datasets to analyze stable isotope records from precipitation samples. The data upload module supports MS Excel (.xlsx, .xls) and CSV (.csv) with a maximum file size limit of 500 MB. To ensure proper functionality, the uploaded data must contain specific columns: Station (measurement station name or identifier), Date (observation date in a standard format such as DD/MM/YYYY or DD-MM-YYYY), Longitude and Latitude (geographic coordinates in decimal degrees), Altitude, d18O (‰) and d2H (‰). Date values should be properly formatted in the case of MS Excel files, and if necessary, users can manually specify the date format before finalizing the upload in the case of CSV files. Longitude and latitude must be within valid geographic ranges, and missing values are allowed. The file should not contain extra header rows, with only the first row dedicated to column names.
A test file (data.xlsx) is also uploaded.
