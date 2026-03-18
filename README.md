
# Introduction
**IsoQC** is an interactive quality‑control dashboard designed for analysing stable isotope records (δ¹⁸O and δ²H) from precipitation samples.  
The app helps users:

- upload and inspect their own datasets,
- visualise temporal availability,
- compare a station’s measurements with nearby stations,
- identify potential outliers based on thresholds,
- export time‑series diagnostics.

This guide walks you through the complete usage of the application.

---

# 1. Uploading Your Data

## Supported file types
You may upload the following:

- **Excel files**: `.xlsx`, `.xls`
- **CSV files**: `.csv`
- Maximum file size: **500 MB**

## Required columns
Your uploaded dataset **must** contain:

- **Station** – station name or code  
- **Date** – observation date (`DD/MM/YYYY`, `YYYY-MM-DD`, etc.)  
- **Latitude** – decimal degrees  
- **Longitude** – decimal degrees  
- **Altitude** – metres above sea level  
- **δ¹⁸O** (‰)  
- **δ²H** (‰)

### Additional rules
- If using **Excel**, ensure that the Date column has proper date formatting.
- For **CSV**, you may manually specify:
  - delimiter (comma / semicolon / tab),
  - date format (any `lubridate`-compatible format, e.g. `%d/%m/%Y`).
- Rows with **both δ¹⁸O and δ²H missing** are automatically removed.
- The **first row must contain column names only** (no multi‑header files).

---

# 2. Column Mapping

After uploading, the app requests you to map the correct columns:

- Station name  
- Altitude  
- Latitude  
- Longitude  
- Date  
- δ¹⁸O  
- δ²H  

Once completed, click **"Upload Data"** to load your dataset into IsoQC.

A built‑in **test dataset** is also available via the *Use Slovenian test dataset from Hatvani et al. (2025)* button.

---

# 3. Data Preview and Availability

## Dataset preview
The app displays:

- a **raw preview** (first rows of your file),
- a **mapped preview** (only the required and selected columns).

## Station availability plot
You can visualise how many stations have data in a given period at:

- **Daily resolution**
- **Monthly resolution**
- **Yearly resolution**

This helps identify data gaps and temporal coverage issues.

---

# 4. Using the Dashboard

Once your dataset is imported, the **Dashboard** tab activates the core functionality.

## 4.1 Station selection
Choose a station from the searchable dropdown.  
The app will automatically:

- focus the map on the selected station,  
- refresh the date range bounds,  
- update all plots.

## 4.2 Nearby stations and thresholds
You can customise:

- **Search radius** (km) for selecting nearby stations
- Thresholds for:
  - δ¹⁸O  
  - δ²H  
  - D‑excess  
- Elevation‑correction parameters (default: Kern et al. 2020)

Nearby stations appear on the map:

- **Blue** → selected station  
- **Dark grey** → nearby stations within radius  
- **Light grey** → other stations  
- Black lines show station–neighbour distances.

---

# 5. Date Range Handling

IsoQC includes a date‑range controller:
 
- Direct **From** / **To** date inputs   

If data are unavailable in the chosen interval, the app warns you (e.g., no δ¹⁸O or no δ²H).

---

# 6. Time-Series Plots

IsoQC provides multiple diagnostic plots, including:

### **δ¹⁸O corrected**
### **δ²H corrected**
### **D‑excess**
### **δ²H vs. δ¹⁸O (XY plot)**

For each variable:

- The selected station is shown in **colour**.
- Nearby stations appear in **grey**.
- The nearby average appears as a **black dashed line**.
- Difference bars use an interpretative colour scheme:
  - **Light grey** → below threshold  
  - **Dark grey** → below threshold but above 2×SD  
  - **Purple** → above threshold  
  - **Purple with black outline** → strongly suspicious (above threshold & above 2×SD)

The XY plot distinguishes:

- All points  
- Points under thresholds  
- Points above thresholds

---

# 7. Exporting Time-Series Data

Use the **Download mean time series** button to export a `.csv` containing:

- Selected station values  
- Nearby mean values  
- Differences  
- Date range metadata  
- Thresholds  
- Elevation correction parameters  
- Search radius  

The export includes UTF‑8 encoding with BOM for Excel compatibility.

---

# 8. Filling Missing Months/Days (Optional)

If the dataset contains gaps for a specific month/day, you may:

1. Insert a **dummy numeric value** for that month/day in your original file.
2. Upload the updated file again.

IsoQC will compute an average for that month/day, which can be used as an estimated replacement.

---

# 9. Data Privacy Notice

The IsoQC app **does not store any user-uploaded datasets**.

Uploaded files are:

- processed in memory for the active session,
- used only for temporary on-screen analysis,
- released when the session ends.

To support maintenance and basic usage statistics, the app stores a limited set of **server-side usage metrics**. These metrics may include:

- session start time,
- session end time,
- session duration,
- the active tab viewed in the app,

These usage metrics are used only for app maintenance, stability monitoring, and aggregated usage analysis.

---

# Need help?
If you encounter any unexpected behaviour, refer to:

- the **Changelog** for known updates and fixes,
- contact the developer: <a href="https://github.com/erdelyidani/IsoQC" target="_blank">
  IsoQC GitHub repository
</a>

Thank you for using **IsoQC**!
