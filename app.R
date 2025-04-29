# app.R

library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(geosphere)
library(sf)
library(lubridate)
library(tidyr)
library(zoo)
library(leaflet)
library(bslib)
library(markdown)
#FILEUPLOAD
# --- Helper: wrap long legend names every 16 chars with newline ---
wrap_name <- function(x) {
  gsub("(.{17})", "\\1\n", x)
}

# --- Helper: split data by month, inserting gaps ---
split_data_by_month <- function(df, x_var, y_var) {
  df <- as.data.frame(df)[order(df[[x_var]]), ]
  df$ym <- zoo::as.yearmon(df[[x_var]])
  out <- df[1, , drop = FALSE]
  for (i in 2:nrow(df)) {
    if (is.na(df$ym[i]) || is.na(df$ym[i-1])) {
      out <- rbind(out, df[i, , drop = FALSE])
    } else {
      d <- df$ym[i] - df$ym[i-1]
      if (!is.na(d) && d > (1/12 + 1e-6)) {
        gap <- df[i-1, , drop = FALSE]
        gap[[y_var]] <- NA
        out <- rbind(out, gap, df[i, , drop = FALSE])
      } else {
        out <- rbind(out, df[i, , drop = FALSE])
      }
    }
  }
  out$ym <- NULL
  out
}

#empty df
data_GNIP_raw <- reactiveVal(NULL)

# --- Build reactive helpers object ---
rebuild_helpers <- function(df) {
  minAlt <- min(df$Altitude, na.rm = TRUE)
  sts <- df %>%
    distinct(Station, Latitude, Longitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  coords <- st_coordinates(sts)
  sts$lon <- coords[,1]
  sts$lat <- coords[,2]
  dm <- as.matrix(distm(coords, fun = distGeo)) / 1000
  list(
    Altitude_min    = minAlt,
    stations        = sts,
    distance_matrix = dm
  )
}
helpers <- reactive({ rebuild_helpers(data_GNIP_raw()) })


# --- UI ---
ui <- fluidPage(
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")),
  div(
    style = "display:flex;align-items:center;padding:10px;",
    tags$img(src = "https://geochem.hu/logo/hunrencsfkfgilogo2.svg",
             height = "60px", style = "margin-right:15px;"),
    tags$h2("IsoQC - Screening Dashboard for Precipitation Stable Isotope Records",
            style = "color:#0A92FF;margin-left:20px;")
  ),
  theme = bs_theme(bootswatch = "cerulean"),
  
  tabsetPanel(id = "main_tabs",
              
              tabPanel("Dataset", sidebarLayout(
                sidebarPanel(
                  fileInput("file", "Upload Data (.xlsx, .xls or .csv)"),
                  
                  # 1) CSV‐only controls (delimiter & date-format)
                  uiOutput("csv_controls"),
                  
                  # 2) Column‐mapping + Import button
                  uiOutput("column_mapping_ui"),
                  
                  # always show the test-dataset link
                  tags$p(
                    em(actionLink("load_test_data",
                                  "Use Slovenian test dataset from Hatvani et al. (2025)")),
                    style = "margin-top:5px;font-size:90%;"
                  )
                )
                
                ,
                mainPanel(
                  h5("Preview (first 10 rows)"),
                  tableOutput("preview_table")
                )
              )),
              
              tabPanel("Dashboard", sidebarLayout(
                sidebarPanel(
                  selectizeInput("station", "Select Station:", choices = NULL, options = list(placeholder = "Type to search...")),
                  sliderInput("Xd", "Nearby Search Radius (km):", min = 0, max = 500, value = 90, step = 10),
                  numericInput("d18O_threshold", HTML("&delta;<sup>18</sup>O Threshold (‰):"), value = 6.5),
                  numericInput("d2H_threshold",   HTML("&delta;<sup>2</sup>H Threshold (‰):"), value = 52),
                  numericInput("D_excess_threshold", "D‑excess Threshold (‰):", value = 19),
                  tags$p("Default thresholds from ",
                         a("Erdélyi et al. (2024)", href="https://doi.org/10.17738/ajes.2024.0014", target="_blank", style="color:#0A92FF;"),
                         ".", style="font-size:12px;font-style:italic;color:grey;"),
                  numericInput("g_isoO", HTML("&delta;<sup>18</sup>O elev. corr. (‰/km):"), value = 1.2, step = 0.1),
                  numericInput("g_isoH", HTML("&delta;<sup>2</sup>H elev. corr. (‰/km):"), value = 7.9, step = 0.1),
                  sliderInput("date_range", "Select Date Range:",
                              min = Sys.Date()-365, max = Sys.Date(),
                              value = c(Sys.Date()-365, Sys.Date()), timeFormat = "%d-%m-%Y",
                              step= 30)
                  
                  ,
                  dateInput("date_min", "From:", value = Sys.Date(), format = "dd-mm-yyyy", startview = "year"),
                  dateInput("date_max", "To:", value = Sys.Date(), format = "dd-mm-yyyy", startview = "year"),
                  leafletOutput("map")
                ),
                mainPanel(
                  plotlyOutput("plot_O18"), div(style = "height:40px;"),
                  plotlyOutput("plot_H2"),  div(style = "height:40px;"),
                  plotlyOutput("plot_D_excess"), div(style = "height:40px;"),
                  div(style = "height:40px;"),
                  plotlyOutput("plot_XY")
                )
              )),
              
              tabPanel("Instructions", includeMarkdown("instructions.Rmd"))
  ),
  
  tags$footer(
    style = "width:100%;text-align:center;padding:10px;background:white;
             font-size:12px;color:grey;border-top:1px solid #ddd;margin-top:30px;",
    "© 2025 2ka Paleoklíma Lendület. All rights reserved. IsoQC v1.0.96"
  )
)

# --- Server logic ---
server <- function(input, output, session) {
  # Column‐mapping UI
  # 1) Drive the CSV-vs-Excel inputs + Upload button
  output$csv_controls <- renderUI({
    req(input$file)  
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext == "csv") {
      tagList(
        radioButtons("csv_delim", "Delimiter:",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        textInput("date_format", "Date format (lubridate):", value = "%d/%m/%Y")
      )
    }
  })
  output$column_mapping_ui <- renderUI({
    req(input$file)                # must have a file
    ext <- tolower(tools::file_ext(input$file$name))
    
    if (ext == "csv") {
      # for CSV, require both csv_delim and date_format before mapping
      req(input$csv_delim, input$date_format)
    }
    # for Excel, no extra req beyond input$file
    
    # read just the header to get column names
    cols <- switch(ext,
                   csv  = names(read.csv(input$file$datapath,
                                         sep = input$csv_delim, nrows = 1)),
                   xlsx = names(read_excel(input$file$datapath, n_max = 0)),
                   xls  = names(read_excel(input$file$datapath, n_max = 0))
    )
    cols <- make.unique(cols)
    
    # emit mapping selectors AND the Import button
    tagList(
      selectInput("map_station", "Station name column:", cols),
      selectInput("map_alt",     "Altitude column:",      cols,
                  selected = grep("Alt", cols, ignore.case=TRUE, value=TRUE)[1]),
      selectInput("map_lat",     "Latitude column:",      cols,
                  selected = grep("Lat", cols, ignore.case=TRUE, value=TRUE)[1]),
      selectInput("map_lon",     "Longitude column:",     cols,
                  selected = grep("Lon", cols, ignore.case=TRUE, value=TRUE)[1]),
      selectInput("map_date",    "Date column:",          cols,
                  selected = grep("Date",cols,ignore.case=TRUE,value=TRUE)[1]),
      selectInput("map_O18",     HTML("&delta;<sup>18</sup>O (‰) column:"), cols,
                  selected = grep("O18", cols, ignore.case=TRUE, value=TRUE)[1]),
      selectInput("map_H2",      HTML("&delta;<sup>2</sup>H (‰) column:"),  cols,
                  selected = grep("H2",  cols, ignore.case=TRUE, value=TRUE)[1]),
      
      # only now does Upload Data appear
      actionButton("import_data", "Upload Data", icon = icon("upload"))
    )
  })
  
  # Preview
  output$preview_table <- renderTable({
    req(input$file, input$map_station)
    ext <- tolower(tools::file_ext(input$file$name))
    raw <- switch(ext,
                  "csv"  = read.csv(input$file$datapath, sep = input$csv_delim, stringsAsFactors = FALSE),
                  "xlsx" = read_excel(input$file$datapath),
                  "xls"  = read_excel(input$file$datapath)
    )
    names(raw) <- make.unique(names(raw))
    raw %>%
      rename(
        Station   = !!sym(input$map_station),
        Altitude  = !!sym(input$map_alt),
        Latitude  = !!sym(input$map_lat),
        Longitude = !!sym(input$map_lon),
        Date      = !!sym(input$map_date),
        d18O      = !!sym(input$map_O18),
        d2H       = !!sym(input$map_H2)
      ) %>%
      mutate(
        # parse into Date class for CSV, coerce for Excel
        Date = if (ext == "csv") {
          as.Date(parse_date_time(Date, orders = input$date_format))
        } else {
          as.Date(Date)
        },
        # now format as dd/mm/YYYY for display
        Date = format(Date, "%d/%m/%Y")
      ) %>%
      head(10)
  })
  
  
  # 2) Import data into the main reactive
  observeEvent(input$import_data, {
    req(input$file, input$map_station)
    ext <- tolower(tools::file_ext(input$file$name))
    raw <- switch(ext,
                  "csv"  = read.csv(input$file$datapath, sep = input$csv_delim, stringsAsFactors = FALSE),
                  "xlsx" = read_excel(input$file$datapath),
                  "xls"  = read_excel(input$file$datapath)
    )
    names(raw) <- make.unique(names(raw))
    df <- raw %>%
      rename(
        Station  = !!sym(input$map_station),
        Altitude = !!sym(input$map_alt),
        Latitude = !!sym(input$map_lat),
        Longitude= !!sym(input$map_lon),
        Date     = !!sym(input$map_date),
        d18O     = !!sym(input$map_O18),
        d2H      = !!sym(input$map_H2)
      ) %>%
      mutate(
        Date = if (ext == "csv") {
          parsed <- parse_date_time(Date, orders = input$date_format)
          as.Date(parsed)
        } else {
          as.Date(Date)
        },
        D_excess = d2H - 8 * d18O
      ) %>%
      select(Station, Date, Longitude, Latitude, Altitude, d18O, d2H, D_excess)
    
    data_GNIP_raw(df)
    
    # then your existing code to update inputs…
    dr <- range(df$Date)
    updateSelectizeInput(session, "station",
                         choices = sort(unique(df$Station)),
                         selected = sort(unique(df$Station))[1])
    updateSliderInput(
      session, "date_range",
      min        = dr[1],
      max        = dr[2],
      value      = dr,
      timeFormat = "%d-%m-%Y"
    )

    updateDateInput(session, "date_min", value = dr[1])
    updateDateInput(session, "date_max", value = dr[2])
    updateTabsetPanel(session, "main_tabs", selected = "Dashboard")
  })
  observeEvent(input$load_test_data, {
    # 1) Read the Excel test file
    df_test <- read_excel("data.xlsx", sheet = "Sheet 1") %>%
      select(Site, Date, Longitude, Latitude, Altitude, O18, H2) %>%
      rename(
        Station = Site,
        d18O    = O18,
        d2H     = H2
      ) %>%
      filter(!(is.na(d18O) & is.na(d2H))) %>%
      mutate(
        Date     = as.Date(Date),
        D_excess = d2H - 8 * d18O
      ) %>%
      select(Station, Date, Longitude, Latitude, Altitude, d18O, d2H, D_excess)
    
    # 2) Push it into your reactiveVal
    data_GNIP_raw(df_test)
    
    #compute safe date‐range
    dates <- df_test$Date
    good  <- dates[!is.na(dates)]
    dr <- if (length(good)) range(good) else c(Sys.Date(), Sys.Date())
    
    # update station selector
    updateSelectizeInput(session, "station",
                         choices  = sort(unique(df_test$Station)),
                         selected = sort(unique(df_test$Station))[1])
    
    # update date‐range slider + dateInputs
    updateSliderInput(
      session, "date_range",
      min        = dr[1],
      max        = dr[2],
      value      = dr,
      timeFormat = "%d-%m-%Y"
    )
    updateDateInput(session, "date_min", value = dr[1])
    updateDateInput(session, "date_max", value = dr[2])
    
    # finally switch to Dashboard
    updateTabsetPanel(session, "main_tabs", selected = "Dashboard")
  })
  
  
  # Sync slider ↔ dateInputs
  observeEvent(input$date_range, {
    updateDateInput(session, "date_min", value = as.Date(input$date_range[1]))
    updateDateInput(session, "date_max", value = as.Date(input$date_range[2]))
  }, ignoreInit = TRUE)
  observeEvent(c(input$date_min, input$date_max), {
    a <- input$date_min; b <- input$date_max
    updateSliderInput(session, "date_range",
                      value      = c(min(a, b), max(a, b)),
                      timeFormat = "%d-%m-%Y"
    )
  }, ignoreInit = TRUE)
  
  
  # When the user clicks a station on the map, update the station dropdown
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)  # make sure there's an ID
    updateSelectizeInput(session, "station", selected = click$id)
    
    # (optional) pan/zoom the map to the clicked station
    leafletProxy("map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 8)
  })
  
  # Reactive data transforms
  correctedData <- reactive({
    df <- data_GNIP_raw()
    df %>% mutate(
      d18Oc = d18O + input$g_isoO * ((Altitude - helpers()$Altitude_min)/1000),
      d2Hc  = d2H  + input$g_isoH * ((Altitude - helpers()$Altitude_min)/1000)
    )
  })
  selected_station_data <- reactive({
    req(input$station)
    correctedData() %>% filter(Station == input$station)
  })
  nearby_station_data <- reactive({
    req(input$station)
    stns <- helpers()$stations$Station
    idx  <- which(stns == input$station)
    nei  <- which(helpers()$distance_matrix[idx, ] <= input$Xd)
    correctedData() %>% filter(Station %in% stns[nei] & Station != input$station)
  })
  filtered_data <- reactive({
    req(input$station)
    dr <- as.Date(input$date_range)
    sel <- selected_station_data() %>% filter(Date >= dr[1] & Date <= dr[2])
    near <- nearby_station_data()  %>% filter(Date >= dr[1] & Date <= dr[2])
    means <- near %>% group_by(Date) %>% summarize(
      d18Oc_Mean    = mean(d18Oc,   na.rm = TRUE),
      d2Hc_Mean     = mean(d2Hc,    na.rm = TRUE),
      D_excess_Mean = mean(D_excess,na.rm = TRUE), .groups="drop"
    )
    left_join(sel, means, by = "Date") %>% mutate(
      d18Oc_Diff    = abs(d18Oc    - d18Oc_Mean),
      d2Hc_Diff     = abs(d2Hc     - d2Hc_Mean),
      D_excess_Diff = abs(D_excess - D_excess_Mean)
    )
  })
  
  # Map with initial distances
  output$map <- renderLeaflet({
    req(data_GNIP_raw(), input$station)
    sts <- helpers()$stations
    leaflet(sts) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        layerId = ~Station,
        radius = 5,
        fillColor   = "#D3D3D3",  # other stations
        fillOpacity = 0.7,
        stroke      = FALSE,
        label       = ~Station,
        labelOptions = labelOptions(direction = "auto", textsize = "12px")
      ) %>%
      setView(
        lng = sts$lon[1],
        lat = sts$lat[1], zoom = 6
      )
  })
  
  observe({
    req(input$station)
    all_sts <- helpers()$stations
    sel   <- all_sts %>% filter(Station == input$station)
    idx   <- which(all_sts$Station == input$station)
    near_idx <- which(helpers()$distance_matrix[idx, ] <= input$Xd)
    near_sts <- all_sts[near_idx, ] %>% filter(Station != input$station)
    other_sts <- all_sts %>% filter(!Station %in% c(input$station, near_sts$Station))
    
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    sc <- st_coordinates(sel)
    for (i in seq_len(nrow(near_sts))) {
      nc  <- st_coordinates(near_sts[i, ])
      dkm <- round(geosphere::distGeo(sc, nc)/1000, 2)
      proxy <- proxy %>% addPolylines(
        lng = c(sc[1], nc[1]), lat = c(sc[2], nc[2]),
        color = "black", weight = 1, opacity = 0.5,
        label = paste0(dkm, " km"),
        labelOptions = labelOptions(direction = "auto", textsize = "12px")
      )
    }
    
    proxy %>%
      addCircleMarkers(
        data = other_sts, lng = ~lon, lat = ~lat,
        layerId = ~Station, radius = 5,
        fillColor   = "#D3D3D3", fillOpacity = 0.7, stroke = FALSE,
        label = ~Station, labelOptions = labelOptions(direction = "auto")
      ) %>%
      addCircleMarkers(
        data = near_sts, lng = ~lon, lat = ~lat,
        layerId = ~Station, radius = 6,
        fillColor   = "#666666", fillOpacity = 0.8, stroke = FALSE,
        label = ~Station, labelOptions = labelOptions(direction = "auto")
      ) %>%
      addCircleMarkers(
        data = sel, lng = ~lon, lat = ~lat,
        layerId = ~Station, radius = 7,
        fillColor   = "#0A92FF", fillOpacity = 1, stroke = FALSE,
        label = ~Station, labelOptions = labelOptions(direction = "auto")
      )
  })
  
  # Time‐series plots
  generate_plot <- function(y_var, mean_var, diff_var, color, thr_input) {
    fd <- filtered_data()
    nd <- nearby_station_data() %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    p <- plot_ly()
    for (stn in sort(unique(nd$Station))) {
      sub <- nd %>% filter(Station == stn)
      sl  <- split_data_by_month(sub, "Date", y_var)
      if (!all(is.na(sl[[y_var]]))) {
        p <- p %>% add_trace(
          data=sl, x=~Date, y=as.formula(paste0("~",y_var)),
          type="scatter", mode="lines+markers",
          line=list(color="grey",width=0.5),
          marker=list(size=2,opacity=0.7),
          name=stn
        )
      }
    }
    sel_l <- split_data_by_month(fd, "Date", y_var)
    p <- p %>% add_trace(
      data=sel_l, x=~Date, y=sel_l[[y_var]],
      type="scatter", mode="lines+markers",
      line=list(color=color,width=2),
      marker=list(size=5,color=color),
      name=input$station
    )
    mean_l <- split_data_by_month(fd, "Date", mean_var)
    p <- p %>% add_trace(
      data=mean_l, x=~Date, y=mean_l[[mean_var]],
      type="scatter", mode="lines+markers",
      line=list(color="black",width=2,dash="dot"),
      marker=list(size=4,color="black"),
      name="Nearby Mean"
    )
    p <- p %>% add_bars(
      data=fd, x=~Date, y=as.formula(paste0("~",diff_var)),
      marker=list(color="#808080"),
      name=paste(input$station,"Difference"),
      opacity=0.5, width=1000*60*60*24*20
    )
    thr <- as.numeric(input[[thr_input]])
    ov <- fd %>% filter(.data[[diff_var]]>thr)
    if(nrow(ov)>0) {
      p <- p %>% add_bars(
        data=ov, x=~Date, y=as.formula(paste0("~",diff_var)),
        marker=list(color="purple"),
        name=wrap_name(paste(input$station,"Above threshold")),
        opacity=0.7, width=1000*60*60*24*20
      )
    }
    p %>% layout(
      barmode="overlay",
      legend=list(font=list(size=9), x=1.02,y=1),
      xaxis=list(tickformat="%d-%m-%Y"),
      yaxis=list(title=switch(y_var,
                              "d18Oc"="δ¹⁸O (‰)",
                              "d2Hc" ="δ²H (‰)",
                              "D_excess"="D‑excess (‰)"
      ))
    )
  }
  
  output$plot_O18 <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    generate_plot("d18Oc", "d18Oc_Mean", "d18Oc_Diff", "#0A92FF", "d18O_threshold")
  })
  
  output$plot_H2  <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    generate_plot("d2Hc",  "d2Hc_Mean",  "d2Hc_Diff",  "red",   "d2H_threshold")
  })
  
  output$plot_D_excess <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    generate_plot("D_excess", "D_excess_Mean", "D_excess_Diff", "green", "D_excess_threshold")
  })
  
  output$plot_XY <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    df    <- filtered_data()
    valid <- df %>% filter(d18Oc_Diff <= input$d18O_threshold & d2Hc_Diff <= input$d2H_threshold)
    prob  <- df %>% filter(d18Oc_Diff >  input$d18O_threshold | d2Hc_Diff > input$d2H_threshold)
    plot_ly() %>%
      add_trace(data=df,    x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="#D3D3D3",size=8), name="All points") %>%
      add_trace(data=valid, x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="#666666",size=8), name="Under threshold") %>%
      add_trace(data=prob,  x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="purple",size=10),
                name=wrap_name(paste(input$station,"Above threshold"))) %>%
      layout(
        xaxis=list(title="δ¹⁸O (‰)"), yaxis=list(title="δ²H (‰)"),
        legend=list(font=list(size=9), x=1.02,y=1)
      )
  })
}

shinyApp(ui, server)
