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
#NO fileupload
#setwd("G:/.shortcut-targets-by-id/1nGS-HGeaX1tlVpCzh-L9D4HzZAFszMzL/GNIP data filtering/fileupload_version")
# --- Helper Function: split_data_by_month ---
split_data_by_month <- function(df, x_var, y_var) {
  df <- as.data.frame(df)
  df <- df[order(df[[x_var]]), ]
  df$ym <- zoo::as.yearmon(df[[x_var]])
  new_df <- df[1, , drop = FALSE]
  for (i in 2:nrow(df)) {
    if (is.na(df$ym[i]) || is.na(df$ym[i-1])) {
      new_df <- rbind(new_df, df[i, , drop = FALSE])
    } else {
      diff_val <- df$ym[i] - df$ym[i-1]
      if (!is.na(diff_val) && diff_val > (1/12 + 1e-6)) {
        na_row <- df[i-1, , drop = FALSE]
        na_row[[y_var]] <- NA
        new_df <- rbind(new_df, na_row)
        new_df <- rbind(new_df, df[i, , drop = FALSE])
      } else {
        new_df <- rbind(new_df, df[i, , drop = FALSE])
      }
    }
  }
  new_df$ym <- NULL
  return(new_df)
}

### --- Global Data Loading and Preparation ---
#data_GNIP_raw <- read_excel("SLONIP_data.xlsx") %>%
data_GNIP_raw <- read_excel("SLOVEN_100_data.xlsx", sheet = "Sheet 1") %>%
  select(Site, Date3, Longitude, Latitude, Altitude, O18, H2) %>%
  rename(Station = Site, 
         d18O = O18, 
         d2H = H2, 
         Date = Date3) %>%
  filter(!(is.na(d18O) & is.na(d2H))) %>%
  mutate(
    Date = as.Date(Date),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date),
    D_excess = d2H - (8 * d18O)
  )

Altitude_min <- min(data_GNIP_raw$Altitude, na.rm = TRUE)

stations <- data_GNIP_raw %>%
  distinct(Station, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

coordinates <- st_coordinates(stations)
distance_matrix <- as.matrix(geosphere::distm(coordinates, fun = geosphere::distGeo)) / 1000

defaultStation <- sort(unique(data_GNIP_raw$Station))[1]

### --- UI ---
ui <- fluidPage(
  # Add MathJax for LaTeX rendering
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
  ),
  
  div(
    style = "display: flex; align-items: center; padding: 10px;",
    tags$img(
      src = "https://geochem.hu/logo/hunrencsfkfgilogo2.svg",
      height = "60px", style = "margin-right: 15px;"
    ),
    tags$h2("IsoQC - Screening Dashboard for Precipitation Stable Isotope Records", style = "color: #0A92FF; margin-left: 20px;")
  ),
  
  theme = bs_theme(bootswatch = "cerulean"),
  
  # --- Start of tabsetPanel for adding multiple tabs ---
  tabsetPanel(
    id = "main_tabs",
    
    # Existing tab for dashboard
    tabPanel("Dashboard", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("station", "Select Station:",
                                choices = sort(unique(data_GNIP_raw$Station)),
                                selected = defaultStation,
                                options = list(placeholder = "Type to search...")),
                 sliderInput("Xd", "Nearby Search Radius (km):", 
                             min = 0, max = 500, value = 100, step = 10),
                 numericInput("d18O_threshold", HTML("&delta;<sup>18</sup>O (‰) Threshold:"), value = 7.5),
                 numericInput("d2H_threshold", HTML("&delta;<sup>2</sup>H (‰) Threshold:"), value = 55),
                 numericInput("D_excess_threshold", "D-excess (‰) Threshold:", value = 19),
                 
                 tags$p(
                   "The default threshold values based on ",
                   a("Erdélyi et al. (2024)", href = "https://doi.org/10.17738/ajes.2024.0014", target = "_blank", 
                     style = "color: #0A92FF; text-decoration: none;"),
                   ".",
                   style = "font-size: 12px; font-style: italic; color: grey;"
                 ),
                 
                 numericInput("g_isoO", HTML("&delta;<sup>18</sup>O elevation correction (‰/km):"),
                              value = 1.2, step = 0.1),
                 numericInput("g_isoH", HTML("&delta;<sup>2</sup>H elevation correction (‰/km):"),
                              value = 7.9, step = 0.1),
                 tags$p(
                   "The default elevation correction values based on ",
                   a("Kern et al. (2020)", href = "https://doi.org/10.3390/w12061797", target = "_blank", 
                     style = "color: #0A92FF; text-decoration: none;"),
                   ".",
                   style = "font-size: 12px; font-style: italic; color: grey;"
                 ),
                 sliderInput("date_range", "Select Date Range:",
                             min = min(data_GNIP_raw$Date),
                             max = max(data_GNIP_raw$Date),
                             value = c(min(data_GNIP_raw$Date), max(data_GNIP_raw$Date)),
                             timeFormat = "%Y-%m"),
                 leafletOutput("map")
               ),
               
               mainPanel(
                 style = "padding-bottom: 80px;",
                 plotlyOutput("plot_O18"),
                 div(style = "height: 40px"),
                 plotlyOutput("plot_H2"),
                 div(style = "height: 40px"),
                 plotlyOutput("plot_D_excess"),
                 # --- New XY Plot added under the D-Excess plot ---
                 div(style = "height: 40px"),
                 plotlyOutput("plot_XY")
               )
             )
    ),
    
    # --- New Instructions tab added here ---
    tabPanel("Instructions",
             # You can use Rmarkdown or plain HTML to display instructions
             includeMarkdown("instructions.Rmd")  # Include a Markdown file with the instructions
    )
  ),
  # --- End of tabsetPanel for adding multiple tabs ---
  
  tags$footer(
    style = "width: 100%; text-align: center; padding: 10px; background-color: white; font-size: 12px; color: grey; border-top: 1px solid #ddd; position: relative; margin-top: 30px;",
    "© 2025 2ka Paleoklíma Lendület. All rights reserved. IsoQC v1.0.90"
  ),
  
  tags$head(
    tags$script(HTML("
      $(document).on('change', '#selected_station', function() {
        Shiny.setInputValue('selected_station_visible', $(this).is(':checked'), {priority: 'event'});
      });
      $(document).on('change', '#nearby_stations', function() {
        Shiny.setInputValue('nearby_stations_visible', $(this).is(':checked'), {priority: 'event'});
      });
      $(document).on('change', '#other_stations', function() {
        Shiny.setInputValue('other_stations_visible', $(this).is(':checked'), {priority: 'event'});
      });
      $(document).on('change', '#lines', function() {
        Shiny.setInputValue('lines_visible', $(this).is(':checked'), {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('updateSelectedStation', function(message) {
        document.getElementById('selected_station_text').innerText = message;
      });
    "))
  )
)

### --- Server ---
server <- function(input, output, session) {
  
  correctedData <- reactive({
    data_GNIP_raw %>%
      mutate(
        d18Oc = d18O + input$g_isoO * ((Altitude - Altitude_min) / 1000),
        d2Hc = d2H + input$g_isoH * ((Altitude - Altitude_min) / 1000)
      )
  })
  
  selected_station_data <- reactive({
    req(input$station != "")
    correctedData() %>% filter(Station == input$station)
  })
  
  selected_station <- reactive({
    req(input$station != "")
    stations %>% filter(Station == input$station)
  })
  
  nearby_stations <- reactive({
    req(input$station != "")
    station_index <- which(stations$Station == input$station)
    if (length(station_index) == 0) return(data.frame())
    nearby_indices <- which(distance_matrix[station_index, ] <= input$Xd)
    stations %>% filter(Station %in% stations$Station[nearby_indices])
  })
  
  other_stations <- reactive({
    req(input$station != "")
    sel <- selected_station()$Station
    near <- if (!is.null(nearby_stations())) nearby_stations()$Station else character(0)
    stations %>% filter(!(Station %in% c(sel, near)))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addControl(
        html = paste0('
          <div id="custom-control" style="background: white; padding: 0px; border:0px solid #ccc; border-radius: 2px;">
            <div>
              <input type="checkbox" id="selected_station" checked> 
              <label for="selected_station">
                <span style="background-color: #0A92FF; display:inline-block; width:12px; height:12px; margin-right:5px;"></span>
                <span id="selected_station_text">', defaultStation, '</span>
              </label>
            </div>
            <div>
              <input type="checkbox" id="nearby_stations" checked> 
              <label for="nearby_stations">
                <span style="background-color: #004782; display:inline-block; width:12px; height:12px; margin-right:5px;"></span> Nearby Stations
              </label>
            </div>
            <div>
              <input type="checkbox" id="other_stations" checked> 
              <label for="other_stations">
                <span style="background-color: #D3D3D3; display:inline-block; width:12px; height:12px; margin-right:5px;"></span> Other Stations
              </label>
            </div>
            <div>
              <input type="checkbox" id="lines" checked> 
              <label for="lines">
                <span style="background-color: black; display:inline-block; width:12px; height:12px; margin-right:5px;"></span> Distances
              </label>
            </div>
          </div>
        '),
        position = "topright"
      )
  })
  
  observe({
    req(input$station != "")
    sel <- selected_station()
    near <- nearby_stations()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() -> map
    
    if (!is.null(sel) && !is.null(near) && nrow(near) > 0) {
      sel_coords <- st_coordinates(sel)
      near_coords <- st_coordinates(near)
      
      for (i in 1:nrow(near)) {
        distance_km <- round(
          geosphere::distGeo(c(sel_coords[1], sel_coords[2]),
                             c(near_coords[i, 1], near_coords[i, 2])) / 1000, 2
        )
        map <- map %>% addPolylines(
          lat = c(sel_coords[2], near_coords[i, 2]),
          lng = c(sel_coords[1], near_coords[i, 1]),
          color = "black", weight = 1, opacity = 0.5,
          label = paste(distance_km, "km"),
          labelOptions = labelOptions(textsize = "12px", direction = "top", opacity = 0.8),
          group = "Distances"
        )
      }
    }
    
    map %>%
      addCircleMarkers(
        data = other_stations(),
        layerId = ~Station,
        label = ~Station,
        fillColor = "#D3D3D3", color = "black", weight = 1,
        fillOpacity = 0.7, opacity = 0.2, radius = 4,
        group = "Other Stations"
      ) %>%
      addCircleMarkers(
        data = near,
        layerId = ~Station,
        label = ~Station,
        fillColor = "#666666", color = "black", weight = 1,
        fillOpacity = 0.8, opacity = 0.2, radius = 5,
        group = "Nearby Stations"
      ) %>%
      addCircleMarkers(
        data = sel,
        layerId = ~Station,
        label = ~Station,
        fillColor = "black", color = "black", weight = 2,
        fillOpacity = 1, opacity = 1, radius = 6,
        group = "Selected Station"
      )
  })
  
  observeEvent(input$station, {
    req(input$station != "")
    st_data <- selected_station_data()
    updateDateRangeInput(session, "date_range",
                         start = min(st_data$Date, na.rm = TRUE),
                         end = max(st_data$Date, na.rm = TRUE))
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "station", selected = click$id)
    }
  })
  
  observe({
    req(input$station != "")
    session$sendCustomMessage("updateSelectedStation", input$station)
  })
  
  nearby_station_data <- reactive({
    req(input$station != "")
    station_index <- which(stations$Station == input$station)
    if (length(station_index) == 0) return(data.frame())
    nearby_indices <- which(distance_matrix[station_index, ] <= input$Xd)
    nearby_stations <- stations$Station[nearby_indices]
    nearby_stations <- setdiff(nearby_stations, input$station)
    correctedData() %>% filter(Station %in% nearby_stations)
  })
  
  filtered_data <- reactive({
    req(input$station != "")
    st_data <- selected_station_data() %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    near_data <- nearby_station_data() %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    near_means <- near_data %>%
      group_by(Date) %>%
      summarise(
        d18Oc_Mean_Nearby = mean(d18Oc, na.rm = TRUE),
        d2Hc_Mean_Nearby = mean(d2Hc, na.rm = TRUE),
        D_excess_Mean_Nearby = mean(D_excess, na.rm = TRUE),
        .groups = "drop"
      )
    merged <- left_join(st_data, near_means, by = "Date")
    merged %>% mutate(
      d18Oc_Diff = abs(d18Oc - d18Oc_Mean_Nearby),
      d2Hc_Diff = abs(d2Hc - d2Hc_Mean_Nearby),
      D_excess_Diff = abs(D_excess - D_excess_Mean_Nearby)
    )
  })
  
  generate_plot <- function(data, y_var, mean_var, diff_var, color, threshold) {
    data <- as.data.frame(data)
    data_selected_line <- split_data_by_month(data, "Date", y_var)
    data_mean_line <- split_data_by_month(data, "Date", mean_var)
    
    if (y_var == "d18Oc") {
      plot_title <- ""
      y_axis_title <- "\u03B4\u00B9\u2078O (‰)"
    } else if (y_var == "d2Hc") {
      plot_title <- ""
      y_axis_title <- "\u03B4\u00B2H (‰)"
    } else if (y_var == "D_excess") {
      plot_title <- ""
      y_axis_title <- "D-excess (‰)"
    } else {
     plot_title <- ""
      y_axis_title <- y_var
    }
    
    p <- plot_ly()
    
    nearby_data <- nearby_station_data() %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    
    if (!is.null(nearby_data) && nrow(nearby_data) > 0) {
      nearby_data <- as.data.frame(nearby_data)
      nearby_split <- split(nearby_data, nearby_data$Station)
      
      for (station_name in names(nearby_split)) {
        station_df <- as.data.frame(nearby_split[[station_name]])
        
        # Skip if the entire variable is NA
        if (all(is.na(station_df[[y_var]]))) next
        wrapped_name <- gsub("(.{17})", "\\1<br>", station_name)  # insert line break every 15 chars
        station_df_line <- split_data_by_month(station_df, "Date", y_var)
        
        p <- p %>% add_trace(
          data = station_df_line,
          x = station_df_line$Date,
          y = station_df_line[[y_var]],
          type = "scatter", mode = "lines+markers",
          line = list(color = "grey", width = 0.5),
          marker = list(size = 2, opacity = 0.7),
          name = station_name,
          connectgaps = FALSE
        )
      }
    }
    
    # Selected station trace
    p <- p %>% add_trace(
      data = data_selected_line,
      x = data_selected_line$Date,
      y = data_selected_line[[y_var]],
      type = "scatter", mode = "lines+markers",
      line = list(color = color, width = 2),
      marker = list(size = 5, color = color),
      name = gsub("(.{17})", "\\1<br>", input$station),
      connectgaps = FALSE
    ) %>%
      add_trace(
        data = data_mean_line,
        x = data_mean_line$Date,
        y = data_mean_line[[mean_var]],
        type = "scatter", mode = "lines+markers",
        line = list(color = "black", width = 2, dash = "dot"),
        marker = list(size = 4, color = "black"),
        name = "Nearby Mean",
        connectgaps = FALSE
      )
    
    # Bar plots for difference
    p <- p %>% add_bars(
      data = data,
      x = ~Date,
      y = as.formula(paste0("~", diff_var)),
      marker = list(color = "#808080"),
      name = paste(gsub("(.{17})", "\\1<br>", input$station), "Difference"),
      opacity = 0.5,
      width = 10
    )
    
    # Highlight bars above threshold
    threshold_value <- as.numeric(input[[threshold]])
    data_threshold <- data[data[[diff_var]] > threshold_value, ]
    if(nrow(data_threshold) > 0) {
      p <- p %>% add_bars(
        data = data_threshold,
        x = ~Date,
        y = as.formula(paste0("~", diff_var)),
        marker = list(color = "purple"),
        name = paste(gsub("(.{17})", "\\1<br>", input$station), "Above Threshold"),
        opacity = 0.7,
        width = 1000 * 60 * 60 * 24 * 12  # roughly 12 days in milliseconds
        
      )
    }
    
    # Final layout
    p <- p %>% layout(
      title = list(text = plot_title, x = 0, xanchor = "left", y = 0.95),
      yaxis = list(title = y_axis_title),
      xaxis = list(title = "Date", range = c(input$date_range[1], input$date_range[2]), tickformat = "%Y-%m"),
      barmode = "overlay"
    )
    
    p
  }
  
  
  output$plot_O18 <- renderPlotly({
    generate_plot(filtered_data(), "d18Oc", "d18Oc_Mean_Nearby", "d18Oc_Diff", "#0A92FF", "d18O_threshold")
  })
  
  output$plot_H2 <- renderPlotly({
    generate_plot(filtered_data(), "d2Hc", "d2Hc_Mean_Nearby", "d2Hc_Diff", "red", "d2H_threshold")
  })
  
  output$plot_D_excess <- renderPlotly({
    generate_plot(filtered_data(), "D_excess", "D_excess_Mean_Nearby", "D_excess_Diff", "green", "D_excess_threshold")
  })
  
  # --- New XY Plot using d18Oc (x-axis) and d2Hc (y-axis) ---
  # Points are marked purple if either the d18Oc_Diff or d2Hc_Diff is above its respective threshold.
  # A new trace is added before valid points showing all points in a light grey color with hover info.
  output$plot_XY <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Separate data into valid and problematic points
    data_problem <- data %>% filter(d18Oc_Diff > input$d18O_threshold | d2Hc_Diff > input$d2H_threshold)
    data_valid   <- data %>% filter(!(d18Oc_Diff > input$d18O_threshold | d2Hc_Diff > input$d2H_threshold))
    
    p <- plot_ly()
    
    # New trace: All points in a barely visible light grey color, now with hover info.
    p <- p %>% add_trace(
      data = data,
      x = ~d18Oc, y = ~d2Hc,
      type = "scatter", mode = "markers",
      marker = list(color = "#D3D3D3", size = 8),
      name = "Without neighbour",
      text = ~paste("Date:", Date, "<br>\u03B4\u00B9\u2078O (‰):", d18Oc, "<br>\u03B4\u00B2H (‰):", d2Hc),
      hoverinfo = "Without neighbour"
    )
    
    if(nrow(data_valid) > 0){
      p <- p %>% add_trace(
        data = data_valid, 
        x = ~d18Oc, y = ~d2Hc, 
        type = "scatter", mode = "markers",
        marker = list(color = "#808080", size = 8),
        text = ~paste("Date:", Date, "<br>\u03B4\u00B9\u2078O (‰):", d18Oc, "<br>\u03B4\u00B2H (‰):", d2Hc),
        hoverinfo = "Under Threshold",
        name = "Under Threshold"
      )
    }
    if(nrow(data_problem) > 0){
      p <- p %>% add_trace(
        data = data_problem, 
        x = ~d18Oc, y = ~d2Hc, 
        type = "scatter", mode = "markers",
        marker = list(color = "purple", size = 10),
        text = ~paste("Date:", Date, "<br>\u03B4\u00B9\u2078O (‰):", d18Oc, "<br>\u03B4\u00B2H (‰):", d2Hc, "<br>Below Threshold"),
        hoverinfo = "Below Threshold",
        name = "Below Threshold"
      )
    }
    
    p <- p %>% layout(
      title = list(text = "\u03B4\u00B2H vs \u03B4\u00B9\u2078O", x = 0, xanchor = "left"),
      xaxis = list(title = "\u03B4\u00B9\u2078O (‰)"),
      yaxis = list(title = "\u03B4\u00B2H (‰)")
    )
    
    p
  })
  
  observeEvent(input$selected_station_visible, {
    if (isTRUE(input$selected_station_visible))
      leafletProxy("map") %>% showGroup("Selected Station")
    else
      leafletProxy("map") %>% hideGroup("Selected Station")
  })
  
  observeEvent(input$nearby_stations_visible, {
    if (isTRUE(input$nearby_stations_visible))
      leafletProxy("map") %>% showGroup("Nearby Stations")
    else
      leafletProxy("map") %>% hideGroup("Nearby Stations")
  })
  
  observeEvent(input$other_stations_visible, {
    if (isTRUE(input$other_stations_visible))
      leafletProxy("map") %>% showGroup("Other Stations")
    else
      leafletProxy("map") %>% hideGroup("Other Stations")
  })
  
  observeEvent(input$lines_visible, {
    if (isTRUE(input$lines_visible))
      leafletProxy("map") %>% showGroup("Distances")
    else
      leafletProxy("map") %>% hideGroup("Distances")
  })
  
  observeEvent(input$Xd, {
    req(input$station != "")
    sel <- selected_station()
    near <- nearby_stations()
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() -> map
    
    if (!is.null(sel) && !is.null(near) && nrow(near) > 0) {
      sel_coords <- st_coordinates(sel)
      near_coords <- st_coordinates(near)
      
      for (i in 1:nrow(near)) {
        distance_km <- round(
          geosphere::distGeo(c(sel_coords[1], sel_coords[2]),
                             c(near_coords[i, 1], near_coords[i, 2])) / 1000, 2
        )
        map <- map %>% addPolylines(
          lat = c(sel_coords[2], near_coords[i, 2]),
          lng = c(sel_coords[1], near_coords[i, 1]),
          color = "black", weight = 1, opacity = 0.5,
          label = paste(distance_km, "km"),
          labelOptions = labelOptions(textsize = "12px", direction = "top", opacity = 0.8),
          group = "Distances"
        )
      }
    }
    
    map %>%
      addCircleMarkers(
        data = other_stations(),
        layerId = ~Station,
        label = ~Station,
        fillColor = "#D3D3D3", color = "black", weight = 1,
        fillOpacity = 0.7, opacity = 0.2, radius = 4,
        group = "Other Stations"
      ) %>%
      addCircleMarkers(
        data = near,
        layerId = ~Station,
        label = ~Station,
        fillColor = "#666666", color = "black", weight = 1,
        fillOpacity = 0.8, opacity = 0.2, radius = 5,
        group = "Nearby Stations"
      ) %>%
      addCircleMarkers(
        data = sel,
        layerId = ~Station,
        label = ~Station,
        fillColor = "black", color = "black", weight = 2,
        fillOpacity = 1, opacity = 1, radius = 6,
        group = "Selected Station"
      )
  })
  
  ## --- Zoom to station when a new station is selected ---
  observeEvent(input$station, {
    req(selected_station())
    coords <- st_coordinates(selected_station())
    leafletProxy("map") %>% setView(lng = coords[1], lat = coords[2], zoom = 7)
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "station", selected = click$id)
    }
  })
  
}

shinyApp(ui, server)
