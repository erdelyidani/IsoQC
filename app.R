#IsoQC v2.4
app_version <- "v2.4"
citation_text <- "Hatvani, I.G., Erdélyi, D., Vreča, P., Lojen, S., Žagar, K., Gačnik, J., Kern, Z., 2026. Online screening tool for precipitation stable isotopes records: hybrid distance / density based outlier filtering approach via interactive web application. Journal of Hydrology 672: 135401. DOI:https://doi.org/10.1016/j.jhydrol.2026.135401."
citation_url <- "https://doi.org/10.1016/j.jhydrol.2026.135401"
read_wordmark_part <- function(path) {
  if (file.exists(path)) {
    base64enc::dataURI(file = path, mime = "image/png")
  } else {
    NULL
  }
}

wordmark_src1 <- read_wordmark_part("www/isoqc_wordmark_raster1.png")
wordmark_src2 <- read_wordmark_part("www/isoqc_wordmark_raster2.png")
library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(geosphere)
library(lubridate)
library(tidyr)
library(zoo)
library(leaflet)
library(bslib)
library(markdown)
library(DBI)
library(RSQLite)
library(jsonlite)
#FILEUPLOAD
#setwd("C:/Users/Daniel/My Drive/Erdélyi Dániel/Doktori/Alapadathalmaz/Europe/2025_05_08/")
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

safe_date_range <- function(x) {
  x <- as.Date(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    rep(Sys.Date(), 2)
  } else {
    range(x)
  }
}

clamp_date_range <- function(x, bounds) {
  x <- sort(as.Date(x))
  bounds <- sort(as.Date(bounds))
  x[1] <- max(x[1], bounds[1])
  x[2] <- min(x[2], bounds[2])
  if (x[1] > x[2]) bounds else x
}

#empty df
data_GNIP_raw <- reactiveVal(NULL)
# --- Build reactive helpers object ---
rebuild_helpers <- function(df) {
  sts <- df %>%
    distinct(Station, Latitude, Longitude) %>%
    filter(!is.na(Latitude), !is.na(Longitude)) %>%
    transmute(Station, lon = Longitude, lat = Latitude)
  list(
    stations = sts,
    coords   = as.matrix(sts[, c("lon", "lat")])
  )
}
helpers <- reactive({ rebuild_helpers(data_GNIP_raw()) })

metrics_dir <- "logs"
if (!dir.exists(metrics_dir)) {
  dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
}
metrics_db_path <- file.path(metrics_dir, "isoqc_usage.sqlite")
normalize_country_value <- function(x) {
  x <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
  if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  trimws(x)
}

set_session_client_context <- function(session, country = NULL, source = NULL) {
  current_country <- normalize_country_value(tryCatch(session$userData$client_country, error = function(e) NULL))
  incoming_country <- normalize_country_value(country)

  if (!is.na(incoming_country) && nzchar(incoming_country)) {
    current_country <- incoming_country
  }

  if (!is.null(source) && length(source) && !is.na(source) && nzchar(as.character(source))) {
    session$userData$client_country_source <- as.character(source)[1]
  }

  session$userData$client_country <- current_country

  invisible(list(
    client_country = current_country,
    client_country_source = tryCatch(session$userData$client_country_source, error = function(e) NA_character_)
  ))
}

backfill_session_client_context <- function(session) {
  tryCatch({
    client_country <- normalize_country_value(tryCatch(session$userData$client_country, error = function(e) NULL))
    if (is.na(client_country) || !nzchar(client_country)) {
      return(invisible(FALSE))
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), metrics_db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")

    DBI::dbExecute(
      con,
      "UPDATE usage_events
         SET client_country = ?
       WHERE session_token = ?
         AND (client_country IS NULL OR trim(client_country) = '')",
      params = list(
        as.character(client_country),
        as.character(session$token)
      )
    )

    cols <- tryCatch(DBI::dbGetQuery(con, "PRAGMA table_info(usage_events)"), error = function(e) data.frame(name = character()))
    col_names <- cols$name %||% character()

    if ("client_ip" %in% col_names) {
      DBI::dbExecute(
        con,
        "UPDATE usage_events
           SET client_ip = NULL
         WHERE session_token = ?
           AND client_ip IS NOT NULL
           AND trim(client_ip) <> ''",
        params = list(as.character(session$token))
      )
    }

    invisible(TRUE)
  }, error = function(e) {
    warning(sprintf("Usage metrics backfill failed: %s", conditionMessage(e)), call. = FALSE)
    invisible(FALSE)
  })
}

get_input_value_safe <- function(input, name) {
  value <- tryCatch(input[[name]], error = function(e) NULL)
  if (is.null(value) || !length(value)) return(NULL)

  if (inherits(value, c("Date", "POSIXct", "POSIXt"))) {
    return(as.character(value))
  }

  if (is.atomic(value) && length(value) <= 2) {
    return(ifelse(is.na(value), NA, value))
  }

  if (is.atomic(value)) {
    return(as.vector(value))
  }

  NULL
}

build_current_params_snapshot <- function(input) {
  list(
    main_tab = get_input_value_safe(input, "main_tabs"),
    station = get_input_value_safe(input, "station"),
    search_radius_km = get_input_value_safe(input, "Xd"),
    d18O_threshold = get_input_value_safe(input, "d18O_threshold"),
    d2H_threshold = get_input_value_safe(input, "d2H_threshold"),
    D_excess_threshold = get_input_value_safe(input, "D_excess_threshold"),
    g_isoO = get_input_value_safe(input, "g_isoO"),
    g_isoH = get_input_value_safe(input, "g_isoH"),
    date_min = get_input_value_safe(input, "date_min"),
    date_max = get_input_value_safe(input, "date_max"),
    date_range = get_input_value_safe(input, "date_range"),
    basemap = get_input_value_safe(input, "basemap"),
    resolution = get_input_value_safe(input, "resolution")
  )
}

serialize_log_details <- function(input = NULL, details = NULL) {
  payload <- list()

  if (!is.null(input)) {
    payload$params <- build_current_params_snapshot(input)
  }

  if (!is.null(details)) {
    payload$event_details <- details
  }

  if (!length(payload)) return(NA_character_)

  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

init_metrics_db <- function() {
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), metrics_db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS usage_events (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         event_ts_utc TEXT NOT NULL,
         event_type TEXT NOT NULL,
         app_version TEXT NOT NULL,
         session_token TEXT,
         tab_name TEXT,
         duration_sec INTEGER,
         client_country TEXT,
         details TEXT
       )"
    )

    cols <- tryCatch(DBI::dbGetQuery(con, "PRAGMA table_info(usage_events)"), error = function(e) data.frame(name = character()))
    col_names <- cols$name %||% character()

    if (!("client_country" %in% col_names)) {
      DBI::dbExecute(con, "ALTER TABLE usage_events ADD COLUMN client_country TEXT")
    }

    if ("client_ip" %in% col_names) {
      DBI::dbExecute(con, "UPDATE usage_events SET client_ip = NULL WHERE client_ip IS NOT NULL AND trim(client_ip) <> ''")
    }

    invisible(TRUE)
  }, error = function(e) {
    warning(sprintf("Usage metrics DB init failed: %s", conditionMessage(e)), call. = FALSE)
    invisible(FALSE)
  })
}

log_usage_event <- function(session, event_type, tab_name = NA_character_, duration_sec = NA_integer_, details = NULL, input = NULL) {
  tryCatch({
    client_country <- normalize_country_value(tryCatch(session$userData$client_country, error = function(e) NULL))

    con <- DBI::dbConnect(RSQLite::SQLite(), metrics_db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;")
    DBI::dbExecute(
      con,
      "INSERT INTO usage_events (event_ts_utc, event_type, app_version, session_token, tab_name, duration_sec, client_country, details)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        format(Sys.time(), tz = "UTC", usetz = TRUE),
        as.character(event_type),
        app_version,
        as.character(session$token),
        if (is.null(tab_name) || !length(tab_name) || is.na(tab_name) || !nzchar(as.character(tab_name))) NA_character_ else as.character(tab_name),
        if (is.null(duration_sec) || !length(duration_sec) || is.na(duration_sec)) NA_integer_ else as.integer(duration_sec),
        if (is.null(client_country) || !length(client_country) || is.na(client_country) || !nzchar(as.character(client_country))) NA_character_ else as.character(client_country),
        serialize_log_details(input = input, details = details)
      )
    )
    invisible(TRUE)
  }, error = function(e) {
    warning(sprintf("Usage metrics write failed: %s", conditionMessage(e)), call. = FALSE)
    invisible(FALSE)
  })
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

init_metrics_db()


# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/gsap/3.12.5/gsap.min.js"),
    tags$style(HTML("

.isoqc-header {
  display: grid;
  grid-template-columns: minmax(150px, auto) minmax(0, 1fr) minmax(260px, 420px);
  align-items: center;
  gap: 16px;
  padding: 12px 10px 14px 10px;
}

.isoqc-header-left {
  justify-self: start;
  display: flex;
  align-items: center;
  align-self: start;
  margin-top: 10px;
}

.isoqc-header-center {
  justify-self: center;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-width: 0;
  text-align: center;
  margin-top: -8px;
}

.isoqc-header-right {
  justify-self: end;
  align-self: start;
  width: 100%;
  max-width: 420px;
}

.isoqc-cite-box {
  display: block;
  width: 100%;
  background: rgba(255,255,255,0.94);
  border: 1px solid rgba(10,146,255,0.18);
  border-radius: 14px;
  padding: 10px 12px;
  box-shadow: 0 8px 22px rgba(0,0,0,0.08);
  line-height: 1.35;
  text-align: left;
}

.isoqc-cite-label {
  display: inline-block;
  margin-right: 6px;
  color: #0A92FF;
  font-weight: 700;
  font-size: 12px;
  letter-spacing: 0.02em;
  text-transform: uppercase;
}

.isoqc-cite-link,
.isoqc-cite-link:visited,
.isoqc-cite-link:hover,
.isoqc-cite-link:focus,
.isoqc-cite-link:active {
  color: #334155 !important;
  font-size: 11.5px;
  font-weight: 500;
  text-decoration: none !important;
}

.isoqc-cite-link:hover,
.isoqc-cite-link:focus {
  color: #0A92FF !important;
}

.isoqc-basemap-panel {
  z-index: 1000;
  background: rgba(255,255,255,0.94);
  border: 1px solid rgba(10,146,255,0.16);
  border-radius: 14px;
  padding: 8px 10px;
  box-shadow: 0 8px 22px rgba(0,0,0,0.14);
  backdrop-filter: blur(6px);
  -webkit-backdrop-filter: blur(6px);
}

.isoqc-basemap-control .form-group {
  margin-bottom: 0;
}

.isoqc-basemap-control .control-label {
  display: none;
}

.isoqc-basemap-control select.form-control {
  min-width: 124px;
  height: 38px;
  padding: 0 38px 0 12px;
  border: 1px solid rgba(126,144,166,0.45);
  border-radius: 10px;
  background-color: #ffffff;
  background-image: url('data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='8' viewBox='0 0 12 8'%3E%3Cpath fill='none' stroke='%2355697A' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.8' d='M1 1.5 6 6.5l5-5'/%3E%3C/svg%3E');
  background-repeat: no-repeat;
  background-position: right 12px center;
  background-size: 12px 8px;
  color: #334155;
  font-size: 14px;
  font-weight: 600;
  line-height: 1.2;
  box-shadow: none;
  appearance: none;
  -webkit-appearance: none;
  -moz-appearance: none;
}

.isoqc-basemap-control select.form-control:hover {
  border-color: rgba(10,146,255,0.35);
}

.isoqc-basemap-control select.form-control:focus {
  border-color: #0A92FF;
  box-shadow: 0 0 0 0.18rem rgba(10,146,255,0.14);
}

.app-header-wordmark-row {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0;
  line-height: 1;
  min-height: 112px;
}

.app-header-wordmark-part {
  height: 112px;
  width: auto;
  display: block;
  image-rendering: auto;
  flex-shrink: 0;
}

.app-wordmark-fallback {
  color: #0A92FF;
  font-size: 96px;
  font-weight: 800;
  letter-spacing: 0.02em;
  line-height: 1;
}

.app-header-title {
  margin-top: 8px;
  color: #0A92FF;
  font-size: clamp(16px, 2vw, 28px);
  font-weight: 600;
  line-height: 1.15;
  text-align: center;
  max-width: 100%;
  word-break: break-word;
}

.iso-pie-host {
  width: 72px;
  height: 72px;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
  flex: 0 0 72px;
  margin: 0 -8px;
}

.iso-pie-svg {
  width: 72px;
  height: 72px;
  overflow: visible;
  cursor: default;
}

.iso-pie-tooltip {
  position: absolute;
  left: 50%;
  top: calc(100% + 8px);
  transform: translateX(-50%);
  background: rgba(255,255,255,0.97);
  color: #1f2937;
  border: 1px solid rgba(10,146,255,0.18);
  border-radius: 10px;
  padding: 6px 9px;
  box-shadow: 0 6px 18px rgba(0,0,0,0.12);
  font-size: 11px;
  font-weight: 600;
  line-height: 1.25;
  white-space: nowrap;
  opacity: 0;
  pointer-events: none;
  transition: opacity 0.16s ease;
  z-index: 20;
}

.iso-pie-tooltip.is-visible {
  opacity: 1;
}

.iso-pie-track {
  opacity: 0.18;
}

.iso-pie-segment {
  cursor: pointer;
}

@media (max-width: 900px) {
  .isoqc-header {
    grid-template-columns: 1fr;
    row-gap: 10px;
  }

  .isoqc-header-left {
    justify-self: center;
    margin-top: 5px;
  }

  .isoqc-header-right {
    justify-self: center;
    max-width: 760px;
  }

  .isoqc-cite-box {
    text-align: center;
  }

  .isoqc-cite-label {
    display: block;
    margin: 0 0 4px 0;
  }

  .app-header-wordmark-part {
    height: 100px;
  }

  .app-wordmark-fallback {
    font-size: 80px;
  }

  .app-header-title {
    font-size: 20px;
  }

  .iso-pie-host,
  .iso-pie-svg {
    width: 64px;
    height: 64px;
    flex-basis: 64px;
  }

  .iso-pie-tooltip {
    font-size: 10px;
    padding: 5px 8px;
  }
}

@media (max-width: 640px) {
  .app-header-wordmark-part {
    height: 88px;
  }

  .app-wordmark-fallback {
    font-size: 68px;
  }

  .app-header-title {
    font-size: 17px;
  }

  .iso-pie-host,
  .iso-pie-svg {
    width: 56px;
    height: 56px;
    flex-basis: 56px;
  }

  .iso-pie-tooltip {
    top: calc(100% + 6px);
  }

  .isoqc-cite-box {
    padding: 9px 10px;
  }

  .isoqc-cite-link,
  .isoqc-cite-link:visited,
  .isoqc-cite-link:hover,
  .isoqc-cite-link:focus,
  .isoqc-cite-link:active {
    font-size: 10.5px;
  }
}

/* --- IsoQC UI color overrides --- */
a,
a:visited,
a:hover,
a:focus,
a:active,
.action-button,
.action-button:visited,
.action-button:hover,
.action-button:focus,
.action-button:active,
.btn-link,
.btn-link:hover,
.btn-link:focus,
.btn-link:active {
  color: #0A92FF !important;
}

.nav-tabs > li > a,
.nav-tabs > li > a:hover,
.nav-tabs > li > a:focus,
.nav-tabs > li:not(.active) > a,
.nav-tabs > li:not(.active) > a:hover,
.nav-tabs > li:not(.active) > a:focus,
.nav-tabs .nav-link,
.nav-tabs .nav-link:hover,
.nav-tabs .nav-link:focus,
.nav-tabs .nav-link:not(.active),
.nav-tabs .nav-item:not(.active) .nav-link {
  color: #0A92FF !important;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus,
.nav-tabs .nav-link.active,
.nav-tabs .nav-link.active:hover,
.nav-tabs .nav-link.active:focus {
  color: #0A92FF !important;
}

.btn,
.btn:hover,
.btn:focus,
.btn:active,
.btn-default,
.btn-default:hover,
.btn-default:focus,
.btn-secondary,
.btn-secondary:hover,
.btn-secondary:focus,
.btn-primary,
.btn-primary:hover,
.btn-primary:focus,
.btn-primary:active,
.btn-success,
.btn-success:hover,
.btn-success:focus {
  background-color: #7E90A6 !important;
  border-color: #7E90A6 !important;
  color: #000000 !important;
}

.btn .fa,
.btn-default .fa,
.btn-primary .fa,
.btn-secondary .fa,
.btn-success .fa {
  color: inherit !important;
}

.btn:hover,
.btn:focus,
.btn:active {
  filter: brightness(0.96);
}

.instructions-content h1,
.instructions-content h2,
.instructions-content h3,
.instructions-content h4,
.instructions-content h5,
.instructions-content h6 {
  color: #0A92FF !important;
  font-weight: 700 !important;
}
    ")),
    tags$script(HTML("
      (function() {
        function arcPath(cx, cy, r, startAngle, endAngle) {
          var start = (Math.PI / 180) * (startAngle - 90);
          var end = (Math.PI / 180) * (endAngle - 90);
          var x1 = cx + r * Math.cos(start);
          var y1 = cy + r * Math.sin(start);
          var x2 = cx + r * Math.cos(end);
          var y2 = cy + r * Math.sin(end);
          var largeArc = (endAngle - startAngle) > 180 ? 1 : 0;
          return 'M ' + x1 + ' ' + y1 + ' A ' + r + ' ' + r + ' 0 ' + largeArc + ' 1 ' + x2 + ' ' + y2;
        }

        function setAttrs(el, attrs) {
          Object.keys(attrs).forEach(function(key) {
            el.setAttribute(key, attrs[key]);
          });
        }

        function renderHeaderPie(payload) {
          var group = document.getElementById('isoPieChart');
          var tooltip = document.getElementById('isoPieTooltip');
          if (!group) return;

          while (group.firstChild) {
            group.removeChild(group.firstChild);
          }

          if (tooltip) {
            tooltip.textContent = '';
            tooltip.classList.remove('is-visible');
          }

          var o18 = Number(payload && payload.o18 ? payload.o18 : 0);
          var h2 = Number(payload && payload.h2 ? payload.h2 : 0);
          var values = [o18, h2];
          var labels = ['δ18O', 'δ2H'];
          var colors = ['#0A92FF', 'red'];
          var total = values.reduce(function(a, b) { return a + b; }, 0);
          var cx = 200, cy = 200, r = 100, baseWidth = 50;
          var ns = 'http://www.w3.org/2000/svg';

          var track = document.createElementNS(ns, 'circle');
          setAttrs(track, {
            cx: cx,
            cy: cy,
            r: r,
            fill: 'none',
            stroke: '#0A92FF',
            'stroke-width': baseWidth,
            class: 'iso-pie-track'
          });
          group.appendChild(track);

          var center = document.createElementNS(ns, 'circle');
          setAttrs(center, {
            cx: cx,
            cy: cy,
            r: 74,
            fill: '#ffffff'
          });
          group.appendChild(center);

          if (!total || total <= 0) return;

          var startAngle = 0;

          values.forEach(function(value, i) {
            if (!value || value <= 0) return;

            var pct = Math.round((value / total) * 100);
            var sweep = (value / total) * 360;
            var endAngle = startAngle + sweep;
            var path = document.createElementNS(ns, 'path');
            setAttrs(path, {
              d: arcPath(cx, cy, r, startAngle, endAngle),
              fill: 'none',
              stroke: colors[i],
              'stroke-width': baseWidth,
              'stroke-linecap': 'round',
              class: 'iso-pie-segment',
              'data-base-width': baseWidth
            });

            var hoverText = labels[i] + ' • ' + pct + '% • n=' + value;
            var title = document.createElementNS(ns, 'title');
            title.textContent = hoverText;
            path.appendChild(title);
            group.appendChild(path);

            var length = path.getTotalLength();
            path.style.strokeDasharray = length + ' ' + length;
            path.style.strokeDashoffset = length;

            if (window.gsap) {
              gsap.to(path, {
                strokeDashoffset: 0,
                duration: 1.1,
                delay: i * 0.12,
                ease: 'power3.out'
              });
            } else {
              path.style.strokeDashoffset = 0;
            }

            path.addEventListener('pointerenter', function() {
              if (tooltip) {
                tooltip.textContent = hoverText;
                tooltip.classList.add('is-visible');
              }
              if (window.gsap) {
                gsap.to(path, {
                  attr: { 'stroke-width': baseWidth + 10 },
                  duration: 0.18,
                  ease: 'power2.out'
                });
              } else {
                path.setAttribute('stroke-width', baseWidth + 10);
              }
            });

            path.addEventListener('pointerleave', function() {
              if (tooltip) {
                tooltip.classList.remove('is-visible');
              }
              if (window.gsap) {
                gsap.to(path, {
                  attr: { 'stroke-width': baseWidth },
                  duration: 0.18,
                  ease: 'power2.out'
                });
              } else {
                path.setAttribute('stroke-width', baseWidth);
              }
            });

            startAngle = endAngle;
          });
        }

        window.renderHeaderPie = renderHeaderPie;

        document.addEventListener('DOMContentLoaded', function() {
          renderHeaderPie({o18: 0, h2: 0});
        });

        if (window.Shiny) {
          Shiny.addCustomMessageHandler('updateHeaderIsoPie', function(payload) {
            renderHeaderPie(payload || {});
          });
        } else {
          document.addEventListener('shiny:connected', function() {
            Shiny.addCustomMessageHandler('updateHeaderIsoPie', function(payload) {
              renderHeaderPie(payload || {});
            });
          }, { once: true });
        }
      })();
    ")),
    tags$script(HTML("
      (function() {
        function pushClientGeo(payload) {
          if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
            window.Shiny.setInputValue('client_geo', payload, {priority: 'event'});
            return;
          }
          setTimeout(function() { pushClientGeo(payload); }, 250);
        }

        function requestClientGeo() {
          fetch('https://ipapi.co/json/', { cache: 'no-store' })
            .then(function(response) {
              if (!response.ok) throw new Error('country_lookup_failed');
              return response.json();
            })
            .then(function(data) {
              pushClientGeo({
                country_name: data && data.country_name ? data.country_name : null,
                source: 'browser_country_lookup',
                ts: Date.now()
              });
            })
            .catch(function() {
              pushClientGeo({
                country_name: null,
                source: 'browser_country_lookup_failed',
                ts: Date.now()
              });
            });
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', requestClientGeo, { once: true });
        } else {
          requestClientGeo();
        }
      })();
    "))
  ),
div(
  class = "isoqc-header",

  tags$a(
    class = "isoqc-header-left",
    href = "https://geochem.hu/",
    target = "_blank",
    rel = "noopener noreferrer",
    tags$img(
      src = "https://geochem.hu/logo/hunrencsfkfgilogo2.svg",
      height = "60px",
      style = "cursor:pointer;display:block;position:relative;top:-6px;"
    )
  ),

  div(
    class = "isoqc-header-center",
    div(
      class = "app-header-wordmark-row",
      if (!is.null(wordmark_src1) && !is.null(wordmark_src2)) {
        tagList(
          tags$img(
            src = wordmark_src1,
            alt = "Is",
            class = "app-header-wordmark-part"
          ),
          div(
            class = "iso-pie-host",
            tags$div(id = "isoPieTooltip", class = "iso-pie-tooltip"),
            tags$svg(
              id = "isoPieSvg",
              class = "iso-pie-svg",
              viewBox = "0 0 400 400",
              role = "img",
              `aria-label` = "Availability of O18 and H2 records",
              tags$g(id = "isoPieChart")
            )
          ),
          tags$img(
            src = wordmark_src2,
            alt = "QC",
            class = "app-header-wordmark-part"
          )
        )
      } else {
        tags$span("IsoQC", class = "app-wordmark-fallback")
      }
    ),
    tags$span(
      "Screening Dashboard for Precipitation Stable Isotope Records",
      class = "app-header-title"
    )
  ),

  div(
    class = "isoqc-header-right",
    div(
      class = "isoqc-cite-box",
      tags$span("Cite:", class = "isoqc-cite-label"),
      tags$a(
        href = citation_url,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "isoqc-cite-link",
        citation_text
      )
    )
  )
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
                  conditionalPanel(
                    condition = "input.map_station",    ## only truthy once the mapping UI exists
                    h5("Preview (first 10 rows)"),
                    tableOutput("mapped_preview_table"),
                    br(),
                    h5("Station Data Availability Over Time"),
                    selectInput(
                      "resolution", "Select Time Resolution:",
                      choices = c("Daily" = "day", "Monthly" = "month", "Yearly" = "year"),
                      selected = "month"
                    ),
                    plotlyOutput("station_data_plot")
                  ))
              )),
              
              tabPanel("Dashboard", sidebarLayout(
                sidebarPanel(
                  selectizeInput("station", "Select Station:", choices = NULL, options = list(placeholder = "Type to search...")),
                  sliderInput("Xd", "Nearby Search Radius (km):", min = 0, max = 500, value = 90, step = 10),
                  numericInput("d18O_threshold", HTML("&delta;<sup>18</sup>O Threshold (‰):"), value = 6.5),
                  numericInput("d2H_threshold",   HTML("&delta;<sup>2</sup>H Threshold (‰):"), value = 52),
                  numericInput("D_excess_threshold", "D-excess Threshold (‰):", value = 19),
                  
                  tags$p("Default thresholds based on ",
                         a("Erdélyi et al. (2024)", href="https://doi.org/10.17738/ajes.2024.0014", target="_blank", style="color:#0A92FF;"),
                         ".", style="font-size:12px;font-style:italic;color:grey;"),
                  
                  numericInput("g_isoO", HTML("&delta;<sup>18</sup>O elev. corr. (‰/km):"), value = 1.2, step = 0.1),
                  numericInput("g_isoH", HTML("&delta;<sup>2</sup>H elev. corr. (‰/km):"), value = 7.9, step = 0.1),
                  
                  tags$p("Default elevation correction based on ",
                         a("Kern et al. (2020)", href = "https://doi.org/10.3390/w12061797", target = "_blank", style = "color:#0A92FF;"),
                         ".", style = "font-size:12px;font-style:italic;color: grey;"),
                  
                  tags$div(
                    style = "margin-bottom: 12px;",
                    fluidRow(
                      column(
                        6,
                        dateInput("date_min", "From:", value = Sys.Date(), format = "dd-mm-yyyy", startview = "year")
                      ),
                      column(
                        6,
                        dateInput("date_max", "To:", value = Sys.Date(), format = "dd-mm-yyyy", startview = "year")
                      )
                    ),
                    sliderInput(
                      "date_range", "Selected Date Range:",
                      min = Sys.Date() - 365,
                      max = Sys.Date(),
                      value = c(Sys.Date() - 365, Sys.Date()),
                      timeFormat = "%d-%m-%Y",
                      step = 1,
                      dragRange = TRUE
                    ),
                    actionButton("reset_date_range", "Reset date range", icon = icon("rotate-left"))
                  ),
                  
                  tags$div(
                    style = "position: relative;",
                    leafletOutput("map"),
                    absolutePanel(
                      top = 10, right = 10, fixed = FALSE, draggable = FALSE,
                      class = "isoqc-basemap-panel",
                      div(
                        class = "isoqc-basemap-control",
                        selectInput(
                          "basemap", NULL,
                          choices = c("Light", "Terrain"),
                          selected = "Light",
                          width = "124px",
                          selectize = FALSE
                        )
                      )
                    )
                  ),
                  br(),
                  downloadButton("download_plot_data", "Download mean time series")
                ),
                mainPanel(
                  plotlyOutput("plot_O18"), div(style = "height:40px;"),
                  plotlyOutput("plot_H2"),  div(style = "height:40px;"),
                  plotlyOutput("plot_D_excess"), div(style = "height:40px;"),
                  div(style = "height:40px;"),
                  plotlyOutput("plot_XY")
                )
              )),
              
              tabPanel("Instructions", div(class = "instructions-content", includeMarkdown("instructions.Rmd")))
  ),
  
  
  tags$footer(
    style = "width:100%;text-align:center;padding:10px;background:white;
           font-size:12px;color:grey;border-top:1px solid #ddd;margin-top:30px;",

    "© 2026 ",

    tags$a(
      href   = "https://geochem.hu/",
      target = "_blank",
      rel    = "noopener noreferrer",
      "HUN-REN CSFK FGI"
    ),

    ". Developed by ",

    tags$a(
      href   = "https://paleoclimate2ka.geochem.hu/",
      target = "_blank",
      rel    = "noopener noreferrer",
      "2ka Paleoklíma Lendület"
    ),

    ". All rights reserved. ",

    actionLink("show_changelog", paste("IsoQC", app_version))
  )
  ,
  
  tags$head(tags$script(HTML("
  $(document).on('hidden.bs.modal', '.modal', function () {
    Shiny.setInputValue('any_modal_closed', Math.random());
  });

  function bindDateRangeFinish() {
    var el = $('#date_range');
    if (!el.length) return;
    var slider = el.data('ionRangeSlider');
    if (!slider) return;

    slider.update({
      onFinish: function(data) {
        Shiny.setInputValue('date_range_commit', [data.from, data.to], {priority: 'event'});
      }
    });
  }

  $(document).on('shiny:connected', function() {
    setTimeout(bindDateRangeFinish, 0);
  });

  Shiny.addCustomMessageHandler('bindDateRangeFinish', function(_) {
    setTimeout(bindDateRangeFinish, 0);
  });
"))))

# --- Server logic ---
server <- function(input, output, session) {

  session_started_at <- Sys.time()

  session_start_logged <- reactiveVal(FALSE)
  session_start_deadline <- Sys.time() + 3

  log_session_start_once <- function() {
    if (isTRUE(session_start_logged())) return(invisible(FALSE))
    session_start_logged(TRUE)

    log_usage_event(
      session,
      "session_start",
      details = list(
        app_launch = TRUE,
        country_source = tryCatch(session$userData$client_country_source, error = function(e) NA_character_)
      ),
      input = input
    )

    invisible(TRUE)
  }

  observeEvent(input$client_geo, {
    geo <- input$client_geo

    browser_country <- tryCatch(geo$country_name, error = function(e) NULL)
    browser_source <- tryCatch(as.character(geo$source)[1], error = function(e) NA_character_)

    set_session_client_context(
      session,
      country = browser_country,
      source = if (is.na(browser_source) || !nzchar(browser_source)) "browser_country_lookup" else browser_source
    )

    backfill_session_client_context(session)

    if (!isTRUE(session_start_logged()) && !is.na(normalize_country_value(browser_country))) {
      log_session_start_once()
    }
  }, ignoreInit = TRUE)

  observe({
    if (isTRUE(session_start_logged())) return()
    invalidateLater(250, session)
    if (Sys.time() < session_start_deadline) return()
    log_session_start_once()
  })

  observeEvent(input$main_tabs, {
    req(input$main_tabs)
    log_usage_event(session, "tab_view", tab_name = input$main_tabs, input = input)
  }, ignoreInit = FALSE)

  observeEvent(input$import_data, {
    log_usage_event(session, "import_data_click", tab_name = isolate(input$main_tabs), details = list(source = "user_upload", file_name = isolate(input$file$name %||% NA_character_)), input = input)
  }, ignoreInit = TRUE)

  observeEvent(input$load_test_data, {
    log_usage_event(session, "load_test_data_click", tab_name = isolate(input$main_tabs), details = list(source = "PrismEU Database"), input = input)
  }, ignoreInit = TRUE)

  observeEvent(input$download_plot_data, {
    log_usage_event(session, "download_export_click", tab_name = isolate(input$main_tabs), details = list(export_type = "mean_time_series"), input = input)
  }, ignoreInit = TRUE)

  observeEvent(
    list(
      input$station,
      input$Xd,
      input$d18O_threshold,
      input$d2H_threshold,
      input$D_excess_threshold,
      input$g_isoO,
      input$g_isoH,
      input$date_range_commit,
      input$date_min,
      input$date_max,
      input$basemap,
      input$resolution
    ), {
      log_usage_event(
        session,
        "dashboard_params_change",
        tab_name = isolate(input$main_tabs),
        details = list(change_source = "dashboard_controls"),
        input = input
      )
    },
    ignoreInit = TRUE
  )

  observe({
    df <- data_GNIP_raw()
    o18_n <- 0L
    h2_n  <- 0L

    if (!is.null(df)) {
      o18_n <- sum(!is.na(df$d18O))
      h2_n  <- sum(!is.na(df$d2H))
    }

    session$sendCustomMessage("updateHeaderIsoPie", list(
      o18 = as.integer(o18_n),
      h2  = as.integer(h2_n)
    ))
  })
  
  observeEvent(input$any_modal_closed, { suppress_data_warnings(FALSE) })
  
  # Skip the very first availability warning after (re)loading data
  skip_first_warning <- reactiveVal(FALSE)
  
  
  # --- Context & guards ---
  station_changed <- reactiveVal(FALSE)     # user has changed station at least once
  last_action <- reactiveVal(NULL)          # "selection" or "changelog"
  suppress_data_warnings <- reactiveVal(FALSE)  # TRUE while changelog modal is up
  last_warn_key <- reactiveVal(NULL)        # to avoid repeating the same popup
  
  # User selection: station (ignore initial programmatic selection)
  observeEvent(input$station, {
    station_changed(TRUE)
    last_action("selection")
  }, ignoreInit = TRUE)
  
  
  
  ####
  # User selection: date changes (both sources)
  observeEvent(input$date_range_commit, { last_action("selection") }, ignoreInit = TRUE)
  observeEvent(list(input$date_min, input$date_max), {
    last_action("selection")
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$show_changelog, {
    log_usage_event(session, "changelog_open", tab_name = isolate(input$main_tabs), input = input)
    last_action("changelog")
    suppress_data_warnings(TRUE)
    showModal(modalDialog(
      title = "IsoQC Changelog",
      size  = "l",
      easyClose = TRUE,   # <-- most félrekattintásra és ESC-re is zár
      footer = tagList(actionButton("close_changelog", "Close")),
      includeMarkdown("changelog.Rmd")
    ))
  })
  
  observeEvent(input$close_changelog, {
    suppress_data_warnings(FALSE)
    removeModal()
  })
  #### 
  
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
  # Top preview: raw data (first 5 rows)
  raw_preview_data <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    raw <- switch(ext,
                  "csv" = read.csv(input$file$datapath, sep = input$csv_delim, stringsAsFactors = FALSE),
                  "xlsx" = read_excel(input$file$datapath),
                  "xls" = read_excel(input$file$datapath)
    )
    head(raw, 5)
  })
  
  # Bottom preview: mapped columns only
  mapped_preview_data <- reactive({
    req(input$file, input$map_station, input$map_alt, input$map_lat, input$map_lon, input$map_date, input$map_O18, input$map_H2)
    ext <- tolower(tools::file_ext(input$file$name))
    raw <- switch(ext,
                  "csv" = read.csv(input$file$datapath, sep = input$csv_delim, stringsAsFactors = FALSE),
                  "xlsx" = read_excel(input$file$datapath),
                  "xls" = read_excel(input$file$datapath)
    )
    # Avoid renaming conflicts
    if ("Station" %in% names(raw)) {
      names(raw)[names(raw) == "Station"] <- "Station_original"
    }
    
    raw %>%
      rename(
        Station = !!sym(input$map_station),
        Altitude = !!sym(input$map_alt),
        Latitude = !!sym(input$map_lat),
        Longitude = !!sym(input$map_lon),
        Date = !!sym(input$map_date),
        d18O = !!sym(input$map_O18),
        d2H = !!sym(input$map_H2)
      ) %>%
      
      mutate(
        Date = if (is.numeric(Date)) {
          as.Date(Date, origin = "1899-12-30")
        } else if (ext == "csv") {
          as.Date(parse_date_time(Date, orders = input$date_format))
        } else {
          as.Date(Date)
        }
      ) %>%
      mutate(
        Date = format(Date, "%d/%m/%Y")
      ) %>%
      select(Station, Altitude, Latitude, Longitude, Date, d18O, d2H) %>%
      head(10)
  })
  
  
  preview_data <- reactive({
    req(input$file, input$map_station, input$map_alt, input$map_lat, input$map_lon, input$map_date, input$map_O18, input$map_H2)
    ext <- tolower(tools::file_ext(input$file$name))
    raw <- switch(ext,
                  "csv" = read.csv(input$file$datapath, sep = input$csv_delim, stringsAsFactors = FALSE),
                  "xlsx" = read_excel(input$file$datapath),
                  "xls" = read_excel(input$file$datapath)
    )
    names(raw) <- make.unique(names(raw))
    raw %>%
      rename(
        Station = !!sym(input$map_station),
        Altitude = !!sym(input$map_alt),
        Latitude = !!sym(input$map_lat),
        Longitude = !!sym(input$map_lon),
        Date = !!sym(input$map_date),
        d18O = !!sym(input$map_O18),
        d2H = !!sym(input$map_H2)
      ) %>%
      mutate(
        Date = if (ext == "csv") {
          as.Date(parse_date_time(Date, orders = input$date_format))
        } else {
          as.Date(Date)
        },
        Date = format(Date, "%d/%m/%Y")
      )
  })
  
  
  
  output$preview_table <- renderTable({
    head(preview_data(), 10)
  })
  
  output$mapped_preview_table <- renderTable({
    mapped_preview_data()
  })
  
  output$station_data_plot <- renderPlotly({
    req(preview_data(), input$resolution)
    df <- preview_data()
    
    # parse your formatted dates back to Date
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    
    # bin into periods
    df$Period <- switch(
      input$resolution,
      "day"   = df$Date,
      "month" = as.Date(format(df$Date, "%Y-%m-01")),
      "year"  = as.Date(format(df$Date, "%Y-01-01"))
    )
    
    df$has_d18O <- !is.na(df$d18O)
    df$has_d2H  <- !is.na(df$d2H)
    
    summary_df <- df %>%
      group_by(Period) %>%
      summarise(
        d18O_stations = n_distinct(Station[has_d18O]),
        d2H_stations  = n_distinct(Station[has_d2H]),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(d18O_stations, d2H_stations),
        names_to = "Isotope",
        values_to = "Station_Count"
      )
    
    # először jelöljük, melyik melyik
    summary_df$Isotope <- ifelse(summary_df$Isotope == "d18O_stations", "d18O", "d2H")
    
    # majd adjunk szép labelt: δ¹⁸O és δ²H
    summary_df$Isotope <- factor(
      summary_df$Isotope,
      levels = c("d18O", "d2H"),
      labels = c("δ¹⁸O", "δ²H")
    )
    
    # choose the date format for hover
    date_fmt <- switch(
      input$resolution,
      "day"   = "%d/%m/%Y",
      "month" = "%m/%Y",
      "year"  = "%Y"
    )
    
    # build a hovertemplate: formatted date + station count
    hover_tmpl <- paste0("%{x|", date_fmt, "}<br>Count: %{y}<extra></extra>")
    
    plot_ly(
      summary_df,
      x = ~Period,
      y = ~Station_Count,
      color = ~Isotope,
      # faktor szintek sorrendje: δ¹⁸O, δ²H → ezekhez jönnek a színek
      colors = c("#0A92FF", "red"),
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = hover_tmpl
    ) %>%
      layout(
        title = paste("Number of Stations (", input$resolution, ")", sep = ""),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Stations")
      )
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
    
    # Remove or rename existing 'Station' column to avoid conflict
    if ("Station" %in% names(raw)) {
      names(raw)[names(raw) == "Station"] <- "Station_original"
    }
    
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
    skip_first_warning(TRUE)
    log_usage_event(
      session,
      "data_imported",
      tab_name = isolate(input$main_tabs),
      details = list(
        source = "user_upload",
        file_name = input$file$name %||% NA_character_,
        rows = nrow(df),
        stations = dplyr::n_distinct(df$Station)
      ),
      input = input
    )
    updateSelectizeInput(session, "station",
                         choices = sort(unique(df$Station)),
                         selected = sort(unique(df$Station))[1])
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
    skip_first_warning(TRUE)
    log_usage_event(
      session,
      "data_imported",
      tab_name = isolate(input$main_tabs),
      details = list(
        source = "PrismEU Database",
        rows = nrow(df_test),
        stations = dplyr::n_distinct(df_test$Station)
      ),
      input = input
    )
    # update station selector
    updateSelectizeInput(session, "station",
                         choices  = sort(unique(df_test$Station)),
                         selected = sort(unique(df_test$Station))[1])
    
    # finally switch to Dashboard
    updateTabsetPanel(session, "main_tabs", selected = "Dashboard")
  })
  
  
  station_date_limits <- reactive({
    req(data_GNIP_raw(), input$station)
    df_station <- data_GNIP_raw() %>%
      filter(
        Station == input$station,
        !is.na(Date),
        !(is.na(d18O) & is.na(d2H))
      )
    safe_date_range(df_station$Date)
  })
  
  syncing_date_controls <- reactiveVal(FALSE)
  committed_date_range <- reactiveVal(NULL)
  
  active_date_range <- reactive({
    dr <- committed_date_range()
    req(length(dr) == 2, !any(is.na(dr)))
    as.Date(dr)
  })

  parse_slider_commit_value <- function(x) {
    vals <- unlist(x, use.names = FALSE)
    if (length(vals) < 2) return(as.Date(c(NA, NA)))

    num_vals <- suppressWarnings(as.numeric(vals[1:2]))
    if (all(!is.na(num_vals))) {
      return(as.Date(as.POSIXct(num_vals / 1000, origin = "1970-01-01", tz = "UTC")))
    }

    as.Date(vals[1:2])
  }
  
  update_date_controls <- function(range_values, bounds = station_date_limits(),
                                   update_inputs = TRUE, update_slider = TRUE) {
    range_values <- clamp_date_range(range_values, bounds)
    syncing_date_controls(TRUE)
    on.exit(syncing_date_controls(FALSE), add = TRUE)
    
    if (update_inputs) {
      updateDateInput(session, "date_min",
                      value = range_values[1], min = bounds[1], max = bounds[2])
      updateDateInput(session, "date_max",
                      value = range_values[2], min = bounds[1], max = bounds[2])
    } else {
      updateDateInput(session, "date_min", min = bounds[1], max = bounds[2])
      updateDateInput(session, "date_max", min = bounds[1], max = bounds[2])
    }
    
    if (update_slider) {
      updateSliderInput(
        session, "date_range",
        min        = bounds[1],
        max        = bounds[2],
        value      = range_values,
        timeFormat = "%d-%m-%Y"
      )
      session$sendCustomMessage("bindDateRangeFinish", list())
    }
  }

  observeEvent(list(data_GNIP_raw(), input$station), {
    req(data_GNIP_raw(), input$station)
    bounds <- station_date_limits()
    update_date_controls(bounds, bounds, update_inputs = TRUE, update_slider = TRUE)
    committed_date_range(bounds)
  }, ignoreInit = FALSE)
  
  observeEvent(input$reset_date_range, {
    req(data_GNIP_raw(), input$station)
    bounds <- station_date_limits()
    update_date_controls(bounds, bounds, update_inputs = TRUE, update_slider = TRUE)
    committed_date_range(bounds)
    last_action("selection")
  }, ignoreInit = TRUE)

  observeEvent(input$date_range_commit, {
    req(data_GNIP_raw(), input$station, input$date_range_commit)
    bounds <- station_date_limits()
    final_range <- clamp_date_range(parse_slider_commit_value(input$date_range_commit), bounds)
    update_date_controls(final_range, bounds, update_inputs = TRUE, update_slider = FALSE)
    committed_date_range(final_range)
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$date_min, input$date_max), {
    req(data_GNIP_raw(), input$station, input$date_min, input$date_max)
    if (isTRUE(syncing_date_controls())) return(invisible(NULL))
    bounds <- station_date_limits()
    current_inputs <- clamp_date_range(c(input$date_min, input$date_max), bounds)
    update_date_controls(current_inputs, bounds, update_inputs = TRUE, update_slider = TRUE)
    committed_date_range(current_inputs)
  }, ignoreInit = TRUE)
  
  # When the user clicks a station on the map, update the station dropdown
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)  # make sure there's an ID
    updateSelectizeInput(session, "station", selected = click$id)
    
    # (optional) pan/zoom the map to the clicked station
    leafletProxy("map") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 6)
  })
  
  # --- Selected station altitude for elevation correction ---
  selected_station_altitude <- reactive({
    req(input$station, data_GNIP_raw())
    df <- data_GNIP_raw()
    alt_vec <- df$Altitude[df$Station == input$station]
    alt_vec <- alt_vec[!is.na(alt_vec)]
    if (length(alt_vec) == 0) 0 else alt_vec[1]
  })
  
  # Reactive data transforms
  correctedData <- reactive({
    df <- data_GNIP_raw()
    refAlt <- selected_station_altitude()  # use altitude of selected station
    df %>% mutate(
      # based on line 321 (https://github.com/istvan60/SPbI/blob/main/v4)
      d18Oc = d18O + ((Altitude - refAlt)*input$g_isoO/1000),
      d2Hc  = d2H  + ((Altitude - refAlt)*input$g_isoH/1000)
    )
  })
  selected_station_data <- reactive({
    req(input$station)
    correctedData() %>% filter(Station == input$station)
  })
  
  station_distance_info <- reactive({
    req(input$station, data_GNIP_raw())
    sts <- helpers()$stations
    coords <- helpers()$coords
    idx <- match(input$station, sts$Station)
    req(!is.na(idx), nrow(sts) > 0)
    dists_km <- as.numeric(geosphere::distGeo(coords[idx, , drop = FALSE], coords)) / 1000
    sts %>% mutate(distance_km = dists_km)
  })
  
  nearby_station_data <- reactive({
    req(input$station)
    near_names <- station_distance_info() %>%
      filter(Station != input$station, distance_km <= input$Xd) %>%
      pull(Station)
    correctedData() %>% filter(Station %in% near_names)
  })
  filtered_data <- reactive({
    req(input$station)
    dr <- active_date_range()
    sel <- selected_station_data() %>% filter(Date >= dr[1] & Date <= dr[2])
    near <- nearby_station_data()  %>% filter(Date >= dr[1] & Date <= dr[2])
    means <- near %>% group_by(Date) %>% summarize(
      d18Oc_Mean    = mean(d18Oc,    na.rm = TRUE),
      d18Oc_SD      = sd(d18Oc,      na.rm = TRUE),
      d2Hc_Mean     = mean(d2Hc,     na.rm = TRUE),
      d2Hc_SD       = sd(d2Hc,       na.rm = TRUE),
      D_excess_Mean = mean(D_excess, na.rm = TRUE),
      D_excess_SD   = sd(D_excess,   na.rm = TRUE),
      .groups       = "drop"
    )
    left_join(sel, means, by = "Date") %>% mutate(
      d18Oc_Diff    = abs(d18Oc    - d18Oc_Mean),
      d2Hc_Diff     = abs(d2Hc     - d2Hc_Mean),
      D_excess_Diff = abs(D_excess - D_excess_Mean)
    )
  })
  
  nearby_means <- reactive({
    req(input$station)
    near <- nearby_station_data() %>%
      filter(Date >= active_date_range()[1] & Date <= active_date_range()[2])
    
    near %>% group_by(Date) %>% summarize(
      d18Oc   = mean(d18Oc,    na.rm = TRUE),
      d2Hc    = mean(d2Hc,     na.rm = TRUE),
      D_excess = mean(D_excess, na.rm = TRUE),
      .groups = "drop"
    )
  })
  
  
  selected_station_coords <- reactive({
    req(input$station)
    selected <- helpers()$stations %>% filter(Station == input$station)
    if (nrow(selected) > 0) {
      list(lat = selected$lat[1], lon = selected$lon[1])
    } else {
      NULL
    }
  })
  
  
  # Build a key that changes on (station, date_range)
  availability_key <- reactive({
    dr <- active_date_range()
    req(input$station, dr)
    paste(input$station, as.character(dr), collapse = "|")
  })
  
  observeEvent(availability_key(), {
    # Only warn for user-triggered selection changes,
    # not during app init, not while changelog is open.
    
    if (isTRUE(skip_first_warning())) {
      skip_first_warning(FALSE)
      return(invisible(NULL))
    }
    
    if (!isTRUE(station_changed())) return()
    if (!identical(last_action(), "selection")) return()
    if (isTRUE(suppress_data_warnings())) return()
    
    dr <- active_date_range()
    sd_sub <- selected_station_data() %>%
      dplyr::filter(Date >= dr[1], Date <= dr[2])
    
    # Determine availability strictly on the selected station's raw values
    has_O18 <- nrow(sd_sub) > 0 && any(!is.na(sd_sub$d18O))
    has_H2  <- nrow(sd_sub) > 0 && any(!is.na(sd_sub$d2H))
    
    missing <- c()
    if (!has_O18) missing <- c(missing, "δ<sup>18</sup>O")
    if (!has_H2)  missing <- c(missing, "δ<sup>2</sup>H")
    
    if (length(missing) == 0) {
      # reset last shown key so future missing states can notify again
      last_warn_key(NULL)
      return(invisible(NULL))
    }
    
    # De-dup: only show if this (station, range, which-missing) changed
    key <- paste(input$station, format(dr[1]), format(dr[2]), paste(missing, collapse = "&"))
    if (identical(last_warn_key(), key)) return(invisible(NULL))
    last_warn_key(key)
    
    msg <- if (length(missing) == 1) {
      sprintf("<b>%s</b> does not have %s measurements between %s and %s.",
              input$station, missing,
              format(dr[1], "%d-%m-%Y"), format(dr[2], "%d-%m-%Y"))
    } else {
      sprintf("<b>%s</b> does not have %s measurements between %s and %s.",
              input$station, paste(missing, collapse = " and "),
              format(dr[1], "%d-%m-%Y"), format(dr[2], "%d-%m-%Y"))
    }
    
    showModal(modalDialog(
      title = "Data not available",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(msg)
    ))
  })
  
  
  current_basemap_provider <- reactive({
    if (isTruthy(input$basemap) && identical(input$basemap, "Terrain")) {
      "Esri.WorldTopoMap"
    } else {
      "CartoDB.PositronNoLabels"
    }
  })
  
  # Map with initial distances
  output$map <- renderLeaflet({
    req(data_GNIP_raw())
    sts <- helpers()$stations
    leaflet(sts) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        layerId = ~Station,
        radius = 5,
        fillColor   = "#D3D3D3",
        fillOpacity = 0.7,
        stroke      = TRUE,
        color       = "black",
        weight      = 1,
        label       = ~Station,
        labelOptions = labelOptions(direction = "auto", textsize = "12px")
      ) %>%
      setView(
        lng = sts$lon[1],
        lat = sts$lat[1], zoom = 6
      )
  })
  
  observeEvent(input$basemap, {
    req(data_GNIP_raw())
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(current_basemap_provider())
  }, ignoreInit = TRUE)
  
  observe({
    req(input$station)
    dist_info <- station_distance_info()
    sel <- dist_info %>% filter(Station == input$station)
    near_sts <- dist_info %>% filter(Station != input$station, distance_km <= input$Xd)
    other_sts <- dist_info %>% filter(!Station %in% c(input$station, near_sts$Station))
    
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    if (nrow(sel) > 0 && nrow(near_sts) > 0) {
      for (i in seq_len(nrow(near_sts))) {
        proxy <- proxy %>% addPolylines(
          lng = c(sel$lon[1], near_sts$lon[i]),
          lat = c(sel$lat[1], near_sts$lat[i]),
          color = "black", weight = 1, opacity = 0.5,
          label = paste0(round(near_sts$distance_km[i], 2), " km"),
          labelOptions = labelOptions(direction = "auto", textsize = "12px")
        )
      }
    }
    
    if (nrow(other_sts) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = other_sts, lng = ~lon, lat = ~lat,
          layerId = ~Station, radius = 5,
          fillColor   = "#D3D3D3", fillOpacity = 0.7,
          stroke      = TRUE,
          color       = "black",
          weight      = 0.5,
          label = ~Station, labelOptions = labelOptions(direction = "auto")
        )
    }
    
    if (nrow(near_sts) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = near_sts, lng = ~lon, lat = ~lat,
          layerId = ~Station, radius = 6,
          fillColor   = "#666666", fillOpacity = 0.8,
          stroke      = TRUE,
          color       = "black",
          weight      = 0.5,
          label = ~Station, labelOptions = labelOptions(direction = "auto")
        )
    }
    
    if (nrow(sel) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = sel, lng = ~lon, lat = ~lat,
          layerId = ~Station, radius = 7,
          fillColor   = "#0A92FF", fillOpacity = 1,
          stroke      = TRUE,
          color       = "black",
          weight      = 1,
          label = ~Station, labelOptions = labelOptions(direction = "auto")
        )
    }
    
  })


  observe({
    coords <- selected_station_coords()
    if (!is.null(coords)) {
      leafletProxy("map") %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 6)
    }
  })
  
  # ---------- EXPORT DATA FOR PLOTS ----------
  plot_export_data <- reactive({
    req(data_GNIP_raw(), input$station)
    fd <- filtered_data()
    if (is.null(fd) || nrow(fd) == 0) return(NULL)
    
    dr <- active_date_range()
    
    
    fd2 <- fd %>%
      mutate(
        d18Oc_STD_active  = 2 * d18Oc_SD,
        d2Hc_STD_active   = 2 * d2Hc_SD,
        D_excess_STD_active = 2 * D_excess_SD
      )
    
    
    
    # Nearby station data in current range
    #near <- nearby_station_data() %>%
    #  filter(Date >= dr[1] & Date <= dr[2])
    
    # δ18O: selected station, mean, diff, STD, neighbours
    
    d18_mean <- fd2 %>%
      transmute(
        Date,
        Variable = "d18O",
        Legend   = "Nearby mean",
        Value    = d18Oc_Mean
      )
    
    #d18_nei <- near %>%
    #  transmute(
    #    Date,
    #    Variable = "d18O",
    #    Legend   = Station,
    #    Value    = d18Oc
    #  )
    #d18_long <- bind_rows(d18_sel, d18_mean, d18_diff, d18_std, d18_nei)
    d18_long <- bind_rows(d18_mean)
    
    # δ2H: selected station, mean, diff, STD, neighbours
    
    d2H_mean <- fd2 %>%
      transmute(
        Date,
        Variable = "d2H",
        Legend   = "Nearby mean",
        Value    = d2Hc_Mean
      )
    
    #d2H_nei <- near %>%
    #  transmute(
    #    Date,
    #    Variable = "d2H",
    #    Legend   = Station,
    #    Value    = d2Hc
    #  )
    #d2H_long <- bind_rows(d2H_sel, d2H_mean, d2H_diff, d2H_std, d2H_nei)
    d2H_long <- bind_rows(d2H_mean)
    
    # D-excess: selected station, mean, diff, STD, neighbours (uncorrected)
    
    dex_mean <- fd2 %>%
      transmute(
        Date,
        Variable = "D_excess",
        Legend   = "Nearby mean",
        Value    = D_excess_Mean
      )
    
    #dex_nei <- near %>%
    #  transmute(
    #    Date,
    #    Variable = "D_excess",
    #    Legend   = Station,
    #    Value    = D_excess
    #  )
    #dex_long <- bind_rows(dex_sel, dex_mean, dex_diff, dex_std, dex_nei)
    dex_long <- bind_rows(dex_mean)
    
    all_data <- bind_rows(d18_long, d2H_long, dex_long) %>%
      arrange(Date, Variable, Legend)
    
    all_data %>%
      mutate(
        Search_Radius_km   = input$Xd,
        Threshold_d18O     = input$d18O_threshold,
        Threshold_d2H      = input$d2H_threshold,
        Threshold_D_excess = input$D_excess_threshold,
        ElevCorr_d18O      = input$g_isoO,
        ElevCorr_d2H       = input$g_isoH,
        From               = active_date_range()[1],
        To                 = active_date_range()[2],
        Citation           = citation_text
      )
  })
  
  
  output$download_plot_data <- downloadHandler(
    filename = function() {
      paste0("IsoQC_timeseries_", input$station, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_out <- plot_export_data()
      if (is.null(df_out)) df_out <- data.frame()
      
      # 1) minden karakteroszlopot biztosan UTF-8-ra állítunk
      df_out[] <- lapply(df_out, function(col) {
        if (is.character(col)) enc2utf8(col) else col
      })
      
      # 2) megnyitjuk a célfájlt, beírjuk a UTF-8 BOM-ot, majd CSV-be írjuk
      con <- file(file, open = "wb")          # bináris, hogy a BOM biztosan pontos legyen
      on.exit(close(con), add = TRUE)
      
      # UTF-8 BOM
      writeBin(charToRaw("\ufeff"), con)
      
      # A write.table-nek/ write.csv-nek *nem* adjuk át újra a file nevét, hanem a con-t
      # Figyelem: write.csv 'fileEncoding' nélkül is működik, mivel a con már UTF-8-as.
      utils::write.table(
        df_out,
        file = con,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        qmethod = "double",
        na = ""
      )
    }
  )
  
  
  # Time‐series plots
  # y_var: value at selected station (corrected)
  # mean_var: nearby mean (by date)
  # sd_var: nearby SD (by date; base SD)
  # diff_var: |station - nearby mean|
  generate_plot <- function(y_var, mean_var, sd_var, diff_var, color, thr_input) {
    fd <- filtered_data()
    nd <- nearby_station_data() %>%
      filter(Date >= active_date_range()[1] & Date <= active_date_range()[2])
    p <- plot_ly()
    
    # Nearby stations time-series (grey)
    for (stn in sort(unique(nd$Station))) {
      sub <- nd %>% filter(Station == stn)
      sl  <- split_data_by_month(sub, "Date", y_var)
      if (!all(is.na(sl[[y_var]]))) {
        p <- p %>% add_trace(
          data = sl, x = ~Date, y = as.formula(paste0("~", y_var)),
          type = "scatter", mode = "lines+markers",
          line = list(color = "grey", width = 0.5),
          marker = list(size = 2, opacity = 0.7),
          name = stn
        )
      }
    }
    
    # Selected station (colored line)
    sel_l <- split_data_by_month(fd, "Date", y_var)
    p <- p %>% add_trace(
      data = sel_l, x = ~Date, y = sel_l[[y_var]],
      type = "scatter", mode = "lines+markers",
      line = list(color = color, width = 2),
      marker = list(size = 5, color = color),
      name = input$station
    )
    
    # Nearby mean (black dashed)
    mean_l <- split_data_by_month(fd, "Date", mean_var)
    p <- p %>% add_trace(
      data = mean_l, x = ~Date, y = mean_l[[mean_var]],
      type = "scatter", mode = "lines+markers",
      line = list(color = "black", width = 2, dash = "dot"),
      marker = list(size = 4, color = "black"),
      name = "Nearby Mean"
    )
    
    # ----- Bar logic using selected STD type -----
    thr <- as.numeric(input[[thr_input]])

    fd_bars <- fd %>%
      mutate(
        diff_value = .data[[diff_var]],
        sd_base    = .data[[sd_var]],
        sd_value   = 2 * sd_base
      )
    
    
    std_label <- "2×St.Dev."
    
    
    
    
    hover_tmpl <- paste0(
      "%{x|%d-%m-%Y}<br>",
      "Difference: %{y:.2f}‰<br>",
      std_label, ": %{customdata:.2f}‰<extra></extra>"
      
    )
    
    
    
    # Threshold alatti és feletti pontok szétválasztása
    below_thr <- fd_bars %>%
      filter(!is.na(diff_value) & diff_value <= thr)
    above_thr <- fd_bars %>%
      filter(!is.na(diff_value) & diff_value > thr)
    
    ## ---- THRESHOLD ALATT ----
    # Sötétszürke: diff > STD (de még threshold alatt)
    if (nrow(below_thr) > 0) {
      dark_df <- below_thr %>%
        filter(!is.na(sd_value) & diff_value > sd_value)
      
      # Világosszürke: STD >= diff vagy STD hiányzik
      light_df <- below_thr %>%
        filter(is.na(sd_value) | sd_value >= diff_value)
      
      if (nrow(light_df) > 0) {
        p <- p %>% add_bars(
          data  = light_df,
          x     = ~Date,
          y     = ~diff_value,
          marker = list(color = "#C0C0C0"),   # világosszürke
          name  = paste(input$station, "diff."),
          opacity = 0.6,
          width   = 1000 * 60 * 60 * 24 * 20,
          customdata   = ~sd_value,
          hovertemplate = hover_tmpl
        )
      }
      
      if (nrow(dark_df) > 0) {
        p <- p %>% add_bars(
          data  = dark_df,
          x     = ~Date,
          y     = ~diff_value,
          marker = list(color = "#606060"),   # sötétszürke
          name  = paste(input$station, "diff. > 2×St.Dev."),
          opacity = 0.7,
          width   = 1000 * 60 * 60 * 24 * 20,
          customdata   = ~sd_value,
          hovertemplate = hover_tmpl
        )
      }
    }
    
    ## ---- THRESHOLD FELETT ----
    if (nrow(above_thr) > 0) {
      # Fekete körvonal CSAK akkor, ha:
      # diff > threshold (ez már teljesül itt) ÉS diff > STD ÉS STD > 0
      ov_outline <- above_thr %>%
        filter(!is.na(sd_value) & sd_value > 0 & diff_value > sd_value)
      
      # Minden más threshold feletti pont: lila, körvonal nélkül
      ov_no_outline <- above_thr %>%
        filter(is.na(sd_value) | sd_value <= 0 | diff_value <= sd_value)
      
      # Lila fekete körvonallal
      if (nrow(ov_outline) > 0) {
        p <- p %>% add_bars(
          data = ov_outline,
          x    = ~Date,
          y    = ~diff_value,
          marker = list(
            color = "purple",
            line  = list(color = "black", width = 1.5)
          ),
          name = wrap_name(paste(input$station, "diff. > threshold & 2×St.Dev.")),
          opacity = 0.8,
          width   = 1000 * 60 * 60 * 24 * 20,
          customdata   = ~sd_value,
          hovertemplate = hover_tmpl
        )
      }
      
      # Lila, KÖRVONAL NÉLKÜL (diff > threshold, de diff ≤ STD vagy STD ≤ 0 / NA)
      if (nrow(ov_no_outline) > 0) {
        p <- p %>% add_bars(
          data = ov_no_outline,
          x    = ~Date,
          y    = ~diff_value,
          marker = list(color = "purple"),
          name = wrap_name(paste(input$station, "diff. > threshold")),
          #showlegend = nrow(ov_outline) == 0,  # ha már van lila+outline elem, ne duplázzuk
          showlegend  = TRUE,
          opacity = 0.7,
          width   = 1000 * 60 * 60 * 24 * 20,
          customdata   = ~sd_value,
          hovertemplate = hover_tmpl
        )
      }
    }
    
    p %>% layout(
      barmode = "overlay",
      legend  = list(font = list(size = 9), x = 1.02, y = 1),
      xaxis   = list(tickformat = "%d-%m-%Y"),
      yaxis   = list(title = switch(
        y_var,
        "d18Oc"    = "δ¹⁸O (‰)",
        "d2Hc"     = "δ²H (‰)",
        "D_excess" = "D-excess (‰)"
      ))
    )
  }
  
  
  
  output$plot_O18 <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    
    dr <- active_date_range()
    sd_sub <- selected_station_data() %>% dplyr::filter(Date >= dr[1], Date <= dr[2])
    has_O18 <- nrow(sd_sub) > 0 && any(!is.na(sd_sub$d18O))
    if (!has_O18) return(NULL)
    
    generate_plot("d18Oc", "d18Oc_Mean", "d18Oc_SD", "d18Oc_Diff", "#0A92FF", "d18O_threshold")
  })
  
  
  
  output$plot_H2 <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    
    dr <- active_date_range()
    sd_sub <- selected_station_data() %>% dplyr::filter(Date >= dr[1], Date <= dr[2])
    has_H2 <- nrow(sd_sub) > 0 && any(!is.na(sd_sub$d2H))
    if (!has_H2) return(NULL)
    
    generate_plot("d2Hc", "d2Hc_Mean", "d2Hc_SD", "d2Hc_Diff", "red", "d2H_threshold")
  })
  
  
  
  output$plot_D_excess <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    
    dr <- active_date_range()
    sd_sub <- selected_station_data() %>% dplyr::filter(Date >= dr[1], Date <= dr[2])
    has_pair <- nrow(sd_sub) > 0 && any(!is.na(sd_sub$d18O) & !is.na(sd_sub$d2H))
    if (!has_pair) return(NULL)
    
    generate_plot("D_excess", "D_excess_Mean", "D_excess_SD", "D_excess_Diff", "green", "D_excess_threshold")
  })
  
  
  
  
  output$plot_XY <- renderPlotly({
    req(data_GNIP_raw(), input$station)
    
    df <- filtered_data()
    
    # --- require at least one point with BOTH corrected values ---
    has_pair <- nrow(df) > 0 && any(!is.na(df$d18Oc) & !is.na(df$d2Hc))
    if (!has_pair) return(NULL)
    
    valid <- df %>% dplyr::filter(d18Oc_Diff <= input$d18O_threshold & d2Hc_Diff <= input$d2H_threshold)
    prob  <- df %>% dplyr::filter(d18Oc_Diff >  input$d18O_threshold & d2Hc_Diff >  input$d2H_threshold)
    
    plot_ly() %>%
      add_trace(data=df,    x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="#D3D3D3",size=8), name="All points") %>%
      add_trace(data=valid, x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="#666666",size=8), name="Under threshold") %>%
      add_trace(data=prob,  x=~d18Oc, y=~d2Hc, type="scatter", mode="markers",
                marker=list(color="purple",size=10),
                name=wrap_name(paste(input$station,"Above threshold"))) %>%
      layout(
        xaxis=list(title="δ¹⁸O (‰)"),
        yaxis=list(title="δ²H (‰)"),
        legend=list(font=list(size=9), x=1.02, y=1)
      )
  })
  
  session$onSessionEnded(function() {
    duration_sec <- round(as.numeric(difftime(Sys.time(), session_started_at, units = "secs")))
    log_usage_event(session, "session_end", tab_name = isolate(input$main_tabs), duration_sec = duration_sec, details = list(session_complete = TRUE), input = input)
  })
}

shinyApp(ui, server)
