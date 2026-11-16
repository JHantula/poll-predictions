# Poll Prediction Pipeline
# Joona Hantula, updated 11/16/26
# Pulls the latest Silver Bulletin data, predicts each pollster's next poll,
# logs predictions to Google Sheets, scores released polls, and builds the website chart

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(googlesheets4)
library(plotly)
library(htmlwidgets)

# ---- settings ----
polls_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS-FKWVTTFtJT6u56e0bqdfoMcXvDO1DUChsJ3jQAMB2lZk2SMqVfmg7dGjclTYkYWz-Pm5lfcLPjp4/pub?output=csv"
sheet_url <- "https://docs.google.com/spreadsheets/d/1S9-qR7ovW30Dkj0YSnK28HdIP4eGZI0H6eFcvdrl5nM/edit?gid=1556615819#gid=1556615819"

# score each poll against the last prediction made before it was released
score_against <- "release"

# pollster-sponsors and their decay values
pollster_list <- tibble(
  pollster = c("Rasmussen Reports", "Morning Consult", "YouGov", "RMG Research", "Ipsos"),
  sponsor  = c(NA, NA, "Economist", "Napolitan News Service", "Reuters"),
  z        = c(0.9, 0.921, 0.967, 0.963, 0.983))

# the SB average chart URL changes version with every update, so look up the current one
get_avg_url <- function() {
  page <- paste(readLines("https://datawrapper.dwcdn.net/kSCt4/", warn = FALSE), collapse = " ")
  version <- str_extract(page, "kSCt4/(\\d+)/", group = 1)
  paste0("https://datawrapper.dwcdn.net/kSCt4/", version, "/dataset.csv")}

# ---- load and clean data ----
polls_raw <- read_csv(polls_url) |>
  filter(subgroup == "All polls") |>
  filter(!(pollster == "Morning Consult" & population == "A"))

approval_avg <- read_csv(get_avg_url())

polls <- polls_raw |>
  mutate(startdate = as.Date(startdate, format = "%m/%d/%Y"),
         enddate = as.Date(enddate, format = "%m/%d/%Y"),
         middate = startdate + floor((enddate - startdate) / 2))

approvals <- approval_avg |>
  mutate(modeldate = as.Date(modeldate, format = "%m/%d/%Y"),
         agg_net = approve - disapprove)

polls_merged <- polls |>
  left_join(select(approvals, modeldate, agg_net), by = c("middate" = "modeldate")) |>
  filter(!is.na(agg_net)) |>
  mutate(poll_net = approve - disapprove,
         error = poll_net - agg_net) |>
  group_by(pollster, sponsors, middate) |>
  summarize(poll_net = mean(poll_net),
            agg_net = first(agg_net),
            error = mean(error),
            .groups = "drop")

# most recent SB average
latest <- approvals |> filter(modeldate == max(modeldate))
latest_sb <- latest$agg_net
sb_date <- latest$modeldate

# ---- prediction functions ----
# rolling forecast residuals: predict each past poll using only earlier polls
get_residuals <- function(pollster_data, z) {
  residuals <- numeric()
  for (i in 21:nrow(pollster_data)) {
    prior <- pollster_data[1:(i-1), ]
    days_ago <- as.numeric(pollster_data$middate[i] - prior$middate)
    weights <- z ^ days_ago
    weighted_bias <- sum(weights * prior$error) / sum(weights)
    residuals <- c(residuals, pollster_data$error[i] - weighted_bias)}
  return(residuals)}

# point estimate = latest SB + exponentially weighted bias
# interval widths = quantiles of past absolute forecast residuals (unweighted)
make_prediction <- function(pollster_name, sponsor_name, z) {
  if (is.na(sponsor_name)) {
    pollster_data <- polls_merged |> filter(pollster == pollster_name, is.na(sponsors))
  } else {
    pollster_data <- polls_merged |> filter(pollster == pollster_name, sponsors == sponsor_name)}
  pollster_data <- arrange(pollster_data, middate)
  
  days_ago <- as.numeric(max(pollster_data$middate) - pollster_data$middate)
  weights <- z ^ days_ago
  weighted_bias <- sum(weights * pollster_data$error) / sum(weights)
  point_estimate <- latest_sb + weighted_bias
  
  abs_residuals <- abs(get_residuals(pollster_data, z))
  half_width_50 <- quantile(abs_residuals, 0.50, names = FALSE)
  half_width_90 <- quantile(abs_residuals, 0.90, names = FALSE)
  
  tibble(prediction_date = Sys.Date(),
         sb_date = sb_date,
         sb_used = round(latest_sb, 2),
         pollster = pollster_name,
         sponsor = sponsor_name,
         point_estimate = round(point_estimate, 2),
         lower_50 = round(point_estimate - half_width_50, 2),
         upper_50 = round(point_estimate + half_width_50, 2),
         lower_90 = round(point_estimate - half_width_90, 2),
         upper_90 = round(point_estimate + half_width_90, 2))}

predictions <- pmap_dfr(pollster_list, function(pollster, sponsor, z) {
  make_prediction(pollster, sponsor, z)})

print(predictions)

# ---- log in to Google ----
# service account key on GitHub, browser login on laptop
key_path <- Sys.getenv("GS4_KEY")
if (key_path != "") {
  token <- gargle::credentials_service_account(
    path = key_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets")
  gs4_auth(token = token)
} else {
  gs4_auth()
}

# ---- log predictions (skip pollsters already predicted today) ----
existing <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date))

new_predictions <- predictions |>
  anti_join(existing, by = c("prediction_date", "pollster"))

if (nrow(new_predictions) > 0) {
  sheet_append(sheet_url, new_predictions, sheet = "predictions")}

# ---- scoring ----
# every poll from the five pollsters, one row per poll
actual_polls <- polls |>
  rename(sponsor = sponsors) |>
  semi_join(pollster_list, by = c("pollster", "sponsor")) |>
  mutate(release_date = as.Date(createddate, format = "%m/%d/%Y"),
         poll_net = approve - disapprove) |>
  group_by(pollster, sponsor, startdate, enddate) |>
  summarize(release_date = min(release_date),
            poll_net = mean(poll_net),
            .groups = "drop") |>
  mutate(cutoff = if (score_against == "fieldwork") startdate else release_date)

all_predictions <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date))

# skip polls that are already scored
existing_results <- read_sheet(sheet_url, sheet = "results")
if (nrow(existing_results) > 0) {
  existing_results <- existing_results |>
    mutate(startdate = as.Date(startdate), enddate = as.Date(enddate))
  actual_polls <- anti_join(actual_polls, existing_results,
                            by = c("pollster", "sponsor", "startdate", "enddate"))}

# match each new poll to its most recent prediction before the cutoff, then score it
new_results <- actual_polls |>
  inner_join(all_predictions, by = c("pollster", "sponsor"), relationship = "many-to-many") |>
  filter(prediction_date < cutoff) |>
  group_by(pollster, sponsor, startdate, enddate) |>
  slice_max(prediction_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(error = round(poll_net - point_estimate, 2),
         abs_error = abs(error),
         in_50 = as.integer(poll_net >= lower_50 & poll_net <= upper_50),
         in_90 = as.integer(poll_net >= lower_90 & poll_net <= upper_90),
         scored_date = Sys.Date(),
         surprise = case_when(
           in_50 == 1 ~ "As expected",
           in_90 == 1 ~ "Somewhat surprising",
           TRUE ~ "Very surprising")) |>
  select(pollster, sponsor, startdate, enddate, release_date, poll_net,
         prediction_date, point_estimate, lower_50, upper_50, lower_90, upper_90,
         error, abs_error, in_50, in_90, scored_date, surprise)

if (nrow(new_results) > 0) {
  if (nrow(existing_results) == 0) {
    sheet_write(new_results, ss = sheet_url, sheet = "results")
  } else {
    sheet_append(sheet_url, new_results, sheet = "results")}}

print(new_results)

# ---- interactive website chart ----
dir.create("docs", showWarnings = FALSE)

make_label <- function(pollster, sponsor) {
  ifelse(is.na(sponsor) | sponsor == "", pollster, paste(pollster, sponsor, sep = " / "))}

preds <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date),
         label = make_label(pollster, sponsor))

res <- read_sheet(sheet_url, sheet = "results") |>
  mutate(release_date = as.Date(release_date),
         label = make_label(pollster, sponsor))

labels <- unique(preds$label)
palette <- c("#2a9d8f", "#e76f51", "#6a4c93", "#e9a03b", "#264653")
colors <- setNames(palette[seq_along(labels)], labels)

p <- plot_ly()

for (lab in labels) {
  d <- filter(preds, label == lab)
  r <- filter(res, label == lab)
  col <- colors[[lab]]
  vis <- if (grepl("Ipsos", lab)) TRUE else "legendonly"
  
  d <- d |> mutate(hover = paste0(
    "<b>", lab, "</b><br>", format(prediction_date, "%b %d, %Y"),
    "<br>Predicted: ", point_estimate,
    "<br>50% interval: ", lower_50, " to ", upper_50,
    "<br>90% interval: ", lower_90, " to ", upper_90,
    "<br>SB average: ", sb_used))
  
  p <- p |>
    add_segments(data = d, x = ~prediction_date, xend = ~prediction_date,
                 y = ~lower_90, yend = ~upper_90,
                 line = list(color = col, width = 7), opacity = 0.3,
                 legendgroup = lab, showlegend = FALSE, visible = vis,
                 hoverinfo = "skip") |>
    add_segments(data = d, x = ~prediction_date, xend = ~prediction_date,
                 y = ~lower_50, yend = ~upper_50,
                 line = list(color = col, width = 7),
                 legendgroup = lab, showlegend = FALSE, visible = vis,
                 hoverinfo = "skip") |>
    add_markers(data = d, x = ~prediction_date, y = ~point_estimate,
                marker = list(color = "white", size = 4,
                              line = list(color = col, width = 1)),
                name = lab, legendgroup = lab, visible = vis,
                text = ~hover, hoverinfo = "text")
  
  if (nrow(r) > 0) {
    r <- r |> mutate(hover = paste0(
      "<b>", lab, " poll</b><br>Released ", format(release_date, "%b %d, %Y"),
      "<br>Actual: ", poll_net,
      ifelse(poll_net > point_estimate, " (higher than predicted)", " (lower than predicted)"),
      "<br>Predicted: ", point_estimate,
      "<br>50% interval: ", lower_50, " to ", upper_50,
      ifelse(in_50 == 1, " ✓", " ✗"),
      "<br>90% interval: ", lower_90, " to ", upper_90,
      ifelse(in_90 == 1, " ✓", " ✗")))
    
    p <- p |>
      add_markers(data = r, x = ~release_date, y = ~poll_net,
                  marker = list(symbol = "x", size = 11, color = "black"),
                  legendgroup = lab, showlegend = FALSE, visible = vis,
                  text = ~hover, hoverinfo = "text")}}

sb <- preds |> distinct(prediction_date, sb_used)

p <- p |>
  add_markers(data = sb, x = ~prediction_date, y = ~sb_used,
              marker = list(color = "grey50", size = 6),
              name = "Silver Bulletin average",
              text = ~paste0("SB average: ", sb_used), hoverinfo = "text") |>
  layout(title = "Our predictions vs. actual polls",
         xaxis = list(title = ""),
         yaxis = list(title = "Trump net approval"),
         legend = list(title = list(text = "Click to show or hide")),
         hovermode = "closest")

saveWidget(p, file.path(getwd(), "docs", "predictions.html"),
           selfcontained = FALSE, libdir = "lib")
