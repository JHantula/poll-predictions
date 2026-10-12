# Poll Prediction Pipeline
# Joona Hantula, 10/5/26

library(tidyverse)
library(lubridate)
library(googlesheets4)

# data sources
polls_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS-FKWVTTFtJT6u56e0bqdfoMcXvDO1DUChsJ3jQAMB2lZk2SMqVfmg7dGjclTYkYWz-Pm5lfcLPjp4/pub?output=csv"
get_avg_url <- function() {
  page <- paste(readLines("https://datawrapper.dwcdn.net/kSCt4/", warn = FALSE), collapse = " ")
  version <- str_extract(page, "kSCt4/(\\d+)/", group = 1)
  paste0("https://datawrapper.dwcdn.net/kSCt4/", version, "/dataset.csv")
}

avg_url <- get_avg_url()

# pollster-sponsors and their z values
pollster_list <- tibble(
  pollster = c("Rasmussen Reports", "Morning Consult", "YouGov", "RMG Research", "Ipsos"),
  sponsor  = c(NA, NA, "Economist", "Napolitan News Service", "Reuters"),
  z        = c(0.9, 0.921, 0.967, 0.963, 0.983))

# load and clean data
polls_raw <- read_csv(polls_url) |>
  filter(subgroup == "All polls") |>
  filter(!(pollster == "Morning Consult" & population == "A"))

approval_avg <- read_csv(avg_url)

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

# rolling forecast residuals for one pollster (same logic as run_calibration_weighted)
get_residuals <- function(pollster_data, z) {
  residuals <- numeric()
  for (i in 21:nrow(pollster_data)) {
    prior <- pollster_data[1:(i-1), ]
    days_ago <- as.numeric(pollster_data$middate[i] - prior$middate)
    weights <- z ^ days_ago
    weighted_bias <- sum(weights * prior$error) / sum(weights)
    residuals <- c(residuals, pollster_data$error[i] - weighted_bias)}
  return(residuals)}

# prediction for one pollster
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

# predictions for all five
predictions <- pmap_dfr(pollster_list, function(pollster, sponsor, z) {
  make_prediction(pollster, sponsor, z)})

print(predictions)

sheet_url <- "http://docs.google.com/spreadsheets/d/1S9-qR7ovW30Dkj0YSnK28HdIP4eGZI0H6eFcvdrl5nM/edit?gid=1556615819#gid=1556615819"

# skip pollsters already predicted today, so rerunning doesn't create duplicates
existing <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date))

new_predictions <- predictions |>
  anti_join(existing, by = c("prediction_date", "pollster"))

if (nrow(new_predictions) > 0) {
  sheet_append(sheet_url, new_predictions, sheet = "predictions")}