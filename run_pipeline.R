# Poll Prediction Pipeline
# Joona Hantula, 10/5/26

library(readr)
library(dplyr)
library(purrr)
library(stringr)
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

key_path <- Sys.getenv("GS4_KEY")
if (key_path != "") {
  token <- gargle::credentials_service_account(
    path = key_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets")
  gs4_auth(token = token)
} else {
  gs4_auth()
}

sheet_url <- "http://docs.google.com/spreadsheets/d/1S9-qR7ovW30Dkj0YSnK28HdIP4eGZI0H6eFcvdrl5nM/edit?gid=1556615819#gid=1556615819"

# skip pollsters already predicted today, so rerunning doesn't create duplicates
existing <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date))

new_predictions <- predictions |>
  anti_join(existing, by = c("prediction_date", "pollster"))

if (nrow(new_predictions) > 0) {
  sheet_append(sheet_url, new_predictions, sheet = "predictions")}

score_against <- "release"

# every poll from the five pollsters
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

# predictions logged so far
all_predictions <- read_sheet(sheet_url, sheet = "predictions") |>
  mutate(prediction_date = as.Date(prediction_date))

# skip polls that are scored already
existing_results <- read_sheet(sheet_url, sheet = "results")
if (nrow(existing_results) > 0) {
  existing_results <- existing_results |>
    mutate(startdate = as.Date(startdate), enddate = as.Date(enddate))
  actual_polls <- anti_join(actual_polls, existing_results,
                            by = c("pollster", "sponsor", "startdate", "enddate"))}

# matching each new poll to its most recent prediction then score
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
         scored_date = Sys.Date()) |>
  select(pollster, sponsor, startdate, enddate, release_date, poll_net,
         prediction_date, point_estimate, lower_50, upper_50, lower_90, upper_90,
         error, abs_error, in_50, in_90, scored_date)

# write to the results tab (headers on the first write, append after that)
if (nrow(new_results) > 0) {
  if (nrow(existing_results) == 0) {
    sheet_write(new_results, ss = sheet_url, sheet = "results")
  } else {
    sheet_append(sheet_url, new_results, sheet = "results")}}

print(new_results)
