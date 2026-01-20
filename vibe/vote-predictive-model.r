# Load required libraries
library(jsonlite)
library(ggplot2)
library(dplyr)
library(forecast)

# Function to read a single votes JSON file
read_votes_file <- function(file_path) {
  json_data <- fromJSON(file_path)
  votes_data <- json_data$votes
  if (is.data.frame(votes_data)) {
    if (nrow(votes_data) == 0) {
      votes_df <- data.frame(date = as.Date(character()), votes = numeric(), year = integer(), meetingdate = as.Date(character()))
    } else {
      votes_df <- votes_data
      votes_df$date <- as.Date(votes_df$date)
      votes_df$votes <- as.numeric(votes_df$votes)
      votes_df$year <- json_data$year
      votes_df$meetingdate <- as.Date(json_data$meetingdate)
    }
  } else if (is.list(votes_data)) {
    if (length(votes_data) == 0) {
      votes_df <- data.frame(date = as.Date(character()), votes = numeric(), year = integer(), meetingdate = as.Date(character()))
    } else {
      votes_df <- data.frame(
        date = sapply(votes_data, `[[`, "date"),
        votes = sapply(votes_data, `[[`, "votes")
      )
      votes_df$year <- json_data$year
      votes_df$meetingdate <- as.Date(json_data$meetingdate)
    }
  } else {
    stop("Unexpected votes structure")
  }
  return(votes_df)
}

# List all votes JSON files in sources/ folder
files <- list.files("sources/", pattern = "votes-.*\\.json$", full.names = TRUE)

# Read all files and combine
all_votes <- do.call(rbind, lapply(files, read_votes_file))

# Convert dates
all_votes$date <- as.Date(all_votes$date)
all_votes$meetingdate <- as.Date(all_votes$meetingdate)

# Calculate days to election (positive means days before election)
all_votes$days_to_election <- as.numeric(all_votes$meetingdate - all_votes$date)

# Get final votes for each year (the votes on election day)
years <- unique(all_votes$year)
final_votes_list <- lapply(years, function(y) {
  subset_data <- all_votes[all_votes$year == y & all_votes$days_to_election == 0, ]
  if (nrow(subset_data) > 0) {
    data.frame(year = y, final_votes = subset_data$votes[1])
  } else {
    NULL
  }
})
final_votes_df <- do.call(rbind, final_votes_list)

# Merge final votes back to all_votes
all_votes$final_votes <- final_votes_df$final_votes[match(all_votes$year, final_votes_df$year)]

# Remove rows where final_votes is NA
all_votes <- all_votes[!is.na(all_votes$final_votes), ]

# Build ARIMA model for each year separately
models_list <- list()
for (y in unique(all_votes$year)) {
  subset_data <- all_votes[all_votes$year == y, ] %>% arrange(date)
  votes_ts <- ts(subset_data$votes, start = 1, frequency = 1)
  models_list[[as.character(y)]] <- auto.arima(votes_ts)
}

# Get predictions for each past year using ARIMA (train on all but last point, predict last)
year_predictions <- data.frame()
for (y in unique(all_votes$year)) {
  subset_data <- all_votes[all_votes$year == y, ] %>% arrange(date)
  n <- nrow(subset_data)
  if (n > 1) {
    train_ts <- ts(subset_data$votes[1:(n-1)], start = 1, frequency = 1)
    fit <- auto.arima(train_ts)
    fc <- forecast(fit, h = 1)
    predicted_final <- fc$mean[1]
    actual_final <- subset_data$votes[n]
    year_predictions <- rbind(year_predictions, data.frame(year = y, actual_final = actual_final, predicted_final = predicted_final))
  } else {
    # If only one point, predicted = actual
    year_predictions <- rbind(year_predictions, data.frame(year = y, actual_final = subset_data$votes[1], predicted_final = subset_data$votes[1]))
  }
}

# Plot predicted vs actual for past years
p2 <- ggplot(year_predictions, aes(x = actual_final, y = predicted_final)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual Final Votes for Past Years", x = "Actual Final Votes", y = "Predicted Final Votes") +
  theme_minimal()

# Save the plot
ggsave("predicted_vs_actual.png", plot = p2)

cat("Predicted vs actual plot saved as predicted_vs_actual.png\n")

# For prediction example: predict for 2026 using current data
# Current date: 2026-01-19, election: 2026-02-07
current_date <- as.Date("2026-01-19")
election_date <- as.Date("2026-02-07")
days_left <- as.numeric(election_date - current_date)

# Read 2026 data separately for current votes
votes_2026_raw <- read_votes_file("sources/votes-2026.json")
if (nrow(votes_2026_raw) > 0) {
  # Get the latest votes before or on current date
  recent_votes <- votes_2026_raw[votes_2026_raw$date <= current_date, ] %>% arrange(date)
  if (nrow(recent_votes) > 0) {
    votes_ts <- ts(recent_votes$votes, start = 1, frequency = 1)
    fit <- auto.arima(votes_ts)
    fc <- forecast(fit, h = days_left)
    predicted_final <- fc$mean[days_left]
    cat("Predicted final votes for 2026:", round(predicted_final), "\n")
    # Add shifted_date for scaling
    target_meeting <- as.Date("2026-02-07")
    recent_votes$shifted_date <- recent_votes$date + (target_meeting - election_date)
  } else {
    cat("No current votes data available for 2026\n")
  }
} else {
  cat("No votes data available for 2026\n")
}

# Prepare data for plotting
plot_data <- final_votes_df
plot_data$type <- "Actual"
predicted_row <- data.frame(year = 2026, final_votes = round(predicted_final), type = "Predicted")
plot_data <- rbind(plot_data, predicted_row)

# Plot
p <- ggplot(plot_data, aes(x = year, y = final_votes, color = type)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Glenlake HOA Election Final Votes", x = "Year", y = "Final Votes", color = "Type") +
  theme_minimal()

# Save the plot
ggsave("election_votes_plot.png", plot = p)

cat("Plot saved as election_votes_plot.png\n")

# Plot voting progression over time for each year, scaled to 2026 timeline
target_meeting <- as.Date("2026-02-07")
all_votes$shifted_date <- all_votes$date + (target_meeting - all_votes$meetingdate)

# Get fitted values for each year's model
fitted_data <- data.frame()
for (y in unique(all_votes$year)) {
  subset <- all_votes[all_votes$year == y, ] %>% arrange(date)
  fit <- models_list[[as.character(y)]]
  fitted_vals <- as.numeric(fitted(fit))
  fitted_data <- rbind(fitted_data, data.frame(shifted_date = subset$shifted_date, votes = fitted_vals, year = y, type = "fitted"))
}

# For 2026, get forecast
forecast_data <- data.frame()
if (exists("fc") && nrow(recent_votes) > 0) {
  forecast_dates <- seq(from = max(recent_votes$shifted_date) + 1, by = 1, length.out = days_left)
  forecast_data <- data.frame(shifted_date = forecast_dates, votes = as.numeric(fc$mean), year = 2026, type = "forecast")
}

# Combine data for plotting
plot_data <- all_votes %>% select(shifted_date, votes, year) %>% mutate(type = "actual")
plot_data <- rbind(plot_data, fitted_data, forecast_data)

p3 <- ggplot(plot_data, aes(x = shifted_date, y = votes, color = factor(year), linetype = type)) +
  geom_line() +
  geom_point(data = subset(plot_data, type == "actual")) +
  labs(title = "Voting Progression Over Time (Scaled to 2026 Timeline)", x = "Shifted Date", y = "Votes", color = "Year", linetype = "Type") +
  theme_minimal()

ggsave("voting_progression_scaled.png", plot = p3)

cat("Scaled voting progression plot saved as voting_progression_scaled.png\n")