library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load default colors
source("defaults.r")

# Set working directory to the project root
setwd("c:/Users/robha/Documents/programming/R/glenlake-elections")

# Get list of votes JSON files
files <- list.files("sources/", pattern = "votes-.*\\.json$", full.names = TRUE)

# Initialize empty data frame for board data
board_data <- data.frame(name = character(),
                         tenure_start = as.Date(character()),
                         tenure_end = as.Date(character()),
                         stringsAsFactors = FALSE)

# Read each file and extract board information
for (file in files) {
  data <- fromJSON(file)

  if (!is.null(data$board) && length(data$board) > 0) {
    board <- data$board

    # Convert tenure dates
    board$tenure_start <- ymd(as.character(board$tenure_start))
    board$tenure_end <- as.character(board$tenure_end)
    board$tenure_end <- ifelse(is.na(board$tenure_end) | board$tenure_end %in% c("null", "NULL", "", NA),
                               Sys.Date(), ymd(board$tenure_end))
    board$tenure_end <- as.Date(board$tenure_end)

    # Add to main data frame
    board_data <- bind_rows(board_data, board)
  }
}

# Calculate total tenure for each person
tenure_summary <- board_data %>%
  mutate(duration_days = as.numeric(tenure_end - tenure_start)) %>%
  group_by(name) %>%
  summarize(total_days = sum(duration_days, na.rm = TRUE)) %>%
  mutate(total_years = total_days / 365.25) %>%
  arrange(desc(total_years))

# Create the tenure overview plot
ggplot(tenure_summary, aes(x = total_years, y = reorder(name, total_years))) +
  geom_col(fill = glcolors$green) +
  labs(title = "Total Board Tenure by Member",
       x = "Total Years Served",
       y = "Board Member") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("board_tenure_overview.png", width = 10, height = 12, dpi = 300)

# Also print a summary table
print("Board Tenure Summary (Top 10):")
tenure_summary %>%
  head(10) %>%
  mutate(total_years = round(total_years, 2)) %>%
  print()