library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load default colors
source("defaults.r")

# Set working directory to the project root
# setwd("c:/Users/robha/Documents/programming/R/glenlake-elections")

# Get list of votes JSON files
files <- list.files("sources/", pattern = "votes-.*\\.json$", full.names = TRUE)
# files <- files[2:7]
# Initialize empty data frame for board data
board_data <- data.frame(name = character(),
                         tenure_start = as.Date(character()),
                         tenure_end = as.Date(character()),
                         stringsAsFactors = FALSE)

# Initialize vector for meeting dates
meeting_dates <- as.Date(character())

# Initialize data frame for election participants
participants <- data.frame(date = as.Date(character()), name = character(), stringsAsFactors = FALSE)
# file = files[2]
# Read each file and extract board information
for (file in files) {
  data <- fromJSON(file)

  # Collect meeting date if present
  if (!is.null(data$meetingdate) && !is.na(data$meetingdate)) {
    meeting_date <- ymd(as.character(data$meetingdate))
    meeting_dates <- c(meeting_dates, meeting_date)

    # Collect elected winners (board members who started on election date)
    if (!is.null(data$board) && length(data$board) > 0) {
      elected <- data$board %>%
        mutate(tenure_start_date = ymd(as.character(tenure_start))) %>%
        filter(tenure_start_date == meeting_date) %>%
        pull(name)
      if (length(elected) > 0) {
        participants <- bind_rows(participants, data.frame(date = meeting_date, name = elected, stringsAsFactors = FALSE))
      }
    }
  }

  if (!is.null(data$board) && length(data$board) > 0) {
    board <- data$board

    # Mark ongoing tenures
    board$is_ongoing <- is.na(board$tenure_end) | board$tenure_end %in% c("null", "NULL", "", NA)

    # Convert tenure dates
    board$tenure_start <- ymd(as.character(board$tenure_start))
    board$tenure_end <- ifelse(board$is_ongoing, today(), ymd(as.character(board$tenure_end)))
    board$tenure_end <- as.Date(board$tenure_end)

    # Add to main data frame
    board_data <- bind_rows(board_data, board)
  }
}

# Sort by name and start date
board_data <- board_data %>%
  arrange(name, tenure_start)

# Order names by their earliest tenure start date for the plot
name_order <- board_data %>%
  group_by(name) %>%
  summarize(min_start = min(tenure_start)) %>%
  arrange(min_start) %>%
  pull(name)

board_data$name <- factor(board_data$name, levels = rev(name_order))

# Set factor levels for participants to match board_data
participants$name <- factor(participants$name, levels = levels(board_data$name))

# Remove NA or empty names from participants
participants <- participants %>% filter(!is.na(name) & name != "" & trimws(name) != "")

# Filter out board positions before 2018 elections
board_data <- board_data %>% filter(tenure_start >= ymd("2018-01-01"))

# Sort meeting dates
meeting_dates <- sort(meeting_dates)

# Calculate statistics
total_positions <- nrow(board_data)

# For each tenure, find the next election date
board_data$next_election <- sapply(board_data$tenure_start, function(start) {
  idx <- findInterval(start, meeting_dates) + 1
  if (idx <= length(meeting_dates)) meeting_dates[idx] else NA
})

resigned_prematurely <- sum(!board_data$is_ongoing & !is.na(board_data$next_election) & 
                           board_data$tenure_end < board_data$next_election)

appointed_midterm <- sum(!board_data$tenure_start %in% meeting_dates)
currently_serving <- sum(board_data$is_ongoing)

# Create the timeline plot
ggplot(board_data, aes(x = tenure_start, xend = tenure_end, y = name, yend = name)) +
  geom_segment(size = 3, color = glcolors$green) +
  geom_vline(xintercept = meeting_dates, linetype = "dashed", color = glcolors$dark, alpha = 0.7) +
  geom_point(data = participants, aes(x = date, y = name), inherit.aes = FALSE, shape = 21, fill = glcolors$tan, color = "black", size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Glenlake HOA Board Member Timeline",
       subtitle = paste0("Total board positions: ", total_positions, 
                        " | Resigned prematurely: ", resigned_prematurely, 
                        " | Appointed mid-term: ", appointed_midterm, 
                        " | Currently serving: ", currently_serving),
       x = "Date",
       y = "Board Member") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

# Save the plot
ggsave("board_timeline.png", width = 12, height = 8, dpi = 300)