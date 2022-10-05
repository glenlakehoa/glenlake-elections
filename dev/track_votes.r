library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/votes.Rdata")

votes

year_range <- unique(votes$year)

track_data_by_year <- function(vote_data, yr) {
    vote <- vote_data %>% filter(year == yr)

    meeting_quorum <- unique(vote$quorum)
    meeting_date <- unique(vote$meetingdate)
    vote_max <- max(vote$votesreceived)

    y_max <- ifelse(vote_max < 160,
        160,
        20 * (vote_max %/% 20 + 1)
    )

    date_labels <- meeting_date - -1:4 * weeks(1)

    votes_required <- ifelse(vote_max >= meeting_quorum,
        NA_real_,
        meeting_quorum - vote_max
    )

    subtitle <- ifelse(is.na(votes_required),
        glue::glue("Quorum has been reached with {vote_max} votes"),
        glue::glue("{votes_required} votes are still needed before ", format(meeting_date, format = "%B %d"), " to reach quorum")
    )

    vote %>%
        ggplot() +
        aes(date, votesreceived) +
        geom_hline(yintercept = meeting_quorum, color = "gray70", size = 1) +
        geom_vline(xintercept = meeting_date, lty = 1, color = "gray70", size = 1) +
        annotate("text",
            x = meeting_date + days(1),
            y = 10,
            label = "Annual Meeting",
            hjust = "left",
            angle = 90
        ) +
        geom_point(size = 2) +
        geom_line(lty = 3) +
        labs(
            x = "Date",
            y = "Votes received",
            title = glue::glue("Glen Lake elections {yr}"),
            subtitle = subtitle
        ) +
        scale_x_date(
            limits = c(min(date_labels), max(date_labels)),
            breaks = date_labels,
            date_label = "%b %d"
        ) +
        scale_y_continuous(
            breaks = seq(0, 484, 20),
            limits = c(0, y_max),
            sec.axis = sec_axis(~ . / meeting_quorum,
                breaks = seq(0, 5, .25),
                name = "Relative to quorum",
                labels = scales::percent_format(accuracy = 1)
            )
        )

    ggsave(glue::glue("graphs/vote-tracking-{yr}.png"), width = 6, height = 5)
}

map(year_range, ~ track_data_by_year(votes, .x))