library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/votes.Rdata")

year_range <- unique(votes$year)
year_length <- length(year_range)

colors <- c(rep("gray70", year_length - 1), "gray30")
alphas <- c(rep(.2, year_length - 1), .8)

max_year <- max(year_range)

votes %>%
    ggplot() +
    aes(daysuntilelection,
        votesreceived,
        color = factor(year),
        alpha = factor(year)
    ) +
    scale_x_reverse(breaks = seq(0, 49, 7)) +
    scale_y_continuous(breaks = seq(0, 484, 20)) +
    geom_hline(yintercept = 120, alpha = .8, lty = 1, color = "gray70") +
    geom_line(show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = alphas) +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = glue::glue("Glen Lake elections {max_year}"),
        subtitle = "Comparison of this year with previous years"
    )

ggsave("graphs/vote-comparison.png", width = 6, height = 4)