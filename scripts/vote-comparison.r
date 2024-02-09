library(tidyverse)
library(lubridate)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.caption.position = "plot",
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank()
        )
)

source("defaults.r")
load("Rdata/votes.Rdata")

year_range <- unique(votes$year)
year_length <- length(year_range)

colors <- c(rep(glcolors$tan, year_length - 1), glcolors$green)
alphas <- c(rep(.5, year_length - 1), .8)

max_year <- max(year_range)

comp_g <-
    votes %>%
    ggplot() +
    aes(daysuntilelection,
        votesreceived,
        color = factor(year),
        alpha = factor(year)
    ) +
    scale_x_reverse(breaks = seq(0, 49, 7)) +
    scale_y_continuous(breaks = seq(0, 484, 20)) +
    geom_hline(yintercept = 120, alpha = .8, lty = 1, color = glcolors$green) +
    geom_line(show.legend = FALSE) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = alphas) +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = glue::glue("Glen Lake elections {max_year}"),
        subtitle = glue::glue("Comparison of {max_year} with previous years")
    )

ggsave("graphs/vote-comparison.png",
    width = 6, height = 4,
    plot = comp_g
)
