library(tidyverse)
library(lubridate)
library(patchwork)


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

high_votes <- as.list(votes[nrow(votes), c("votesreceived", "daysuntilelection")])

ranked <- votes %>%
    nest(data = -year) %>%
    mutate(
        estimate_votes =
            map_dbl(data, \(d) {
                approx(d$daysuntilelection, d$votesreceived, xout = high_votes$daysuntilelection)$y
            })
    ) %>%
    replace_na(list(estimate_votes = 0)) %>%
    pull(estimate_votes)

# ranks = rank(-ranks)

label_data <-
    tibble(
        daysuntilelection = high_votes$daysuntilelection, votesreceived = ranks, year = year_range,
        labels = rank(-ranked)
    )


year_range <- unique(votes$year)
year_length <- length(year_range)

colors <- c(rep(glcolors$tan, year_length - 1), glcolors$green)
text_colors <- c(rep("black", year_length - 1), "white")
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
    ) +
    geom_point(
        data = label_data,
        size = 4, show.legend = FALSE, alpha = 1
    ) +
    geom_text(
        data = label_data, aes(label = labels), color = I(text_colors),
        size = 2, show.legend = FALSE, alpha = 1
    )

ggsave("graphs/vote-comparison2.png",
    width = 6, height = 4,
    plot = comp_g
)
