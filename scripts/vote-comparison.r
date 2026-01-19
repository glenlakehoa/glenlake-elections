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

year_range <- unique(votes$year)
year_length <- length(year_range)

colors <- c(rep(glcolors$tan, year_length - 1), glcolors$green)
text_colors <- c(rep(glcolors$dark, year_length - 1), glcolors$light)
alphas <- c(rep(.5, year_length - 1), .8)

max_year <- max(year_range)

label_data <-
    tibble(
        daysuntilelection = high_votes$daysuntilelection, votesreceived = ranked, year = year_range,
        labels = rank(-ranked)
    ) %>%
    mutate(
        label = case_when(
            labels %in% 1:3 ~ paste0(year, ": ", labels),
            TRUE ~ as.character(labels)
        ),
        hjust = case_when(
            labels %in% 1:3 ~ 1,
            TRUE ~ 0.5
        )
    )

comp_g <-
    votes %>%
    ggplot(
        aes(daysuntilelection,
            votesreceived,
            color = factor(year),
            alpha = factor(year)
        )
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

comp_g_rank <-
    comp_g +
    geom_vline(
        xintercept = high_votes$daysuntilelection,
        color = glcolors$green, linewidth = 1, alpha = .2
    ) +
    geom_point(
        data = label_data,
        size = 4, alpha = 1,
        show.legend = FALSE
    ) +
    geom_text(
        data = label_data, aes(label = label, hjust = I(hjust)),
        color = I(text_colors), size = 2, alpha = 1,
        show.legend = FALSE
    )

ggsave("graphs/vote-comparison_rank.png",
    width = 6, height = 4,
    plot = comp_g_rank
)
