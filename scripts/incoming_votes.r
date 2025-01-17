library(tidyverse)
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

year_range <- unique(votes$year)
year_length <- length(year_range)

colors <- c(rep(glcolors$tan, year_length - 1), glcolors$green)
alphas <- c(rep(.7, year_length - 1), .8)

max_year <- max(year_range)

votecount_g <-
    votes %>%
    slice_max(votesreceived, by = year) %>%
    ggplot(aes(x = year, y = votesreceived, fill = factor(year), alpha = factor(year))) +
    geom_hline(
        yintercept = 120, linewidth = 2,
        linetype = 1, alpha = .5, color = glcolors$dark
    ) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(y = votesreceived + 3, label = votesreceived)) +
    scale_fill_manual(
        values = colors
    ) +
    scale_alpha_manual(
        values = alphas
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        x = NULL, y = NULL,
        title = "Votes received in all board election years"
    )


quorum_g <-
    votes %>%
    nest(data = !year) %>%
    mutate(
        qdays = map_dbl(data, \(dat) approx(dat$voteneeded, dat$daysuntilelection, xout = 0)$y)
    ) %>%
    replace_na(replace = list(qdays = -.01)) %>%
    ggplot(aes(x = year, y = qdays, fill = factor(year), alpha = factor(year))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(y = qdays + .5, label = ceiling(qdays))) +
    scale_x_continuous(
        breaks = scales::pretty_breaks()
    ) +
    scale_fill_manual(
        values = colors
    ) +
    scale_alpha_manual(
        values = alphas
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        x = NULL, y = NULL,
        title = "When was quorum reached (in days before the election)?"
    )

p <- votecount_g / quorum_g

ggsave("graphs/votes_and_quorum.png", width = 7, height = 6, plot = p)