library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

load("Rdata/votes.Rdata")


votes_this_year <-
    votes %>% filter(year == year(today()))

votes_min_max <-
    votes %>%
    filter(year != year(today())) %>%
    group_by(year) %>%
    nest() %>%
    mutate(votemodel = map(
        data,
        ~ lm(votesreceived ~ daysuntilelection, data = .x)
    )) %>%
    mutate(votes = map(
        votemodel,
        ~ broom::augment(.x, newdata = tibble(daysuntilelection = 1:30))
    )) %>%
    unnest(votes) %>%
    select(year, daysuntilelection, votesmodeled = .fitted) %>%
    group_by(daysuntilelection) %>%
    summarize(
        vote_max = max(votesmodeled),
        vote_min = min(votesmodeled),
        .groups = "drop"
    ) %>%
    mutate(across(vote_max:vote_min, ~ ifelse(.x > 0, .x, 0)))


votes_this_year %>%
    ggplot() +
    aes(x = daysuntilelection, y = votesreceived) +
    geom_ribbon(
        data = votes_min_max,
        aes(
            y = NULL,
            ymin = vote_min,
            ymax = vote_max
        ),
        alpha = .2
    ) +
    geom_point() +
    geom_hline(yintercept = 120) +
    scale_x_reverse() +
    labs(x = "Days until election", y = "Votes received")

ggsave("graphs/tracking_vs_average.png", width = 6, height = 4)