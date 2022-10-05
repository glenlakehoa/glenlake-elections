library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/votes.Rdata")


votemodels <-
    votes %>%
    group_by(year) %>%
    nest() %>%
    mutate(linmod = map(
        data,
        ~ lm(votesreceived ~ daysuntilelection, data = .)
    ))


predictions <-
    votemodels %>%
    mutate(predictions = map(
        linmod,
        ~ broom::augment(.x,
            newdata = tibble(daysuntilelection = 0:28),
            interval = "confidence"
        )
    )) %>%
    unnest(predictions)


predictions %>%
    ggplot() +
    aes(daysuntilelection, .fitted) +
    geom_line() +
    geom_hline(yintercept = 120, color = "gray70") +
    geom_point(data = votes, aes(y = votesreceived)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
    scale_x_reverse() +
    facet_wrap(~year) +
    coord_cartesian(ylim = c(0, 180))

ggsave("graphs/vote-predictions.png", width = 6, height = 6)