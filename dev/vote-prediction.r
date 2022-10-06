library(tidyverse)
library(lubridate)
library(patchwork)
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



votemodeldata <-
    votemodels %>%
    mutate(dfs = map(data, ~ nrow(.x) - 2)) %>%
    unnest(dfs) %>%
    mutate(qt = qt(pnorm(1), df = dfs)) %>%
    mutate(params = map(linmod, broom::tidy)) %>%
    unnest(params) %>%
    mutate(
        estimate = abs(estimate),
        .lower = estimate - qt * std.error,
        .upper = estimate + qt * std.error
    ) %>%
    select(-data, -linmod, -statistic, -p.value, -dfs, -std.error, -qt) %>%
    pivot_wider(
        names_from = "term",
        names_sep = "_",
        values_from = c(estimate, .upper, .lower)
    ) %>%
    janitor::clean_names()

(votemodeldata %>%
    ggplot() +
    aes(year, estimate_intercept) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_intercept, ymax = upper_intercept)) +
    geom_hline(yintercept = 120, color = "red") +
    labs(x = "Year", y = "Estimated votes")) +
(votemodeldata %>%
    ggplot() +
    aes(year, estimate_daysuntilelection) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_daysuntilelection, ymax = upper_daysuntilelection)) +
    geom_hline(yintercept = 120 / 30, color = "red") +
    labs(x = "Year", y = "Voting rate"))

ggsave("graphs/vote-diagnostics.png", width = 8, height = 4)