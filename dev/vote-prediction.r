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
    janitor::clean_names() %>%
    ungroup()

max_votes <-
    votes %>%
    group_by(year) %>%
    summarise(votes = max(votesreceived), .groups = "drop")

quorum_filter <- votes %>%
    group_by(year) %>%
    filter(voteneeded < 0) %>%
    distinct(year) %>%
    pull(year)

quorum_reached <-
    votes %>%
    filter(year %in% quorum_filter) %>%
    group_by(year) %>%
    nest() %>%
    mutate(q_reach = map_dbl(
        data,
        ~ with(
            .x,
            approx(voteneeded, daysuntilelection, 0, na.rm = TRUE)
        )$y
    )) %>%
    ungroup() %>%
    select(year, q_reach)


quorum_model <- votemodeldata %>%
    mutate(
        q_reach = (estimate_intercept - 120) / estimate_daysuntilelection,
        q_high = (upper_intercept - 120) / lower_daysuntilelection,
        q_low = (lower_intercept - 120) / upper_daysuntilelection
    ) %>%
    select(starts_with("q"), year)


(votemodeldata %>%
    ggplot() +
    aes(year, estimate_intercept) +
    geom_point() +
    geom_point(data = max_votes, aes(y = votes), shape = 10, size = 4) +
    geom_errorbar(aes(ymin = lower_intercept, ymax = upper_intercept)) +
    geom_hline(yintercept = 120, color = "red") +
    labs(x = "Year", y = "Estimated votes")) +
(votemodeldata %>%
    ggplot() +
    aes(year, estimate_daysuntilelection) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_daysuntilelection,
                      ymax = upper_daysuntilelection)) +
    geom_hline(yintercept = 120 / 30, color = "red") +
    labs(x = "Year", y = "Voting rate")) /
(quorum_model %>%
    ggplot() +
    aes(year, q_reach) +
    geom_point() +
    geom_errorbar(aes(ymin = q_low, ymax = q_high)) +
    geom_point(data = quorum_reached, shape = 10, size = 4) +
    scale_y_continuous(limits = c(0, NA)) +
    geom_hline(yintercept = 0, color = "red") +
    labs(x = "Year", y = "Quorum reached (in days before election)")
)

ggsave("graphs/vote-diagnostics.png", width = 8, height = 8)