# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
theme_set(theme_classic())

# define constants
source("00-defaults.r")
# source("01-import.r")
load("Rdata/votes.Rdata")

# votes <- votes %>% filter(year != 2023) %>% mutate(year = factor(year, levels = c("2018", "2019", "2020","2021","2022"))) %>%
#     filter(date < as.Date("2022-01-28"))

year_length <- length(unique(votes$year))

current_year <- last(votes$year)
prev_years <- paste0(
    first(votes$year),
    "-",
    last(votes$year[votes$year != current_year])
)


colors <- c(rep("gray70", year_length - 1), "gray30")
lines <- c(rep(1, year_length - 1), 1)
alpha <- c(rep(.3, year_length - 1), 1)

votes %>%
    arrange(date) %>%
    mutate(label = case_when(
        date == max(votes$date) ~ votesreceived,
        TRUE ~ NA_real_
    )) %>%
    ggplot() +
    aes(x = daysuntilelection, votesreceived, alpha = year, color = year, linetype = year) +
    geom_line(show.legend = FALSE) +
    ggrepel::geom_label_repel(aes(label = label), show.legend = FALSE) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = lines) +
    scale_alpha_manual(values = alpha) +
    scale_x_reverse(breaks = seq(0, 30, 5)) +
    scale_y_continuous(breaks = seq(0, 200, 50)) +
    labs(
        x = "Days until the election",
        y = "Number of votes received",
        title = glue::glue("Number of votes received in {current_year} compared to {prev_years}")
    ) +
    geom_hline(yintercept = 120, lty = 2, color = "gray50") +
    geom_vline(xintercept = 0, lty = 2, color = "gray50") +
    annotate("text", x = 30, y = 124, label = "120 votes needed for quorum", hjust = "left")

ggsave("graphs/incoming-vote-comparison.png", width = 8, height = 5)

#
# predictive analytics
#
#

modeled_votes <-
    votes %>%
    mutate(daysuntilelection = as.numeric(daysuntilelection)) %>%
    group_by(year) %>%
    nest() %>%
    mutate(
        models = map(
            data,
            ~ lm(votesneeded ~ daysuntilelection, data = .x)
        ),
        modeldata = map(
            models,
            ~ broom::augment(.x, newdata = tibble(daysuntilelection = -10:30))
        ),
        params = map(
            models,
            ~ broom::tidy(.x)
        ),
        qual = map_dbl(
            seq_along(models),
            ~ modelr::rsquare(models[[.x]], data[[.x]])
        )
    )

modeled_votes %>%
    unnest(modeldata)

modeled_votes %>%
    ungroup() %>%
    unnest(params) %>%
    mutate(term = case_when(
        term == "(Intercept)" ~ "intercept",
        term == "daysuntilelection" ~ "daysuntilelection"
    )) %>%
    select(year, term, estimate) %>%
    mutate(
        expected_votes = ifelse(term == "intercept", 120 - estimate, NA_real_),
        votes_per_day = ifelse(term == "daysuntilelection", estimate, NA_real_)
    ) %>%
    select(year, expected_votes, votes_per_day) %>%
    pivot_longer(-year) %>%
    drop_na() %>%
    knitr::kable()


modeled_votes %>%
    ungroup() %>%
    unnest(modeldata) %>%
    ggplot() + aes(daysuntilelection, .fitted, color = year) +
    geom_line(show.legend = FALSE) +
    scale_x_reverse() +
    geom_hline(yintercept = 0)