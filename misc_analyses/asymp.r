library(tidyverse)

theme_set(theme_light())
load("Rdata/votes.Rdata")

filter_year <- 2025

mod <- votes %>%
    filter(year == filter_year) %>%
    select(daysuntilelection, votesreceived) %>%
    nls(votesreceived ~ a0 - a1 * exp(-a2 * daysuntilelection),
        start = list(a0 = 120, a1 = 120, a2 = -.1),
        data = .
    )

quorum_date <-
    with(
        mod_data <- broom::augment(mod, newdata = tibble(daysuntilelection = seq(-20, 35, 1))),
        approx(.fitted, daysuntilelection, xout = 120)
    )$y %>%
    round(., digits = 1)

qual <- ifelse(quorum_date > 0, "before", "after")

std_err <- round(broom::glance(mod)$sigma[1], digits = 2)

year_2025 <-
    mod_data %>%
    ggplot(aes(x = daysuntilelection, y = .fitted)) +
    geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
    # geom_point() +
    scale_x_reverse(
        limits = c(35, -7),
        breaks = seq(35, -7, -7)
    ) +
    scale_y_continuous(
        limits = c(0, 140),
        breaks = seq(0, 140, 20)
    ) +
    geom_point(
        data = votes %>% filter(year == filter_year), aes(y = votesreceived),
        shape = 21
    ) +
    geom_hline(yintercept = 120, linewidth = 2, alpha = .1) +
    geom_vline(xintercept = 0, linewidth = 2, alpha = .1) +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = glue::glue(
            "At this voting rate, we'll meet quorum {abs(quorum_date)} days ",
            "{qual} the original meeting date"
        ),
        caption = glue::glue("Exponential decay model, standard error {std_err} votes")
    ) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            size = 15, color = "gray50",
            margin = margin(b = 10)
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    )

ggsave("graphs/asymptotic2025.png",
    width = 5, height = 4,
    plot = year_2025
)


#
# apply to all years
#

all_years <-
    votes %>%
    nest(data = !year) %>%
    mutate(
        mod = map(data, \(dat) {
            nls(votesreceived ~ a0 - a1 * exp(-a2 * daysuntilelection),
                start = list(a0 = 120, a1 = 120, a2 = -.1),
                control = list(warnOnly = TRUE, maxiter = 1000),
                data = dat
            )
        }),
    ) %>%
    mutate(
        fitpoint = map(mod, ~ broom::augment(.x, newdata = tibble(daysuntilelection = seq(-20, 35))))
    ) %>%
    unnest(fitpoint) %>%
    ggplot(aes(x = daysuntilelection, y = .fitted, group = year)) +
    geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
    scale_x_reverse(
        limits = c(35, -7),
        breaks = seq(35, -7, -7)
    ) +
    scale_y_continuous(
        limits = c(0, 170),
        breaks = seq(0, 160, 20)
    ) +
    geom_point(
        data = votes,
        aes(y = votesreceived),
        shape = 21
    ) +
    geom_hline(yintercept = 120, linewidth = 2, alpha = .1) +
    geom_vline(xintercept = 0, linewidth = 2, alpha = .1) +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = "Modeling incoming vote rate",
        caption = "Exponential decay model"
    ) +
    facet_wrap(~year) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            size = 15, color = "gray50",
            margin = margin(b = 10)
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    )

ggsave("graphs/asymptotic.png",
    width = 10, height = 8,
    plot = all_years
)
