library(tidyverse)
theme_set(theme_light())

load("Rdata/votes.Rdata")


quorum_reached <-
    votes %>%
    group_by(year) %>%
    nest() %>%
    mutate(quorum_reached = map_dbl(
        data,
        ~ approx(.x$voteneeded, .x$daysuntilelection, 0)$y
    )) %>%
    unnest(quorum_reached) %>%
    ungroup()

modelfit <-
    lm(quorum_reached ~ year, data = quorum_reached %>% filter(year > 2018)) %>%
    broom::augment(
        newdata = tibble(year = 2018:2025),
        interval = "confidence"
    ) %>%
    rename(quorum_reached = .fitted)


quorum_reached %>%
    ggplot() +
    aes(x = year, y = quorum_reached) +
    geom_hline(yintercept = 0) +
    geom_line(
        data = modelfit,
        color = "#D3BDA8",
        size = 2, lty = 1, alpha = .4
    ) +
    geom_errorbar(
        data = modelfit,
        aes(ymin = .lower, ymax = .upper),
        width = .2,
        alpha = .8,
        color = "#D3BDA8"
    ) +
    geom_point(color = "#295043", size = 3) +
    labs(
        x = "",
        y = "Quorum reached\n(days before Annual Meeting)",
        caption = glue::glue("Copyright {lubridate::year(lubridate::today())}, Glenlake Upstate Homeowners Assocation, Inc"), # nolint
        title = "Reaching quorum in time is becoming more difficult throughout the years" # nolint
    ) +
    scale_y_continuous(breaks = -100:60) +
    scale_x_continuous(breaks = 2010:2030) +
    coord_cartesian(ylim = c(-2, 10)) +
    theme(
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot"
    )

ggsave("graphs/quorum_reached_prediction.png", width = 7, height = 5)