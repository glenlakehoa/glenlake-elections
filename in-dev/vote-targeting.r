library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light() +
    theme(panel.grid.minor = element_blank()))

load("Rdata/votes.Rdata")


votemodels <-
    votes %>%
    group_by(year) %>%
    nest() %>%
    mutate(linmod = map(
        data,
        ~ lm(votesreceived ~ daysuntilelection, data = .)
    ))

vote_predict_mods <-
    votemodels %>%
    mutate(mods = map(linmod, broom::tidy)) %>%
    mutate(dats = map(
        linmod, ~ broom::augment(.x, newdata = tibble(daysuntilelection = -20:20), interval = "prediction")
    )) 


targets <-
    vote_predict_mods %>% 
    unnest(dats) %>%
    select(-data) %>%
    nest(d = !c(year, mods)) %>%
    mutate(
        dayzero = map(d, 
        ~with(.x, approx(.fitted, daysuntilelection, xout = 120))$y)
    ) %>%
    mutate(
        day120 = map(d, 
        ~with(.x, approx(daysuntilelection, .fitted, , xout = 0))$y)
    ) %>%
    unnest(day120) %>%
    unnest(dayzero)


targets %>%
    ggplot(
        aes(x = day120, y = dayzero, label = year)
    ) + 
    geom_point() + 
    geom_label() + 
    coord_cartesian(
        xlim = c(0, NA),
        ylim = c(0, NA)
    ) +
    scale_x_continuous(
        breaks = seq(0, 480, 20),
    ) + 
    scale_y_continuous(
        breaks = seq(0, 30, 3)
    ) +
    labs(
        x = "Predicted number of votes at Annual Meeting",
        y = "Reach quorum, in days before the Annual Meeting"
    ) +
    geom_vline(xintercept = 120, color = "darkgreen", linewidth = 2, alpha = .5) +
    geom_hline(yintercept = 0, color = "darkgreen", linewidth = 2, alpha = .5)