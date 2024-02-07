library(tidyverse)

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

records <- votes %>%
    group_by(year) %>%
    mutate(maxvotes = max(votesreceived)) %>%
    nest(data = !c("year", "maxvotes")) %>%
    mutate(quorum_reached = map_dbl(
        data,
        ~ approx(.x$voteneeded, .x$daysuntilelection, 0)$y
    )) %>%
    unnest(quorum_reached) %>%
    ungroup()

records %>%
    ggplot(aes(x = maxvotes, y = quorum_reached, label = year)) + 
    geom_text() + 
    coord_cartesian(
        xlim = c(120, NA),
        ylim = c(0, NA)
    ) + 
    scale_x_continuous(
        breaks = seq(120, 240, 10)
    ) + 
    scale_y_continuous(
        breaks = seq(0, 10, 2)
    ) + 
    labs(
        x = "Total number of votes",
        y = "Quorum reached (in days before meeting)"
    ) + 
    geom_vline(xintercept = 120, color = glcolors$green) +
    geom_hline(yintercept = 0, color = glcolors$green)