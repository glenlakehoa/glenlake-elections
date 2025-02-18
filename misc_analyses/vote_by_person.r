library(tidyverse)
library(patchwork)
library(lubridate)


load("Rdata/vote_results.Rdata")
load("Rdata/votes.Rdata")


max_votes <-
    slice_max(votes, votesreceived, by = year, n = 1) %>%
    select(year, votesreceived)

meetingdates <- board_tenure_dates %>%
    distinct(meetingdate) %>%
    drop_na() %>%
    pull(meetingdate)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.caption = ggtext::element_markdown(hjust = 0, size = 6),
            plot.caption.position = "plot",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.ticks = element_blank()
        )
)

glcolors <- list(green = "#295043", brown = "#D3BDA8")

set.seed(23423637)

vote_results %>%
    arrange(year, desc(votes)) %>%
    group_by(year) %>%
    mutate(
        y = rank(-votes),
        elected = case_when(
            year == 2018 & y <= 4 ~ "elected",
            year == 2018 & y <= 7 ~ "elected1yr",
            year %% 2 == 0 & y <= 4 ~ "elected",
            year %% 2 == 1 & y <= 3 ~ "elected",
            TRUE ~ "notelected"
        )
    ) %>%
    inner_join(max_votes) %>%
    ggplot(aes(x = year, y = votes, color = elected)) +
    geom_vline(xintercept = seq(2017.5, 2025.5, 1), , alpha = .25, linewidth = 2, color = "gray60") +
    geom_jitter(size = 5, alpha = .4, width = .25, show.legend = FALSE) +
    scale_color_manual(
        values = c("elected1yr" = "darkgreen", elected = "dodgerblue", "notelected" = "red")
    ) +
    labs(
        x = NULL,
        y = "Votes received",
        title = "How competitive are the board elections?"
    )


set.seed(23423637)

vote_results %>%
    arrange(year, desc(votes)) %>%
    group_by(year) %>%
    mutate(
        y = rank(-votes),
        elected = case_when(
            year == 2018 & y <= 4 ~ "elected",
            year == 2018 & y <= 7 ~ "elected1yr",
            year %% 2 == 0 & y <= 4 ~ "elected",
            year %% 2 == 1 & y <= 3 ~ "elected",
            TRUE ~ "notelected"
        )
    ) %>%
    inner_join(max_votes) %>%
    ggplot(aes(x = year, y = votes / votesreceived, color = elected)) +
    geom_vline(xintercept = seq(2017.5, 2025.5, 1), , alpha = .25, linewidth = 2, color = "gray60") +
    geom_jitter(size = 5, alpha = .4, width = .35, show.legend = FALSE) +
    scale_y_continuous(
        # limit = c(0, 1),
        breaks = seq(0, 1, .2),
        labels = scales::label_percent()
    ) +
    coord_cartesian(
        ylim = c(0, 1),
        clip = "off"
    ) +
    scale_color_manual(
        values = c("elected1yr" = "darkgreen", elected = "dodgerblue", "notelected" = "red")
    ) +
    labs(
        x = NULL,
        y = "Fraction of votes received",
        title = "How competitive are the board elections?",
        caption = glue::glue(
            "<span style = 'color: dodgerblue'>Blue</span>: elected for 2-year term;<BR>",
            "<span style = 'color: darkgreen'>Green</span>: elected for 1-year term;<BR>",
            "<span style = 'color: red'>Red</span>: not enough votes to be elected"
        )
    )

ggsave("misc_analyses/vote_competition.png", width = 8, height = 5)
