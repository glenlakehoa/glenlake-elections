library(tidyverse)



voting <-
    read_csv("turnout/2023/voting_export.csv") %>%
    janitor::clean_names() %>%
    group_by(street, vote) %>%
    summarize(n = n(), .groups = "drop")

vote_sum <-
    voting %>%
    pivot_wider(names_from = vote, values_from = n) %>%
    mutate(across(no:yes, ~ifelse(is.na(.x), 0, .x))) %>%
    mutate(
        total = no + yes,
        frac_voted = yes / total
    )

vote_sum %>%
    mutate(quorum = ifelse(frac_voted >= .25, "Made quorum", "No quorum")) %>%
    ggplot() +
    aes(y = fct_reorder(street, total), x = yes) +
    geom_col(aes(x = total), fill = "gray80", alpha = .8) +
    geom_col(fill = "gray30") +
    scale_x_continuous(breaks = seq(0, 80, 10)) +
    labs(y = "", x = "Votes", title = "2023 Board elections turn-out") +
    geom_text(aes(
        color = quorum,
        x = yes + 4, y = street,
        label = scales::percent(frac_voted, accuracy = 1)
    ), show.legend = FALSE) +
    scale_color_manual(values = c(
        "Made quorum" = "darkgreen",
        "No quorum" = "maroon"
    )) +
    theme_light() +
    theme(
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank()
    )

ggsave("gl2023_votingpatterns.png", width = 6, height = 8)