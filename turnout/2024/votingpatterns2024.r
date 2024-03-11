library(tidyverse)
library(patchwork)

voting <-
    read_csv("turnout/2024/turnout-2024.csv") %>%
    janitor::clean_names() %>%
    mutate(street = str_sub(property_address, 5, -1)) %>%
    group_by(street, vote) %>%
    summarize(n = n(), .groups = "drop") # %>%
# pivot_wider(names_from = "vote", values_from = "n")

voted_cleaned <-
    voting %>%
    mutate(street = case_when(
        street == "Dewfiled Lane" ~ "Dewfield Lane",
        street == "Bridgeville" ~ "Bridgeville Way",
        street == "Delaney Court" ~ "Delany Court",
        street == "Ethridge Pointe" ~ "Ethridge Point",
        street == "Grays Harbor Court" ~ "Grays Harbour Court",
        street == "Hamilton Gaines Ct" ~ "Hamilton Gaines Court",
        street == "Seymour Drive" ~ "Seymour Court",
        street == "Shoreline Drive" ~ "Shoreline Blvd",
        street == "Shoreline Boulevard" ~ "Shoreline Blvd",
        street == "Dunlieth Court" ~ "Dunleith Court",
        street == "Vadenburg Drive" ~ "Vandenburg Drive",
        street == "Vandenburg Court" ~ "Vandenburg Drive",
        street == "Vanderburg Drive" ~ "Vandenburg Drive",
        street == "Witherspon Court" ~ "Witherspoon Court",
        street == "Walkers Bluff Drive" ~ "Walkers Bluff Road",
        street == "Walkers Bluff" ~ "Walkers Bluff Road",
        TRUE ~ street
    )) %>%
    summarize(
        n = sum(n),
        .by = c("street", "vote")
    ) %>%
    pivot_wider(names_from = "vote", values_from = "n") %>%
    replace_na(list("no" = 0, "yes" = 0)) # %>%


# mutate(streetabb = str_remove_all(street, ".(Road|Way|Court|Street|Lane|Point|Rd|Ln|Blvd|Drive)")) %>%
# full_join(read_csv("glenlakehomes.csv"), by = c("streetabb" = "streetname"))


vote_sum <-
    voted_cleaned %>%
    arrange(street) %>%
    mutate(
        total = yes + no,
        frac_voted = yes / total
    )

by_absvote_g <-
    vote_sum %>%
    mutate(quorum = ifelse(frac_voted >= .25, "Made quorum", "No quorum")) %>%
    ggplot() +
    aes(y = fct_reorder(street, yes), x = yes) +
    geom_col(aes(x = total), fill = "gray80", alpha = .8) +
    geom_col(fill = "gray30") +
    scale_x_continuous(breaks = seq(0, 80, 10)) +
    labs(y = "", x = "Votes") +
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

by_frac_g <- vote_sum %>%
    mutate(quorum = ifelse(frac_voted >= .25, "Made quorum", "No quorum")) %>%
    ggplot() +
    aes(y = fct_reorder(street, frac_voted), x = yes) +
    geom_col(aes(x = total), fill = "gray80", alpha = .8) +
    geom_col(fill = "gray30") +
    scale_x_continuous(breaks = seq(0, 80, 10)) +
    labs(y = "", x = "Votes") +
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


ggsave("turnout/graphs/gl2024_votingpatterns.png",
    width = 12, height = 8,
    plot = by_absvote_g + by_frac_g +
        plot_annotation(
            title = "2024 Board elections turn-out",
            caption = "Counts based on votes received on Feb 9, 2024"
        )
)
