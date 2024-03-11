library(tidyverse)

# voting <-
#     read_csv("votingpatterns.csv") %>%
#     janitor::clean_names() %>%
#     mutate(street = case_when(
#         street == "Dewfiled Lane" ~ "Dewfield Lane",
#         street == "Bridgeville" ~ "Bridgeville Way",
#         street == "Delaney Court" ~ "Delany Court",
#         street == "Ethridge Pointe" ~ "Ethridge Point",
#         street == "Grays Harbor Court" ~ "Grays Harbour Court",
#         street == "Hamilton Gaines Ct" ~ "Hamilton Gaines Court",
#         street == "Seymour Drive" ~ "Seymour Court",
#         street == "Shoreline Drive" ~ "Shoreline Blvd",
#         street == "Shoreline Boulevard" ~ "Shoreline Blvd",
#         street == "Dunlieth Court" ~ "Dunleith Court",
#         street == "Vadenburg Drive" ~ "Vandenburg Drive",
#         street == "Vandenburg Court" ~ "Vandenburg Drive",
#         street == "Vanderburg Drive" ~ "Vandenburg Drive",
#         street == "Witherspon Court" ~ "Witherspoon Court",
#         street == "Walkers Bluff Drive" ~ "Walkers Bluff Road",
#         street == "Walkers Bluff" ~ "Walkers Bluff Road",
#         TRUE ~ street
#     )) %>%
#     arrange(street) %>%
#     mutate(voted = case_when(is.na(vote) ~ "no_vote", vote == "x" ~ "vote")) %>%
#     select(-vote)


voting <-
    read_csv("turnout/2023/corrected_votingpatterns.csv") %>%
    group_by(street, voted) %>%
    summarize(n = n(), .groups = "drop")

vote_sum <-
    voting %>%
    pivot_wider(names_from = voted, values_from = n) %>%
    arrange(street) %>%
    mutate(votee = case_when(is.na(vote) ~ 0, TRUE ~ 1)) %>%
    mutate(vote2 = case_when(is.na(vote) ~ votee, TRUE ~ vote * votee)) %>%
    select(-vote, -votee) %>%
    rename(vote = vote2) %>%
    mutate(
        total = vote + no_vote,
        frac_voted = vote / total
    )

vote_sum %>%
    mutate(quorum = ifelse(frac_voted >= .25, "Made quorum", "No quorum")) %>%
    ggplot() +
    aes(y = fct_reorder(street, total), x = vote) +
    geom_col(aes(x = total), fill = "gray80", alpha = .8) +
    geom_col(fill = "gray30") +
    scale_x_continuous(breaks = seq(0, 80, 10)) +
    labs(y = "", x = "Votes", title = "2023 Board elections turn-out") +
    geom_text(aes(
        color = quorum,
        x = vote + 4, y = street,
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

ggsave("turnout/graphs/gl2023_votingpatterns.png", width = 6, height = 8)