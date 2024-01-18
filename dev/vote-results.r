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

load("Rdata/vote_results.Rdata")

vote_results %>%
    drop_na() %>%
    group_by(year) %>%
    mutate(
        name = fct_reorder(name, -votes)
    ) %>%
    ungroup() %>% view()
    ggplot(aes(y = name, x = votes, group = year)) +
    geom_col() +
    facet_wrap(~year, scale = "free_y")
