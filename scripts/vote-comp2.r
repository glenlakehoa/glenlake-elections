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


load("Rdata/votes.Rdata")


prep_data <-
    votes %>%
    mutate(historical = case_when(
        year == year(today()) ~ as.character(year(today())),
        # year == year(today())-1 ~ as.character(year(today())-1),
        TRUE ~ "Previous years"
    ))

last_election <- as.numeric(max(prep_data$meetingdate) - today())

comp_models <-
    prep_data %>%
    nest(data = !historical) %>%
    mutate(
        mods = map(data, ~ lm(votesreceived ~ daysuntilelection, data = .x)),
        preds = map(mods, ~ broom::augment(.x, newdata = tibble(daysuntilelection = 0:30), interval = "prediction"))
    ) %>%
    unnest(preds)

colorset <- c("gray70", "darkgreen")
names(colorset) <- unique(prep_data$historical)


comp_g <-
    comp_models %>%
    ggplot(aes(x = daysuntilelection, y = .fitted, color = historical)) +
    geom_line() +
    geom_ribbon(
        aes(ymin = .lower, ymax = .upper, fill = historical),
        alpha = .2,
        show.legend = FALSE,
        linetype = 0
    ) +
    geom_point(
        data = prep_data, aes(y = votesreceived, color = historical),
        show.legend = FALSE,
        shape = 1
    ) +
    scale_x_reverse() +
    labs(
        x = "Days until the elections",
        y = "Votes received",
        title = "Comparison of incoming votes to previous years",
        color = "Comparison"
    ) +
    coord_cartesian(
        ylim = c(0, NA)
    ) +
    geom_vline(
        xintercept = last_election,
        linewidth = 2,
        alpha = .3,
        color = "gray60"
    ) +
    geom_hline(
        yintercept = 120,
        linewidth = 2,
        alpha = .3,
        color = "gray60"
    ) +
    scale_color_manual(values = colorset) +
    scale_fill_manual(values = colorset)

ggsave("graphs/comparison-previous-years.png",
    width = 8, height = 5,
    plot = comp_g
)
