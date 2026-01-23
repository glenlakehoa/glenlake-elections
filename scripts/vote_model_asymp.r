library(tidyverse)

theme_set(theme_light())
load("Rdata/votes.Rdata")



#
# apply to all years
#

year_mods <-
    votes %>%
    nest(data = !year) %>%
    mutate(
        mod = map(data, \(dat) {
            # nls(votesreceived ~ a0 - a1 * exp(-a2 * daysuntilelection),
            # nls(votesreceived ~ a0 * sqrt(abs(a1 - daysuntilelection)),
            nls(votesreceived ~ a0 * (abs(a1 - daysuntilelection))^(a2),
                # start = list(a0 = 120, a1 = 120, a2 = -.1),
                start = list(a0 = 23, a1 = 31, a2 = 0.5),
                control = list(warnOnly = TRUE, maxiter = 1000),
                data = dat
            )
        }),
    )

year_mod_preds <-
    year_mods %>%
    mutate(
        final_vote = map(mod, ~ {
            investr::predFit(
                .x,
                newdata = tibble(daysuntilelection = 0), interval = "prediction"
            ) %>%
                as.numeric()
        })
    ) %>%
    unnest_wider(final_vote, names_sep = "_") %>%
    select(year, starts_with("final_vote")) %>% 
    replace_na(list(final_vote_1 = 0, final_vote_2 = 0, final_vote_3 = 0))

all_years <-
    year_mods %>%
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
        limits = c(0, 180),
        breaks = seq(0, 160, 20)
    ) +
    geom_point(
        data = votes,
        aes(y = votesreceived),
        shape = 21
    ) +
    geom_text(
        data = year_mod_preds,
        aes(
            x = 0,
            y = final_vote_1 + 5,
            label = paste0("Expected votes: ", round(final_vote_1, 0))
        ),
        color = "gray50",
        size = 3,
        hjust = 1.1
    ) +
    geom_errorbar(
        data = year_mod_preds,
        aes(
            x = 0,
            y = final_vote_1,
            ymin = final_vote_2,
            ymax = final_vote_3
        ),
        width = 2,
        color = "gray50"
    ) +
    geom_hline(yintercept = 120, linewidth = 2, alpha = .1) +
    geom_vline(xintercept = 0, linewidth = 2, alpha = .1) +
    coord_cartesian(clip = "off") +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = "Modeling incoming vote rate",
        caption = "Polynomial decay model"
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



filter_year <- 2026

mod_filter_year <- year_mods %>%
    filter(year == filter_year) %>%
    pull(mod) %>%
    .[[1]]

params <- broom::tidy(mod_filter_year) %>% 
    pull(estimate) %>% 
    round(., digits = 3)

print_params <- paste("Model parameters:", paste(c("a0", "a1", "a2"), "=", params, collapse = ", "))
print_model <- paste("Polynomial Decay Model: votesreceived ~ a0 * (abs(a1 - daysuntilelection))^(a2)")



quorum_date <-
    with(
        # mod_data <- broom::augment(mod_filter_year, newdata = tibble(daysuntilelection = seq(-20, 35, 1))),
        mod_data <- investr::predFit(
            mod_filter_year,
            newdata = tibble(daysuntilelection = seq(-20, 35, 1)),
            interval = "confidence"
        ) %>% as_tibble() %>% mutate(daysuntilelection = seq(-20, 35, 1)),
        approx(fit, daysuntilelection, xout = 120)
    )$y %>%
    round(., digits = 1)

qual <- ifelse(quorum_date > 0, "before", "after")

# std_err <- round(broom::glance(mod_filter_year)$sigma[1], digits = 2)
err_bar <- mod_data %>%
    filter(daysuntilelection == 0)

err_range <- glue::glue(
    "({round(err_bar$lwr, digits = 0)} - ",
    "{round(err_bar$upr, digits = 0)})"
)

final_vote <-
    round(err_bar$fit, digits = 0)

year_filtered <-
    mod_data %>%
    ggplot(aes(x = daysuntilelection, y =  fit)) +
    geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
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
        subtitle = glue::glue("Expected votes: {final_vote} {err_range}"),
        caption = glue::glue("{print_model}\n{print_params}")
    ) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            size = 12, color = "gray50",
            margin = margin(b = 3)
        ),
        plot.subtitle = ggtext::element_textbox_simple(
            size = 10, color = "gray50",
            margin = margin(b = 3)
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    )

ggsave(paste0("graphs/asymptotic", filter_year, ".png"),
    width = 6, height = 4,
    plot = year_filtered
)
