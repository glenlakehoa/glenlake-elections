library(tidyverse)

theme_set(theme_light())
load("Rdata/votes.Rdata")

year_mods <-
    votes %>%
    nest(data = !year) %>%
    mutate(
        mod = map(data, \(dat) {
            nls(votesreceived ~ a0 * (abs(a1 - daysuntilelection))^(a2),
                start = list(a0 = 23, a1 = 31, a2 = 0.5),
                control = list(warnOnly = TRUE, maxiter = 1000),
                data = dat
            )
        }),
        converged = map_lgl(mod, \(m) pluck(m, "convInfo", "isConv")),
    ) %>%
    filter(converged)

print_model <- paste("Polynomial Decay Model: <I>votesreceived ~ a<sub>0</sub> \U007C a<sub>1</sub> - daysuntilelection \U007C<sup>a<sub>2</sub></sup></I>")

# extract parameters from year_mods
year_mod_params <-
    year_mods %>%
    mutate(
        params = map(mod, ~ broom::tidy(.x) %>% select(term, estimate))
    ) %>%
    unnest(params) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    select(year, a0, a1, a2) %>%
    # make a variable label for a text label containing the model parameters
    mutate(
        param_label = paste0(
            "a<sub>0</sub> = ", format(round(a0, 2), nsmall = 2), "<BR/>",
            "a<sub>1</sub> = ", format(round(a1, 2), nsmall = 2), "<BR/>",
            "a<sub>2</sub> = ", format(round(a2, 2), nsmall = 2)
        )
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
    coord_cartesian(clip = "off") +
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
            y = final_vote_1 + 10,
            label = paste0("Expected votes: ", round(final_vote_1, 0))
        ),
        color = "gray50",
        size = 3,
        hjust = 1.1
    ) +
    # add a geom_text for the model parameters
    ggtext::geom_richtext(
        data = year_mod_params,
        aes(
            x = 11,
            y = 30,
            label = param_label
        ),
        color = "gray50",
        size = 3,
        hjust = 0,
        fill = NA, label.color = NA,
        # remove label padding, since we have removed the label outline
        label.padding = grid::unit(rep(0, 4), "pt")
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
        linewidth = 0.5,
        color = "gray50"
    ) +
    geom_hline(yintercept = 120, linewidth = 2, alpha = .1) +
    geom_vline(xintercept = 0, linewidth = 2, alpha = .1) +
    labs(
        x = "Days until the Annual Meeting",
        y = "Votes received",
        title = "Modeling incoming vote rate",
        caption = glue::glue("{print_model}")
    ) +
    facet_wrap(~year) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            size = 15, color = "gray50",
            margin = margin(b = 10)
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = ggtext::element_markdown(hjust = 0)
    )

ggsave("graphs/vote_model_pred.png",
    width = 10, height = 8,
    plot = all_years
)

#
# Generate plot for a specific year
#
#

generate_year_plot <- function(year_mods, filter_year = year(today())) {
    mod_filter_year <- year_mods %>%
        filter(year == filter_year) %>%
        pull(mod) %>%
        .[[1]]

    params <- broom::tidy(mod_filter_year) %>%
        pull(estimate) %>%
        round(., digits = 3)


    quorum_date <-
        with(
            mod_data <- investr::predFit(
                mod_filter_year,
                newdata = tibble(daysuntilelection = seq(-20, 35, 1)),
                interval = "confidence"
            ) %>%
                as_tibble() %>%
                mutate(daysuntilelection = seq(-20, 35, 1)),
            approx(fit, daysuntilelection, xout = 120)
        )$y %>%
        round(., digits = 1)

    rsq <-
        lm(fit ~ daysuntilelection,
            data = mod_data %>% inner_join(votes %>% filter(year == filter_year), by = "daysuntilelection")
        ) %>%
        summary(.) %>%
        .[["r.squared"]]

    qual <- ifelse(quorum_date > 0, "before", "after")

    err_bar <- mod_data %>%
        filter(daysuntilelection == 0)

    err_range <- glue::glue(
        "({round(err_bar$lwr, digits = 0)} - ",
        "{round(err_bar$upr, digits = 0)})"
    )

    final_vote <-
        round(err_bar$fit, digits = 0)

    print_params <- paste0(
        "Model parameters: ",
        paste(c("a<sub>0</sub>", "a<sub>1</sub>", "a<sub>2</sub>"),
            "=",
            params,
            collapse = ", "
        ),
        "; R<sup>2</sup> = ", round(rsq, 3)
    )

    year_filtered <-
        mod_data %>%
        filter(daysuntilelection > -7) %>%
        ggplot(aes(x = daysuntilelection, y = fit)) +
        geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
        scale_x_reverse(
            limits = c(35, -7),
            breaks = seq(35, -7, -7)
        ) +
        scale_y_continuous(
            limits = c(0, NA),
            breaks = seq(0, 240, 20)
        ) +
        geom_point(
            data = votes %>% filter(year == filter_year), aes(y = votesreceived),
            shape = 21
        ) +
        geom_errorbar(
            data = mod_data %>% filter(daysuntilelection == 0),
            aes(ymin = lwr, ymax = upr), width = 2, linewidth = 0.5, color = "gray50"
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
            caption = glue::glue("{print_model}<BR/><BR/>{print_params}")
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
            plot.caption = ggtext::element_markdown(hjust = 0)
        )

    ggsave(paste0("graphs/year_pred/vote_model_pred_", filter_year, ".png"),
        width = 6.5, height = 5,
        plot = year_filtered
    )
}

purrr::walk(year_mods$year, \(y) generate_year_plot(year_mods, y))
