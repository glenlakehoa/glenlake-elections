library(tidyverse)

theme_set(theme_light())
load("Rdata/votes.Rdata")


print_model <- paste0(
    "Polynomial Decay Model: <I>votesreceived ~ a<sub>0</sub>",
    " \U007C a<sub>1</sub> - daysuntilelection",
    " \U007C<sup>a<sub>2</sub></sup></I>"
)

model_votes <- function(dat) {
    nls(votesreceived ~ a0 * (abs(a1 - daysuntilelection))^(a2),
        start = list(a0 = 23, a1 = 31, a2 = 0.5),
        control = list(warnOnly = TRUE, maxiter = 1000),
        data = dat
    )
}

calc_rsq <- function(mod) {
    pred <- broom::augment(mod)
    lmod <- lm(.fitted ~ votesreceived, data = pred)
    return(summary(lmod)$r.squared)
}

extract_params <- function(mod) {
    all_params <- broom::tidy(mod) %>% select(term, estimate)
    all_params$estimate %>% set_names(all_params$term)
}

param_comment <- function(params, rsq) {
    paste0(
        "a<sub>0</sub> = ", format(round(params["a0"], 2), nsmall = 2), "<BR/>",
        "a<sub>1</sub> = ", format(round(params["a1"], 2), nsmall = 2), "<BR/>",
        "a<sub>2</sub> = ", format(round(params["a2"], 2), nsmall = 2), "<BR/>",
        "r<sup>2</sup> = ", format(round(rsq, 3), nsmall = 2)
    )
}

predict_votes <- function(mod, day_range = seq(-20, 35, 1)) {
    as_tibble(
        investr::predFit(mod,
            newdata = tibble(daysuntilelection = day_range),
            interval = "prediction"
        )
    ) %>% mutate(daysuntilelection = day_range)
}

day_zero_votes <- function(dat) {
    dat %>%
        filter(daysuntilelection == 0) %>%
        mutate(expected_comment = glue::glue("Expected votes: {round(fit, 0)}"))
}


year_mods <-
    votes %>%
    nest(data = !year) %>%
    mutate(
        mod = map(data, model_votes),
        converged = map_lgl(mod, \(m) pluck(m, "convInfo", "isConv"))
    ) %>%
    filter(converged) %>%
    mutate(
        rsq = map_dbl(mod, calc_rsq),
        params = map(mod, extract_params),
        param_comment = map2_chr(params, rsq, param_comment),
        pred_votes = map(mod, predict_votes),
        expected_comment = map(pred_votes, day_zero_votes)
    )

all_years <-
    year_mods %>%
    unnest(pred_votes) %>%
    filter(daysuntilelection >= -7) %>%
    ggplot(aes(x = daysuntilelection, y = fit, group = year)) +
    coord_cartesian(clip = "off") +
    geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
    scale_x_reverse(
        limits = c(35, -7),
        breaks = seq(35, -7, -7)
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 480, 20)
    ) +
    geom_point(
        data = votes,
        aes(y = votesreceived),
        shape = 21
    ) +
    geom_text(
        data = year_mods %>% unnest_wider(expected_comment),
        aes(
            x = 0,
            y = fit + 10,
            label = expected_comment
        ),
        color = "gray50",
        size = 3,
        hjust = 1.1
    ) +
    ggtext::geom_richtext(
        # data = year_mod_params,
        aes(
            x = 11,
            y = 30,
            label = param_comment
        ),
        color = "gray50",
        size = 3,
        hjust = 0,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    geom_errorbar(
        data = year_mods %>% unnest_wider(expected_comment),
        aes(
            x = 0,
            y = fit,
            ymin = lwr,
            ymax = upr
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
    facet_wrap(~year, scales = "free_y") +
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
# Generate individual plots for a each year
#
#

generate_year_plot <- function(year_mod, all_votes = votes) {
    mod_data <- year_mod$pred_votes[[1]]

    filter_year <- year_mod$year

    print_params <- str_replace_all(year_mod$param_comment, "<BR/>", ", ")

    quorum_date <-
        approx(mod_data$fit, mod_data$daysuntilelection, xout = 120)$y %>%
        round(., digits = 1)

    qual <- ifelse(quorum_date > 0, "before", "after")

    err_bar <- mod_data %>%
        filter(daysuntilelection == 0)

    err_range <-
        with(
            year_mod$expected_comment[[1]],
            glue::glue(
                "({round(lwr, digits = 0)} - ",
                "{round(upr, digits = 0)})"
            )
        )

    final_vote <-
        round(year_mod$expected_comment[[1]]$fit, digits = 0)

    year_filtered <-
        mod_data %>%
        filter(daysuntilelection >= -7) %>%
        ggplot(aes(x = daysuntilelection, y = fit)) +
        geom_line(linetype = "dashed", color = "gray50", alpha = .5) +
        scale_x_reverse(
            limits = c(35, -7),
            breaks = seq(35, -7, -7)
        ) +
        scale_y_continuous(
            limits = c(0, NA),
            breaks = seq(0, 480, 20)
        ) +
        geom_point(
            data = all_votes %>% filter(year == filter_year), aes(y = votesreceived),
            shape = 21
        ) +
        geom_errorbar(
            data = mod_data %>% filter(daysuntilelection == 0),
            aes(ymin = lwr, ymax = upr), width = 1, linewidth = 0.5, color = "gray50"
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
            subtitle = glue::glue("Expected votes ({filter_year}): {final_vote} {err_range}"),
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

purrr::walk(seq_along(year_mods$year), \(k) generate_year_plot(year_mods[k, ], votes))