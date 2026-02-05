library(tidyverse)

theme_set(theme_light())
source("defaults.r")
load("Rdata/votes.Rdata")


print_model <- paste0(
    "Polynomial Decay Model: <I>votesreceived ~ a<sub>0</sub>",
    " \U007C a<sub>1</sub> - daysuntilelection",
    " \U007C<sup>a<sub>2</sub></sup></I>"
)

model_votes <- function(dat, start_param = list(a0 = 23, a1 = 31, a2 = 1),
                        control_param = list(warnOnly = TRUE, maxiter = 100)) {
    nls(votesreceived ~ a0 * (abs(a1 - daysuntilelection))^(a2),
        start = start_param,
        control = control_param,
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

extract_model_plumbing <- function(mod, plumb = c("isConv", "finIter", "finTol"), names = c("converged", "n_iter", "fin_tol")) {
    plumb_values <- lapply(plumb, \(p) pluck(mod, "convInfo", p))
    names(plumb_values) <- names
    plumb_values
}

param_comment <- function(params, rsq, n_iter) {
    paste0(
        "a<sub>0</sub> = ", format(round(params["a0"], 2), nsmall = 2), "<BR/>",
        "a<sub>1</sub> = ", format(round(params["a1"], 2), nsmall = 2), "<BR/>",
        "a<sub>2</sub> = ", format(round(params["a2"], 2), nsmall = 2), "<BR/>",
        "r<sup>2</sup> = ", format(round(rsq, 3), nsmall = 2), "<BR/>",
        "# of iter. = ", n_iter
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

find_quorum <- function(dat, quorum = 120) {
    params <- c("lwr", "fit", "upr")
    days <- lapply(params, \(p) approx(dat[[p]], dat[["daysuntilelection"]], xout = quorum)$y)
    names(days) <- c("low_quorum", "fit_quorum", "high_quorum")
    days
}


year_mods <-
    votes %>%
    nest(data = !year) %>%
    mutate(data_points = map_int(data, nrow)) %>%
    # filter on number of data points being greater than number of parameters (3)
    filter(data_points > 3) %>%
    mutate(
        mod = map(data, model_votes),
        plumbing = map(mod, extract_model_plumbing)
    ) %>%
    unnest_wider(plumbing) %>%
    # filter on models that converged
    filter(converged) %>%
    mutate(
        rsq = map_dbl(mod, calc_rsq),
        params = map(mod, extract_params),
        param_comment = pmap_chr(list(params, rsq, n_iter), \(p, r, n) param_comment(p, r, n)),
        pred_votes = map(mod, predict_votes),
        expected_comment = map(pred_votes, day_zero_votes)
    ) %>%
    mutate(
        a1 = map_dbl(params, \(p) floor(p["a1"])),
        pred_votes_adj = map2(pred_votes, a1, \(d, a1) d %>% filter(daysuntilelection <= a1)),
        quorum_range = map(pred_votes_adj, find_quorum)
    )

all_years <-
    year_mods %>%
    unnest(pred_votes_adj) %>%
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
    geom_errorbar(
        data = year_mods %>% unnest_wider(quorum_range),
        aes(
            y = 120,
            x = fit_quorum,
            xmin = high_quorum,
            xmax = low_quorum
        ),
        width = 10,
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
    mod_data <- year_mod$pred_votes_adj[[1]]

    filter_year <- year_mod$year

    print_params <- str_replace_all(year_mod$param_comment, "<BR/>", ", ")

    quorum_dates <- lapply(find_quorum(mod_data), round, digits = 1)

    quorum_date <- quorum_dates[["fit_quorum"]]

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
        geom_errorbar(
            data = as_tibble(quorum_dates),
            aes(y = 120, x = fit_quorum, xmin = low_quorum, xmax = high_quorum),
            width = 10, linewidth = 0.5, color = "gray50", inherit.aes = FALSE
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



#
# dashboard target zone
#
#

boundingbox <- tibble(x = c(400, 120, 120, 400), y = c(0, 0, 40, 40))

box_data <- year_mods %>%
    unnest(pred_votes_adj) %>%
    filter(daysuntilelection == 0) %>%
    unnest_wider(quorum_range) %>%
    select(year, expected_votes = fit, lwr, upr, low_quorum, fit_quorum, high_quorum) %>%
    # select columns lwr, upr and set those to NA where year != 2026
    mutate(across(
        c(lwr, upr, low_quorum, high_quorum),
        ~ ifelse(year != max(year), NA, .)
    ))

comp_mod_g <-
    box_data %>%
    mutate(
        mxyear = max(year),
        currentyear = year == mxyear,
    ) %>%
    ggplot(aes(x = expected_votes, y = fit_quorum, color = currentyear)) +
    geom_polygon(
        data = boundingbox,
        inherit.aes = FALSE,
        aes(x, y),
        fill = alpha(glcolors$green, 0.05),
        lty = 1,
        linewidth = 2,
        color = alpha(glcolors$green, 0.2)
    ) +
    geom_point(shape = 10, size = 4, show.legend = FALSE) +
    geom_errorbar(
        aes(ymin = low_quorum, ymax = high_quorum),
        width = 2,
        show.legend = FALSE
    ) +
    geom_errorbar(
        aes(xmin = lwr, xmax = upr),
        width = 1,
        show.legend = FALSE
    ) +
    geom_label(aes(label = year), show.legend = FALSE) +
    scale_x_continuous(
        breaks = seq(0, 480, 20)
    ) +
    scale_y_continuous(
        breaks = seq(-20, 20, 2)
    ) +
    coord_cartesian(
        xlim = c(80, 180), ylim = c(-5, 20),
    ) +
    labs(
        x = "Polynomial Decay Model Prediction",
        y = "Expected Quorum Date (days until election)",
        title = "Comparison of Vote Prediction Models",
        caption = "Dashed lines indicate quorum threshold of 120 votes"
    ) +
    scale_color_manual(
        values = c("TRUE" = glcolors$green, "FALSE" = glcolors$tan)
    ) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            size = 15,
            margin = margin(b = 10)
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = ggtext::element_markdown(hjust = 0)
    )

ggsave("graphs/vote_model_comparison.png",
    width = 8, height = 6, plot = comp_mod_g
)
