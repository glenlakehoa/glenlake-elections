library(tidyverse)
library(patchwork)

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

pdf_lognorm <- function(x, mu, sig) {
    .5 * (1 + pracma::erf((log(x) - mu) / (sqrt(2) * sig)))
}

now <- lubridate::today()
pdate <- format(now, format = "%b %d, %Y")

board_tenure_raw <- read_csv("sources/boardmember_tenure.csv") %>%
    mutate(active = is.na(resignation)) %>%
    replace_na(list(resignation = now)) %>%
    mutate(tenure = (resignation - start) / lubridate::dyears(1))

board_tenure <-
    board_tenure_raw %>%
    group_by(name) %>%
    summarize(
        total_tenure = sum(tenure),
        active = max(active),
        .groups = "drop"
    ) %>%
    arrange(-total_tenure)

median_tenure <- median(board_tenure$total_tenure)

board_comment <-
    board_tenure %>%
    filter(active > 0) %>%
    summarize(
        sum_tenure = sum(total_tenure),
        median_tenure = median(total_tenure)
    ) %>%
    mutate(comment = glue::glue("The current board has\n{round(sum_tenure, 1)} combined years of\nexperience with median\ntenure of {round(median_tenure, 1)} years.")) # nolint

median_active <- board_comment$median_tenure

tenure_plot <-
    board_tenure %>%
    ggplot() +
    aes(
        y = fct_reorder(name, total_tenure),
        x = total_tenure,
        fill = factor(active)
    ) +
    geom_col(show.legend = FALSE) +
    geom_vline(
        xintercept = median_tenure, lty = 1,
        alpha = .5, size = 2, color = "#d95f02"
    ) +
    scale_fill_manual(values = c("0" = "#7570b3", "1" = "#1b9e77")) +
    labs(
        x = "Total tenure (in years)", y = "",
        caption = glue::glue("{pdate}. Tenure includes all owner-elected boards and the transition committee") # nolint
    ) +
    annotate("text",
        x = median_tenure + .05, y = 3, size = 3, , hjust = 0,
        label = glue::glue("Median tenure: {round(median_tenure,1)} years\n(all board members)") # nolint
    ) +
    annotate("text",
        x = 2.5, y = 20, label = board_comment$comment,
        size = 3, hjust = 0
    )

ggsave("tenure/boardmember_tenure.png",
    width = 6, height = 7,
    plot = tenure_plot
)


logparam <-
    with(
        board_tenure,
        list(
            mean = mean(log(total_tenure)),
            sd = sd(log(total_tenure))
        )
    )

normparam <-
    with(
        board_tenure,
        list(
            mean = mean(total_tenure),
            sd = sd(total_tenure)
        )
    )

logmn <- exp(logparam[["mean"]] + .5 * logparam[["sd"]]^2)


board_cdf <-
    tibble(
        ten_length = seq(0, ceiling(max(board_tenure$total_tenure)), .1),
        cdf = map_dbl(ten_length, ~ nrow(board_tenure %>% filter(total_tenure < .x))) / nrow(board_tenure), # nolint
        cdf_simul_log = map_dbl(ten_length, ~ pdf_lognorm(.x, logparam[["mean"]], logparam[["sd"]])) # nolint
    )

lognormal_plot <-
    board_cdf %>%
    ggplot() +
    aes(x = ten_length) +
    geom_point(aes(y = cdf), alpha = .3) +
    geom_line(aes(y = cdf_simul_log)) +
    geom_vline(xintercept = logmn) +
    labs(x = "", y = "", title = "Tenure CDF") +
    theme(plot.background = element_blank())

finalplot <- tenure_plot + inset_element(lognormal_plot, .5, .2, .97, .5) 

ggsave("tenure/boardmember_tenure_with_dist.png",
    width = 6, height = 7,
    plot = finalplot
)

finalplot2 <- tenure_plot + theme(axis.text.y=element_blank())+ inset_element(lognormal_plot, .5, .2, .97, .5) 

ggsave("tenure/boardmember_tenure_with_dist_nonames.png",
    width = 6, height = 7,
    plot = finalplot2
)