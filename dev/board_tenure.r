library(tidyverse)
library(patchwork)
library(lubridate)


load("Rdata/board_tenure.Rdata")
load("Rdata/votes.Rdata")

meetingdates <- votes %>%
    distinct(meetingdate) %>%
    pull(meetingdate)

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

glcolors <- list(green = "#295043", brown = "#D3BDA8")

now <- lubridate::today()
pdate <- format(now, format = "%b %d, %Y")

board_tenure_raw <-
    board_tenure_dates %>%
    mutate(active = is.na(tenure_end)) %>%
    replace_na(list(tenure_end = now)) %>% 
    mutate(tenure = (tenure_end - tenure_start) / lubridate::dyears(1))

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
        alpha = .5, linewidth = 2, color = "#d95f02"
    ) +
    scale_fill_manual(values = c(
        "0" = glcolors[["brown"]],
        "1" = glcolors[["green"]]
    )) +
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

ggsave("graphs/boardmember_tenure.png",
    width = 6, height = 7,
    plot = tenure_plot
)

#
# board composition
#

source("tenure/filtering.r")

timeline_g <-
    board_tenure_raw %>%
    group_by(name) %>%
    mutate(first_start = min(tenure_start)) %>%
    ungroup() %>%
    mutate(name = fct_reorder(name, desc(first_start))) %>%
    ggplot() +
    aes(y = name) +
    geom_point(aes(x = tenure_start, color = active), size = 2) +
    geom_point(
        aes(
            x = tenure_end,
            color = active,
            shape = active
        ),
        size = 2
    ) +
    geom_segment(
        aes(
            yend = name,
            x = tenure_start,
            xend = tenure_end,
            color = active
        ),
        linewidth = 1.5
    ) +
    geom_vline(
        xintercept = meetingdates, lty = 1,
        alpha = .1, color = "grey70", linewidth = 2
    ) +
    annotate("text",
        x = lubridate::ymd(20180208) + days(21), y = 2.5,
        label = "Owner-elected", hjust = 0, size = 2, alpha = .7
    ) +
    annotate("text",
        x = lubridate::ymd(20180208) - days(21), y = 2.5,
        label = "Transition\nCommittee", hjust = 1, size = 2, alpha = .7
    ) +
    annotate("label",
        x = lubridate::ymd(20170801) - weeks(1), y = 7,
        label = tenure_summary, hjust = 0, size = 3, alpha = .9
    ) +
    scale_x_date(date_breaks = "1 year", date_label = "%Y") +
    scale_color_manual(values = c(
        "TRUE" = glcolors[["green"]],
        "FALSE" = glcolors[["brown"]]
    )) +
    scale_shape_manual(values = c("TRUE" = 1, "FALSE" = 16)) +
    theme(legend.position = "none", panel.grid = element_blank()) +
    labs(
        x = "", y = "", title = "Glenlake HOA board member timeline",
        caption = glue::glue("{pdate}. Tenure includes all owner-elected boards and the transition committee") # nolint
    ) +
    annotate("label",
        x = ymd(today()),
        y = length(unique(board_tenure_raw$name)) - 1,
        hjust = 1,
        label = "Glenlake HOA board\nmember timeline"
    ) +
    theme(plot.title = element_blank())

ggsave("graphs/boardmember_timeline.png",
    height = 6, width = 7,
    plot = timeline_g)
