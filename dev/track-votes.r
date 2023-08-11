library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

load("Rdata/votes.Rdata")

year_range <- unique(votes$year)

track_data_by_year <- function(yr, vote_data = votes) {

    vote <- dplyr::filter(vote_data, year == yr)

    meeting_quorum <- unique(vote$quorum)
    meeting_date <- unique(vote$meetingdate)
    vote_max <- max(vote$votesreceived)
    max_year <- max(vote_data$year)

    y_max <- dplyr::if_else(vote_max < 160,
        160,
        20 * (vote_max %/% 20 + 1)
    )

    date_labels <- meeting_date - 0:5 * lubridate::weeks(1)

    votes_required <- dplyr::if_else(vote_max >= meeting_quorum,
        NA_real_,
        meeting_quorum - vote_max
    )

    subtitle <- dplyr::if_else(is.na(votes_required),
        glue::glue("Quorum has been reached with {vote_max} votes"),
        glue::glue(
            "{votes_required} votes are still needed before ",
            format(meeting_date, format = "%B %d"), " to reach quorum"
        )
    )

    caption <- glue::glue(
        "\U00A9 Glenlake Upstate Homeowners ",
        "Association, Inc. Updated ",
        format(lubridate::today(), format = "%b %d, %Y")
    )

    ggplot2::ggplot(vote, ggplot2::aes(x = date, y = votesreceived)) +
    ggplot2::geom_hline(
        yintercept = meeting_quorum,
        color = "#D3BDA8",
        linewidth = 1
    ) +
    ggplot2::geom_vline(
        xintercept = meeting_date,
        lty = 1,
        color = "#D3BDA8", linewidth = 1
    ) +
    ggplot2::annotate("text",
        x = meeting_date + days(1),
        y = 10,
        label = "Annual Meeting",
        hjust = "left",
        angle = 90
    ) +
    ggplot2::geom_point(size = 2, color = "#295043") +
    ggplot2::geom_line(lty = 1, color = "#295043") +
    ggplot2::labs(
        x = "",
        y = "Votes received",
        title = glue::glue("Glen Lake elections {yr}"),
        subtitle = subtitle,
        caption = caption
    ) +
    ggplot2::scale_x_date(
        limits = c(min(date_labels), max(date_labels) + days(2)),
        breaks = date_labels,
        date_label = "%b %d"
    ) +
    ggplot2::scale_y_continuous(
        breaks = seq(0, 484, 40),
        limits = c(0, y_max),
        sec.axis = ggplot2::sec_axis(~ . / meeting_quorum,
            breaks = seq(0, 5, .25),
            name = "Relative to quorum",
            labels = scales::percent_format(accuracy = 1)
        )
    ) +
    ggplot2::theme(
        plot.caption.position = "plot",
        plot.caption = ggplot2:: element_text(hjust = 0, size = 6),
        plot.title.position = "plot",
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
    ) +
    patchwork::inset_element(
        p = logoimage,
        align_to = "full",
        on_top = FALSE,
        left = 0.85,
        bottom = 0.85,
        right = 1,
        top = 1
    )

    ggplot2::ggsave(glue::glue("graphs/vote-tracking-{yr}.png"), width = 6, height = 4) # nolint
    if (yr == max_year) ggplot2::ggsave("graphs/vote-tracking.png", width = 6, height = 4) # nolint
}

logoimage <- jpeg::readJPEG("images/glenlakelogo.jpg", native = TRUE)

walk(year_range, ~ track_data_by_year(.x))