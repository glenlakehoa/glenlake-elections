library(tidyverse)
library(jsonlite)

source_files <- list.files("sources/", pattern = "*.json$", full.names = TRUE)

votes <-
    map_dfr(
        source_files,
        ~ read_json(.x)[c("year", "numberofhomes", "meetingdate", "votes")]
    ) %>%
    unnest_wider(votes) %>%
    rename(votesreceived = votes) %>%
    mutate(
        across(ends_with("date"), lubridate::ymd),
        quorum = numberofhomes %/% 4,
        daysuntilelection = (meetingdate - date) / lubridate::ddays(1),
        voteneeded = quorum - votesreceived,
        past_quorum = votesreceived >= quorum
    ) %>%
    arrange(date)

save(votes, file = "Rdata/votes.Rdata")

board_tenure_dates <-
    map_dfr(
        source_files,
        ~ read_json(.x)[c("year", "board")]
    ) %>%
    unnest_wider(board) %>%
    mutate(across(
        starts_with("tenure"),
        ~ case_when(
            .x == "" ~ ymd(NA_character_),
            TRUE ~ ymd(.x)
        )
    )) %>%
    arrange(tenure_start, name)

save(board_tenure_dates, file = "Rdata/board_tenure.Rdata")