library(tidyverse)
library(jsonlite)


source_files <- list.files("sources/", pattern = "*.json$", full.names = TRUE)

votes <-
    map_dfr(source_files, 
            ~jsonlite::read_json(.x)) %>%
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

