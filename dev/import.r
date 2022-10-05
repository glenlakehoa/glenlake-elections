library(tidyverse)
library(lubridate)

electorate_data <- read_csv("config/config.csv")

source_files <- list.files("sources/", pattern = "*.csv", full.names = TRUE)

votes <-
    map_dfr(source_files, read_csv) %>%
    mutate(year = year(date)) %>%
    inner_join(electorate_data, by = "year") %>%
    mutate(
        quorum = numberhomes %/% 4,
        daysuntilelection = (meetingdate - date) / ddays(1),
        voteneeded = quorum - votesreceived,
        past_quorum = votesreceived >= quorum
    ) %>%
    arrange(date)

save(votes, file = "Rdata/votes.Rdata")