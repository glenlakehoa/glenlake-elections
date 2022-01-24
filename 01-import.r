# default libraries
library(tidyverse)
library(lubridate)

#read neighborhood defaults
config <- read_csv("config/config.csv") %>%
              mutate(year = factor(year),
                     quorum = numberhomes %/% 4)

# read data file
votes <-
    list.files(path = "sources/", pattern = "*.csv", full.names = TRUE) %>%
    map_df(~read_csv(.)) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
            year = factor(year(date))) %>%
    inner_join(config) %>%
    mutate(votesneeded = quorum - votesreceived,
           daysuntilelection = meetingdate - date,
           pastquorum = ifelse(votesreceived >= quorum, TRUE, FALSE)
           )

save(votes, file = "Rdata/votes.Rdata")