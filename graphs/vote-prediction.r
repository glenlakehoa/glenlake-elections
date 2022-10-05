library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

load("Rdata/votes.Rdata")


votes %>% ggplot + aes(daysuntilelection, voteneeded, color = factor(year)) + geom_line() + facet_wrap(~factor(year)) + 
geom_hline(yintercept = 0) + scale_x_continuous(limit = c(0, 10))


votemodels <-
    votes %>%
    group_by(year) %>%
    nest() %>%
    mutate(linmod = map(data, ~lm(voteneeded ~ daysuntilelection, data = .))) 
    

vote_predict <-
    votemodels %>%
    mutate(params = map(linmod, broom::tidy)) %>%
    unnest(params) %>%
    select(year, term, estimate) %>%
    pivot_wider(names_from = "term", values_from = "estimate") %>% 
    janitor::clean_names() %>%
    mutate(dayzero = -intercept / daysuntilelection) %>%
    select(year, vote_rate = daysuntilelection, quorum_reached = dayzero)


(vote_predict %>%
    ggplot + 
    aes(year, vote_rate) + 
    geom_col() + 
    labs(x = NULL,
        y = "Rate of voting per day")
) /
(vote_predict %>%
    ggplot + 
    aes(year, quorum_reached) + 
    geom_col() + 
    labs(x = NULL,
        y = "Expected time before quorum is reached")
)