library(broom)


source("00-defaults.r")
source("01-import.r")
theme_set(theme_light())

votemodels <-
    votes %>%
    mutate(daysuntilelection = as.numeric(daysuntilelection)) %>%
    group_by(year) %>%
    nest() %>%
    mutate(votesmodel = map(data, 
                            function(tbl) tbl %>% lm(votesreceived ~ daysuntilelection, data = .))) %>%
    ungroup()


votesdata <-
    votemodels %>% mutate(prediction = map(votesmodel, function(tbl) tbl %>% broom::augment(newdata = tibble(daysuntilelection = 0:30)))) %>%
    unnest(prediction)

votesinterpret <-
    votemodels %>% mutate(modelinfo = map(votesmodel, broom::tidy)) %>%
    unnest(modelinfo) %>%
    filter(term == "daysuntilelection") %>%
    mutate(estimate = -estimate)

votesmodelinfo <-
    votemodels %>% mutate(modelinfo = map(votesmodel, broom::glance)) %>%
    unnest(modelinfo)

allinfo <-
    inner_join(votesinterpret, votesmodelinfo, by = "year") %>%
    select(year, estimate, r.squared) %>%
    mutate(across(!year, ~round(., 2))) %>%
    knitr::kable()


votesdata %>%
    ggplot +
    aes(x = daysuntilelection, y = .fitted, color = year) + 
    facet_wrap(~year) +
    geom_point(data = votes, aes(daysuntilelection, votesreceived)) +
    geom_line(aes(y = .fitted)) +
    scale_x_reverse() +
    labs(x = "Days until the election",
         y = "Votes received") +
    theme(legend.position = "none") + 
    geom_hline(yintercept = 120) + 
    scale_y_continuous(limit = c(0, NA), breaks = 20 * 0:100)