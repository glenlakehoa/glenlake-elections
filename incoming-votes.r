# default libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
# library(broom)

#define constants
source("00-defaults.r")
source("01-import.r")
load("Rdata/votes.Rdata")

year_range <- eggstract(votes, year)
year_range_length <- length(year_range)
year_now <- last(year_range)
comp_range <- year_range[seq_len(year_range_length) - 1]

sub_text <- paste0(first(comp_range), "-", last(comp_range))

color_range <- c(rep("gray50", year_range_length - 1), "red")
line_type_range <- c(rep("dotted", year_range_length - 1), "solid")
line_size_range <- c(rep(0.5, year_range_length - 1), 1.5)

votes %>%
    mutate(count = case_when(year == year_now ~ votesreceived
                             )
            ) %>%
    ggplot +
    aes(x = daysuntilelection,
        y = votesreceived,
        label = count,
        color = year,
        linetype = year,
        size = year) +
    geom_line() +
    geom_point(data = votes %>% filter(year == year_now),
               aes(label = NA),
               color = "red",
               size = 3)  +
    geom_label(size = 2) +
    scale_x_reverse(breaks = seq(0, 49, 7)) +
    scale_y_continuous(breaks = seq(0, 200, 20)) +
    labs(x = "Days until the Annual Meeting",
         y = "Votes received",
         title = "Comparison of incoming votes by year",
         subtitle = paste0("Comparing ",
                            year_now,
                            " (in <b style = \"color:red\"\">red</b>) to ",
                            sub_text,
                            " (in <b style = \"color:dimgray\"\">gray</b>)"),
         caption = "\U00A9 2022, Glenlake Upstate Homeowners Assocation."
        ) +
    geom_hline(yintercept = 120, color = "gray50", lty = 2) +
    geom_vline(xintercept = 0, color = "gray50", lty = 2) +
    scale_color_manual(values = color_range) +
    scale_linetype_manual(values = line_type_range) +
    scale_size_manual(values = line_size_range) +
    annotate("label",
             x = 29,
             y = 120,
             label = "Votes needed to meet quorum",
             size = 3,
            #  color = "black",
             hjust = 0) +
    geom_richtext(x = 0,
             y = 10,
             label = "Annual Meeting",
             hjust = 0,
             angle = 90,
             size = 3,
             color = "black"
             ) +
    theme_light() +
    theme(legend.position = "none", 
          plot.subtitle = element_markdown())

ggsave("trends/vote-count-comparison.png", width = 6, height = 6)