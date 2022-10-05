#standard libraries
library(tidyverse)
library(lubridate)

#define constants

YMAX_DEFAULT <- 160

# define standard functions

find_value <- function(x, y, target = 120) {
    aa <- approx(y, x, xout = target)$y
    as.Date(aa, origin = "1970-01-01")  ## convert back to a date (ugh)
}

find_zero_value <- function(x, y, target = 0) {
    aa <- approx(y, x, xout = target)$y
    return(aa)
}


eggstract <- function(tbl, col) {
     egg <- tbl %>%
                distinct({{col}}) %>%
                pull({{col}})
     return(egg)
}