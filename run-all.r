#
#
#

safe_source <- purrr::safely(source)

scripts <-
    list.files(
        path = "./scripts",
        pattern = ".*\\.r$",
        full.names = TRUE
    )

purrr::walk(scripts, safe_source)

source("genweb.r")
