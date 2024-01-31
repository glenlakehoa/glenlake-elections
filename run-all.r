#
#
#

scripts <-
    list.files(
        path = "./scripts",
        pattern = ".*\\.r$",
        full.names = TRUE
    )

purrr::walk(scripts, source)

source("genweb.r")
