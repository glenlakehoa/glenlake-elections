#
#
#

scripts <- list.files(path = "./dev", pattern = ".*\\.r$", full.names = TRUE)
purrr::walk(scripts, source)