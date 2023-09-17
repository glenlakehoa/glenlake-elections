make_links <- function(str_arr) {
    paste0(
        "<p><img src=\"",
        str_arr,
        "\" width = \"50%\">",
        collapse = "\n"
    )
}

header <- function() {
    out <- paste0(
        "<!doctype html>
            <html lang=\"en-US\">
            <head>
            <meta charset=\"utf-8\" />
            <meta name=\"viewport\" content=\"width=device-width\" />
            <title>Glenlake Board Elections Vote Statistics</title>
          </head>
          <body>
        "
    )
    out
}

footer <- function() {
    out <- paste0(
        "   </body>
            </html>
        "
    )
    out
}

make_doc <- function(html_list) {
    paste0(
        header(),
        html_list,
        footer()
    )
}

yearfiles <-
    rev(list.files(
        path = "graphs/year-tracking/",
        pattern = "vote-tracking.*\\.png$",
        full.names = TRUE
    ))

analysisfiles <-
    list.files(
        path = "graphs/",
        pattern = "vote-.*\\.png$",
        full.names = TRUE
    )

writeLines(
    make_doc(make_links(analysisfiles)),
    con = "index.html"
)

writeLines(
    make_doc(make_links(yearfiles)),
    con = "prev_years.html"
)
