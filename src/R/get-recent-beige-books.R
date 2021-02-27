# get beige book contents as text
library(rvest)
library(data.table)

# this has books back to 1996, 8 times a year
recent_archive <- "https://www.federalreserve.gov/monetarypolicy/beige-book-archive.htm"

make_full_link <- function(links) {
  sapply(links, function(link) {
    if(link %like% "^/")  {
      link = paste0("https://www.federalreserve.gov", link)
    } else {
      link
    }
  })
}

#' Get links to beige book from table at the relevant link
#' Returns a data.table with the year and the pdf/html links to the
#' 8 beige books
#' @param annual_link_table data.table with year and link
get_book_links <- function(annual_link_table) {
  message("Running ", annual_link_table$year)
  all_books <- read_html(annual_link_table$link) %>%
    html_nodes("tr") %>%
    lapply(function(x) {
      td <- html_nodes(x, "td")
      date <- html_text(td[1])
      links <- html_nodes(td[2], "a") %>%
        html_attr("href")
      data.table(date = date,
                 html_link = links[1],
                 pdf_link = links[2])
    }) %>%
    rbindlist %>%
    .[!is.na(date)]

  data.table(annual_link_table, all_books)
}


# get the book links from the federal reserve website
# then grab the year and the href (link) to the book
annual_links <- read_html(recent_archive) %>%
  html_nodes(".panel-body") %>%
  html_nodes("li") %>%
  html_nodes("a") %>%
  lapply(function(x) {
    data.table(year = html_text(x),
               link = paste0("https://www.federalreserve.gov",
                             html_attr(x, "href")
                             )
               )
  }) %>%
  lapply(get_book_links) %>%
  rbindlist

annual_links[,html_link := make_full_link(html_link)]

# I'm going to use html links because some pdf links are missing



