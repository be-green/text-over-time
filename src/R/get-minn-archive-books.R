# get books from Minnesota Archive
# this seems easier to parse than the Fed's
library(rvest)
library(httr)
library(data.table)
library(stringr)

all_feds <- c(
  "National Summary",
  "Atlanta",
  "Boston",
  "Chicago",
  "Cleveland",
  "Dallas",
  "Minneapolis",
  "New York",
  "Philadelphia",
  "Richmond",
  "San Francisco",
  "St Louis"
)

# this submits a post request to the form associated with the book
# you may need to change this if it doesn't work
search_fed_archives <- function(fed) {
  resp <- POST(
    url = "https://www.minneapolisfed.org/region-and-community/regional-economic-indicators/beige-book-archive",
    encode = "form",
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:86.0) Gecko/20100101 Firefox/86.0"),
    add_headers(
      `Referer` = "https://www.minneapolisfed.org/region-and-community/regional-economic-indicators/beige-book-archive"
    ),
    body = list(
      bb_district=fed,
      bb_year="Any")
  )
  read_html(resp)
}

# get the table entries and parse them into
# components (fed name, date, link)
get_beige_book_links <- function(fed_html) {
    html_nodes(fed_html, "tr") %>%
        lapply(function(x) {
          x <- html_nodes(x, "td")
          text <- html_text(x)
          link <- html_attr(html_nodes(x, "a"), "href")
          data.table(fed = text[1], date = text[2],
                     link = link[1])
        }) %>%
        rbindlist %>%
    .[!is.na(link)]

}

process_archive <- function(fed_name) {
  html <- search_fed_archives(fed_name)
  get_beige_book_links(html)
}

all_fed_links <- lapply(
  all_feds,
  process_archive
) %>%
  rbindlist

get_text_from_link <- function(link) {
  message("Running ", link)
  read_html(paste0("https://www.minneapolisfed.org", link)) %>%
    html_nodes(".offset-lg-1") %>%
    html_text2 %>%
    str_replace_all(
      "\r \r \r \r\n\n‹ Back to Archive Search\n\n\r \r\n",
      ""
    ) %>%
    str_replace("[A-Za-z: ]+", "") %>%
    str_trim
}


all_text <- sapply(all_fed_links$link, get_text_from_link)

all_fed_links[,text := sapply(link, get_text_from_link)]
all_fed_links[,date := str_extract(text, ".*")]

for(i in 1:nrow(all_fed_links)) {
  name_info <- all_fed_links[i,]
  text <- all_text[i]
  date <- str_extract(text, ".*")

  fname <- paste0("data/raw/beige-book/",
                  name_info$fed, "-",
                  date, ".txt")
  writeLines(text, fname)
}



