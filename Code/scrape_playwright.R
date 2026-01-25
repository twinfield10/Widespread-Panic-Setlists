library(rvest)

fetch_html_playwright <- function(url) {
  cmd <- sprintf(
    "python3 Code/fetch_page.py %s",
    shQuote(url)
  )
  
  html <- system(cmd, intern = TRUE)
  read_html(paste(html, collapse = "\n"))

  return(html)
}

fetch_html_playwright(
  "http://everydaycompanion.com/asp/tour99.asp"
)