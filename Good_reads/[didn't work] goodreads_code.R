# Good Reviews Reviews 
install.packages("xml2")
install.packages("rvest")
install.packages("RSelenium")
install.packages("rvest")
install.packages("data.table")
install.packages("magrittr")

library(data.table)   # Required for rbindlist
library(dplyr)        # Required to use the pipes %>% and some table manipulation commands
library(magrittr)     # Required to use the pipes %>%
library(rvest)        # Required for read_html
library(RSelenium)    # Required for webscraping with javascript
library(xml2)


# Installing RSelenium 
url <- "https://www.goodreads.com/book/show/18619684-the-time-traveler-s-wife#other_reviews"
book.title <- "The time traveler's wife"
output.filename <- "GR_TimeTravelersWife.csv"

rD <- rsDriver(chromever = "110.0.5481.77")
remDr <- rD[["client"]]
remDr$navigate(url)

global.df <- data.frame(book = character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)




remDr <- remoteDriver(browserName = "firefox", port = 5556) # instantiate remote driver to connect to Selenium Server
remDr$open() # open web browser
remDr$navigate(url)
remDr$getStatus()
# Initialize the data frame
global.df <- data.frame(book=character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)

# Finding the elements 

reviews <- remDr$findElements("css selector", "#bookReviews.stacked")

reviews.html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
reviews.text <- unlist(reviews.list)

# Cleaning the text
# Removing all characters that are not letters or dash
reviews.text2 <- gsub("[^A-Za-z\\-]|\\.+", " ", reviews.text)
# Removing the end of line characters and extra spaces
reviews.clean <- gsub("\n|[ \t]+", " ", reviews.text2)

