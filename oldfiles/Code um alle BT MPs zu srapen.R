#Korbinian Webscraping Bundestagsabgeordnete

#Schritt 1 Lade benötigte Packages 

library(plyr)
library(tidyverse)
library(magrittr)
library(rvest)
library(XML)
library(stringi)
library(pbapply)




# Schritt 2 Write Scrape Functions --------------------------------------------------------



link_scraper <- function(url) {
  Sys.sleep(0.6)
  
  
  links <- url %>% read_html() %>% 
    html_nodes("h2 > a") %>%
    html_attr("href")
}

abgeordneten_scraper <- function(url) { 
  
  Sys.sleep(0.6)

  names <- url %>% read_html() %>% 
    html_nodes("h2 > a") %>% 
    html_text() %>% 
    as_tibble()
  
}

partei_scraper <- function(url) {
  
  Sys.sleep(0.6)
  
  partei <- url %>% read_html %>% 
    html_nodes(xpath = "/html/body/div[2]/main/div[4]/div[2]/*/div[1]/div") %>% 
    html_text() %>% 
    as_tibble()
}


ausschuss_scraper <- function(url) {
  
  Sys.sleep(0.3)
  
  ausschuss <- url %>% read_html() %>% 
    html_nodes(xpath = "/html/body/div[2]/main/div[2]/section[3]/div[2]/*/*/*/a") %>% 
    html_text() %>% 
    rbind() %>% 
    as_tibble()
}



# Ausfuehrung  ------------------------------------------------------------

#Schritt 3 get the number of results pages 

landing_page <- read_html("https://www.abgeordnetenwatch.de/bundestag/profile?page=1")

number_of_result_pages <- landing_page %>% 
  html_nodes(xpath = '/html/body/div[2]/main/div[4]/div[2]/ul/li[10]/a') %>% 
  html_attr("href") %>% 
  as.character() %>% 
  str_extract("[0-9]+") %>% 
  str_trim() %>% 
  as.numeric() 


#alle urls zusammenfügen

sequence <- seq(1, number_of_result_pages)
urls1 <- paste0("https://www.abgeordnetenwatch.de/bundestag/profile?page=", sequence)

#page1 muss noch hinzugefügt werden 
page1 <- "https://www.abgeordnetenwatch.de/bundestag/profile"

urls <- append(urls1, page1)
rm(page1)
rm(urls1)

#Links der jeweiligen Abgeordnetenseite fuer die Ausschuesse

linksForAusschuss <- pblapply(urls, link_scraper)
linksForAusschuss <- unlist(linksForAusschuss)


urlsAusschuss <- paste0("https://www.abgeordnetenwatch.de", linksForAusschuss)
rm(linksForAusschuss)


# Abgeordnete und Parteien scrapen
abgeordneteBT <- pblapply(urls, abgeordneten_scraper)
parteien <- pblapply(urls, partei_scraper)


#unlisten und Abgeordnete und Parteien verbinden 

abgeordneteBT <- unlist(abgeordneteBT) %>%  as_tibble()
parteien <- unlist(parteien) %>%  as_tibble()

finalList <- cbind(abgeordneteBT, parteien)
rm(abgeordneteBT)
rm(parteien)

#### 

ausschuesse <- pblapply(urlsAusschuss, ausschuss_scraper)

#this actually "unlists" the data frame
x <- do.call(rbind.fill, ausschuesse)

actualFinalList <- cbind(finalList, x)

write.csv(actualFinalList, file = "BTMPs.csv")


