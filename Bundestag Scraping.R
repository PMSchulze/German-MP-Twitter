

library(plyr)
library(tidyverse)
library(magrittr)
library(rvest)
library(XML)
library(stringi)
library(stringr)
library(pbapply)

#Twitter Namen von CDU Fraktionsseite scrapen

CDUSeite <- read_html("https://www.cducsu.de/hier-stellt-die-cducsu-bundestagsfraktion-ihre-abgeordneten-vor")
CDU <- CDUSeite %>% 
  html_nodes("li.twitter > a") %>% html_attr("href", default = "NA") %>% 
  as_tibble() 

NamenCDU <- CDU %>% mutate(value = str_replace(value, "http://twitter.com/", ""))
NamenCDU <- NamenCDU %>%  mutate(value = str_replace(value, "https://twitter.com/", ""))
NamenCDU <- NamenCDU %>% mutate(value = str_replace(value, "http://www.twitter.com/", ""))
rm(CDU)
rm(CDUSeite)

SPDSeite <- read_html("https://www.spdfraktion.de/abgeordnete/alle?wp=19&view=list&old=19")
SPD <- SPDSeite %>% 
  html_nodes("li > a.ico_twitter") %>% html_attr("href", default = "NA") %>% 
  as_tibble() 

NamenSPD <- SPD %>%mutate(value = str_replace(value, "http://twitter.com/", ""))
NamenSPD <- NamenSPD %>% mutate(value = str_replace(value, "https://twitter.com/", ""))
NamenSPD <- NamenSPD %>% mutate(value = str_replace(value, "http://www.twitter.com/", ""))
rm(SPD)
rm(SPDSeite)


FDPSeite <- read_html("https://www.fdpbt.de/fraktion/abgeordnete")
FDP <- FDPSeite %>% 
  html_nodes("div > a.tw") %>% html_attr("href", default = "NA") %>% 
  as_tibble() 
NamenFDP <- FDP %>%mutate(value = str_replace(value, "http://twitter.com/", ""))
NamenFDP <- NamenFDP %>% mutate(value = str_replace(value, "https://twitter.com/", ""))
NamenFDP <- NamenFDP %>% mutate(value = str_replace(value, "http://www.twitter.com/", ""))
NamenFDP <- NamenFDP %>% mutate(value = str_replace(value, "https://www.twitter.com/", ""))
rm(FDP)
rm(FDPSeite)



#Get AFD page
AFDSeite = read_html("https://www.afdbundestag.de/abgeordnete/")

#Getting all links to individual sites
LinksAFD = AFDSeite %>% html_nodes("div.fusion-column-wrapper > a") %>% 
  html_attr("href")
LinksAFD <- LinksAFD[1:89]
allinfos <- data.frame()
URLAfD <- paste0("https://www.afdbundestag.de", LinksAFD)
rm(AFDSeite)
rm(LinksAFD)

#Write scraper functions

AfdScraper <- function(url) {
  
  name <- url %>%  read_html() %>% html_nodes("h1") %>%  
   html_text() 
    
  twitter <- url %>%  read_html() %>% html_nodes("td > a") %>%  
   html_attr("href")
  tempdf <- data.frame(name, "AfD", twitter) 
  allinfos <- rbind(allinfos, tempdf) 
              
}

AfDNamenScraper <- function(url) {
  names <- url %>%  read_html() %>% html_nodes("h1") %>%  
    html_text() 
}

# get Names

namen <- pblapply(URLAfD, AfDNamenScraper)

# get Twitter
AfDNamen <- pblapply(URLAfD, AfdScraper)
AfDNamen <- do.call(rbind.fill, AfDNamen)
AfDFinal <- AfDNamen[c(20,37,50,56,68,73,91,97,104,117,131,139,172,180,203,211,221,240,256,275,286,299,321,342,355,361,367,389),]

AfD <- AfDFinal[,3] %>%  as_tibble()
AfD <- AfD %>% mutate(value = str_replace(value, "http://twitter.com/", ""))
AfD <- AfD %>% mutate(value = str_replace(value, "https://twitter.com/", ""))
AfD <- AfD %>% mutate(value = str_replace(value, "http://www.twitter.com/", ""))
AfD <- AfD %>% mutate(value = str_replace(value, "https://www.twitter.com/", ""))


AfD <- cbind(AfDFinal, AfD)
AfD <- AfD[,c(1,2,4)]

namen <- unlist(namen)
namen <- namen %>%  as_tibble()


colnames(AfD) <- c("value", "Partei", "Twitter")
test <- merge(AfD, namen, all = TRUE)

write.csv(test, file = "AfDFinal.csv")
