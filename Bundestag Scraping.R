

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









