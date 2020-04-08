

library(plyr)
library(tidyverse)
library(magrittr)
library(rvest)
library(XML)
library(stringi)
library(pbapply)

#Twitter Namen von CDU Fraktionsseite scrapen

landing_page <- read_html("https://www.cducsu.de/hier-stellt-die-cducsu-bundestagsfraktion-ihre-abgeordneten-vor")


partei2 <- landing_page %>% 
  html_nodes("li.twitter > a") %>% html_attr("href") %>% 
  as_tibble()



