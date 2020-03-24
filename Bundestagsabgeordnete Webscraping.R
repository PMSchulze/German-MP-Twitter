library(tidyverse)
library(magrittr)
library(rvest)
library(RSelenium)

#Liste der Bundestagsabgeordneten aus Wikipedia

wiki_entry <- read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(19._Wahlperiode)")

wiki_entry <- wiki_entry %>% html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% 
  html_table() %>% 
  as.matrix() %>% 
  as_tibble() 


#Betrachtung der gesuchten Spalten
wiki_entry <- wiki_entry[,c(2,4,6)]

#on Windows use shell instead of system
system('docker run -d -p 4445:4444 selenium/standalone-firefox')
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open()
remDr$navigate('https://www.bundestag.de/ausschuesse/a11')

button = remDr$findElement(using = "xpath", "//*[contains(@class, 'icon-list-bullet')]")
#button = button[1]
#button.click()
#time.sleep(3)

ausschuss <- remDr$findElements(using = "xpath", "//*[contains(@class, 'bt-teaser-person-text')]/h3")
ausschuss <- ausschuss$getElementText()

remDr$close()
# stop the selenium server
rD[["server"]]$stop() 
# if user forgets to stop server it will be garbage collected.
rD <- rsDriver()
rm(rD)
gc(rD)



names <- landing_page_AS %>% 
  html_nodes(".bt-teaser-person-text h3") %>% 
#  html_nodes(xpath = "//*[(@id = "bt-collapse-538348")]//h3") %>% 
#  html_nodes(xpath = "//*[contains(concat( " ", @class, " " ), concat( " ", "bt-teaser-person-text", " " ))]//h3") %>% 
  html_text() %>% 
  as_tibble()


