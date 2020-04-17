# -*- coding: utf-8 -*-
"""
Created on Fri Apr 17 09:48:55 2020

@author: Korbi
"""

from bs4 import BeautifulSoup 
import requests 
import csv

#getting all links to the individual mdb pages
source = requests.get('https://www.gruene-bundestag.de/abgeordnete').text
soup = BeautifulSoup(source, 'lxml')

allWrappers = soup.find_all('a', class_="abgeordneteTeaser__wrapper")

links = []
for a in allWrappers:
    link = a['href']
    links.append(link)
    
urlbase = 'https://www.gruene-bundestag.de'

urls = []
for link in links:
    url = urlbase + str(link)
    urls.append(url)
     


#just to look at it can be deleted 
csv_file = open('GrueneMdbs.csv', 'w')
csv_writer = csv.writer(csv_file)
csv_writer.writerow(['Partei', 'Name', 'Links'])



#scraping each individual site

twitter_list = []

for url in urls:
    source = requests.get(url).text
    greensoup = BeautifulSoup(source, 'lxml')
    
    #get Name 
    name = greensoup.find('h1').text
    
    #get Twitter
    hrefss = []
    twitter = ""
    for x in greensoup.find_all(class_="weitereInfoTeaser"):
        
        for y in x.find_all('a', href = True):
            
            z = y['href']
            hrefss.append(z)
            
            
            for i in hrefss:
                if "twitter" not in i:
                    continue 
                else:
                    twitter= i              
            
    twitter = twitter.lstrip('http://twitter.com/')
    twitter = twitter.lstrip('https://twitter.com/')
    
    #Append list
    twitter_list.append(
            {
              'partei': "Die Gruenen",
              'name' : name,
              'twitter' : twitter})
    
    #CSV 
    csv_writer.writerow(['Die Gruenen', name, twitter])
    
print(twitter_list)  

 #CSV 
csv_file.close()


    
    