#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr  5 16:12:29 2020

@author: patrickschulze
"""

import GetOldTweets3 as got
import pandas as pd
import pickle as pickle
import re

#-----------------------------------------------------------------------------
## import Bundestag data
with open(r'/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse/abg_dict.pickle',\
          'rb') as handle:
    abg_dict = pickle.load(handle)

# convert into dataframe, add headers
bt_data = pd.DataFrame(abg_dict).transpose()
bt_data.columns = ['Name', 'Partei', 'Wahlart', 'Bundesland', 'Wahlkreis', \
                  'Ausschuesse', 'Soziale Medien', 'Biografie']

# create dataframe with columns 'Name' and 'Twitter Url'
def get_twitter_url(x):
    if 'Twitter' in x:
        return x['Twitter']
    else:
        return ''
url = bt_data['Soziale Medien'].apply(get_twitter_url)
twitter_account = pd.concat([bt_data['Name'], url], axis = 1, \
                            keys = ['Name','Twitter Url'])
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
def get_twitter_username(url):
    if url:
        return(url.split('/')[3].split('?')[0])
    else: 
        return('')
    
def download_tweets(username):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince("2020-03-04")\
                                               .setUntil("2020-06-04")\

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    return(df)

twitter_usernames = twitter_account['Twitter Url'].apply(get_twitter_username)

df = pd.DataFrame()
for username in twitter_usernames[0:10]:
    df = pd.concat([df, download_tweets(username)])
