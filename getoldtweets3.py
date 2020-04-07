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
                            keys = ['Name','Url'])
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# function to get twitter usernames from twitter url
def get_twitter_username(url):
    if url:
        return(url.split('/')[3].split('?')[0])
    else: 
        return('')

# function to download tweets for a specific user 
def download_tweets(username):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince("2017-09-24")\
                                               .setUntil("2020-04-07")

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    return(df)


twitter_usernames = twitter_account['Url'].apply(get_twitter_username)
twitter_usernames.rename("Username", inplace = True)
twitter_account = pd.concat([twitter_account, twitter_usernames], axis = 1)
    
res = pd.DataFrame()
for username in twitter_usernames[0:3]:
    res = pd.concat([res, download_tweets(username)])
#-----------------------------------------------------------------------------
