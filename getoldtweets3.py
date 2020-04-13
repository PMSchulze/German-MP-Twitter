#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr  5 16:12:29 2020

@author: patrickschulze
"""

import GetOldTweets3 as got
import pandas as pd
import pickle as pickle

# set local working directory
import os
os.chdir('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse/')

#-----------------------------------------------------------------------------
## import Bundestag data
with open('abg_df.pickle', 'rb') as handle:
    bt_data = pickle.load(handle)

# create dataframe with columns 'Name' and 'Twitter Url'
def get_twitter_url(x):
    if 'Twitter' in x:
        return x['Twitter']
    else:
        return ''
url = bt_data['Soziale Medien'].apply(get_twitter_url)
twitter_account = pd.concat([bt_data['Name'], url], axis = 1, \
                            keys = ['name','url'])
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# function to get twitter usernames from twitter url
def get_twitter_username(url):
    if url:
        return(url.split('/')[3].split('?')[0])
    else: 
        return('')

# function to download tweets for a specific user 
def download_tweets(username, since, until):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince(since)\
                                               .setUntil(until)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    return(df)


twitter_usernames = twitter_account['url'].apply(get_twitter_username)
twitter_usernames.rename("username", inplace = True)
twitter_account = pd.concat([twitter_account, twitter_usernames], axis = 1)
    
res = pd.DataFrame()
for username in twitter_account.iloc[0:3, 2]:
    res = pd.concat([res, download_tweets(username, since = "2017-09-24", \
                                          until = "2020-04-08")])
#-----------------------------------------------------------------------------

# possible to download more than 3200 tweets:
# res = download_tweets('realDonaldTrump',since = "2018-09-24", until = "2020-04-08")
        
# however, some tweets appear to be missing (and some rows are empty)
# res = download_tweets('realDonaldTrump',since = "2020-04-10", until = "2020-04-14")
