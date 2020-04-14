#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 14 14:02:51 2020

@author: patrickschulze
"""

import GetOldTweets3 as got
import pandas as pd

# create dataframe with columns 'Name' and 'Twitter Url'
def get_twitter_url(x):
    if 'Twitter' in x:
        return x['Twitter']
    else:
        return ''
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
