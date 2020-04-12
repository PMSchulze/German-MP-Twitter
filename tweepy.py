#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 23:23:30 2020

@author: patrickschulze
"""


import tweepy
import pandas as pd
import pickle as pickle

# set local working directory
# import os
# os.chdir('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse/')

#-----------------------------------------------------------------------------
#Add your credentials here
twitter_keys = {
        'consumer_key':        'MpdDoacsyDgW6Bu4CeCVl13Cc',
        'consumer_secret':     'hCUwwMGaS83zPs6pkihWqS3FPek2uVDDxrBWYg9ooJ670pmC8V',
        'access_token_key':    '1243953821220589568-OHPXrSYgIDX5vyvosUKmLEZCxvLgzR',
        'access_token_secret': 'nP93r3AbO05HvzLELGaJjcrmj85gXLN2MmgdkwFMhpOtk'
    }

#Setup access to API
auth = tweepy.OAuthHandler(twitter_keys['consumer_key'], twitter_keys['consumer_secret'])
auth.set_access_token(twitter_keys['access_token_key'], twitter_keys['access_token_secret'])

api = tweepy.API(auth)

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

def download_tweets(username):
    #initialize a list to hold all the tweepy Tweets
    alltweets = []
	
	#make initial request for most recent tweets (200 is the maximum allowed count)
    new_tweets = api.user_timeline(screen_name = username,count=200,tweet_mode="extended")
	
	#save most recent tweets
    alltweets.extend(new_tweets)
	
	#save the id of the oldest tweet less one
    oldest = alltweets[-1].id - 1
	
	#keep grabbing tweets until there are no tweets left to grab
    while len(new_tweets) > 0:		
		#all subsiquent requests use the max_id param to prevent duplicates
        new_tweets = api.user_timeline(screen_name = username,count=200,max_id=oldest,tweet_mode="extended")
		
		#save most recent tweets
        alltweets.extend(new_tweets)
		
		#update the id of the oldest tweet less one
        oldest = alltweets[-1].id - 1
		
        print("%s tweets downloaded so far" % (len(alltweets)))
	
	#transform the tweepy tweets into a 2D array that will populate the csv	
    outtweets = pd.DataFrame([tweet.__dict__ for tweet in alltweets])
    #outtweets = [[tweet.id_str, tweet.created_at, tweet.text.encode("utf-8")] for tweet in alltweets]
    return(outtweets)

twitter_usernames = twitter_account['url'].apply(get_twitter_username)
twitter_usernames.rename("username", inplace = True)
twitter_account = pd.concat([twitter_account, twitter_usernames], axis = 1)

res = pd.DataFrame()
for username in twitter_account.iloc[0:3, 2]:
    res = pd.concat([res, download_tweets(username)])
    
# columns that might be most important for us: 
# 2,3,5,7,15,23,24,25,26,27,28
res.columns
res.iloc[:,[2,3,5,7,15,23,24,25,26,27]].columns
res.iloc[:,[2,3,5,7,15,23,24,25,26,27]]
