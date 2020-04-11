#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 23:23:30 2020

@author: patrickschulze
"""


import tweepy

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

#Make call on home timeline, print each tweets text
public_tweets = api.home_timeline()
for tweet in public_tweets:
    print(tweet.text)