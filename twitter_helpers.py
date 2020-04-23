#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 14 14:02:51 2020

@author: patrickschulze
"""
import tweepy
import GetOldTweets3 as got
import pandas as pd

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
api = tweepy.API(auth, wait_on_rate_limit=True)

# function to download tweets for a specific user with:       
## - GetOldTweets3
def download_tweets_got3(username, since, until):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince(since)\
                                               .setUntil(until)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    return(df)
## - Tweepy
def download_tweets_tweepy(username):
    #initialize a list to hold all the tweepy Tweets
    alltweets = []
    try:	
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
        #convert output to pandas DataFrame	
        outtweets = pd.DataFrame([tweet.__dict__ for tweet in alltweets])
        #add boolean column for availability
        outtweets.insert(0, 'available', True)
        #add column with username
        outtweets.insert(0, 'username', username)
    except:
        print('data for user %s cannot be downloaded' %username)
        #if user cannot be downloaded return df with two columns:
        #username=username, available='False'
        outtweets = pd.DataFrame([username], columns=['username'])
        outtweets.insert(1, 'available', False)
    return(outtweets)