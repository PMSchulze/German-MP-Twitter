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

api = tweepy.API(auth)

# create dataframe with columns 'Name' and 'Twitter Url'
def get_twitter_url(x):
    if 'Twitter' in x:
        return x['Twitter']
    else:
        return ''

# function to get twitter usernames from twitter url
def get_twitter_username(url):
    if url:
        return(url.split('/')[3].split('?')[0])
    else: 
        return('')

# function to download tweets for a specific user 
def download_tweets_got3(username, since, until):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince(since)\
                                               .setUntil(until)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    return(df)

def download_tweets_tweepy(username):
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