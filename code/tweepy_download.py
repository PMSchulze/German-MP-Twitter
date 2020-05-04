#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 19 11:37:06 2020

@author: patrickschulze
"""

import pandas as pd
import pickle
import tweepy_helpers as th

# import Bundestag data
with open('abg_df.pickle', 'rb') as handle:
    bt_data = pickle.load(handle)
    
# select name and username for each member and store in table called twitter_account
names = bt_data['Name']
twitter_usernames = bt_data['Twitter']
names.rename("name", inplace = True)
twitter_usernames.rename("username", inplace = True)
twitter_account = pd.concat([names, twitter_usernames], axis = 1)

# drop usernames that are nan or empty (i.e. parliamentarians with no account)
usr_nan = twitter_account.username.isna()
usr_empty = twitter_account.username == ''
twitter_account = twitter_account[~(usr_nan | usr_empty)]
twitter_account.reset_index(drop = True, inplace = True)

# download most recent tweets using tweepy (at most 3200 tweets per user)
tweepy_df = pd.DataFrame()
for username in twitter_account['username']:
    tweepy_df = pd.concat([tweepy_df, th.download_tweets_tweepy(username)])
# add column 'name'
tweepy_df = twitter_account.merge(tweepy_df, on = 'username')

# save data
with open('tweepy_df.pickle', 'wb') as handle:
    pickle.dump(tweepy_df, handle, protocol=pickle.HIGHEST_PROTOCOL)