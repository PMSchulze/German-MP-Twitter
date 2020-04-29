#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 29 22:36:55 2020

@author: patrickschulze
"""

#import os
#os.chdir('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse')

import pandas as pd
import pickle

with open('tweepy_df_test.pickle', 'rb') as handle:
    tweepy_df_test = pickle.load(handle)
    
tweepy_df_test.to_csv('tweepy_df_test.csv', index = False)