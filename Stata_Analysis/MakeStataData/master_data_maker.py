#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May  3 11:10:13 2020

@author: rachelanderson

This code reads in all raw CSV files and merges them into a CSV that Stata 
can input easily 

Input files should have the same form of id vars:
    
    state (as postal code)
    year 
    
Variables to be merged in are:
    electricity prices (residential)
    % revenue by customer source (industrial, comm., residential)
    % annual generation by energy source
    annual capacity additions by tech type
    cumulative capacity by tech type
    
Then add variables using recode variable function to add state variables
    rps
    net_meter 
    1603 funds received
    regulated? (restructured)
    
if missing, fill with something

"""

import pandas as pd
import json
import glob


inputDir = "./InputData/"
outputDir = "./OutputData/"
jsonDir = "../../../Data for Tax Equity Project/JSONFiles/"

df_dict = {}
master_df = pd.DataFrame()

for fileName in glob.glob(inputDir + "*.csv"):
    
    df_name = fileName.split(inputDir)[-1].split('.csv')[0]
    print(df_name)
    # ignore monthly data for now
    if "monthly" in fileName:
        continue
    
    df = pd.read_csv(fileName)
    df.drop([x for x in df.columns if "Unnamed" in x],axis=1,inplace=True)
#    print(df_name + ": " + df.columns)
    df_dict[df_name] = df
    
    
master_df = df_dict['annual_generation_data'].merge(df_dict['annual_capacity_data'],
       on=['state','year','energy_source'])

# number of observations drops because price data missing for some states
master_df = master_df.merge(df_dict['annual_price_data'], on=['state','year'])

master_df = master_df.merge(df_dict['grant_data'], how='outer', on=['state','energy_source'])
master_df['funded']=master_df['funded'].fillna(0)

# finally, add RPS policies, regions with appropriate dictionary code  
recode_dict = {}
for fileName in glob.glob(jsonDir + "*.txt"):
    dict_name = fileName.split(jsonDir)[-1].split('.txt')[0]
    print(dict_name)
    with open(fileName) as outfile:
        recode_dict[dict_name] = json.load(outfile)
        
master_df['region'] = master_df['state'].map(recode_dict['region_dict'])
master_df['regulated'] = master_df['state'].map(recode_dict['regulated_dict'])
master_df['rps'] = master_df['state'].map(recode_dict['rps_dict'])
master_df['net_meter'] = master_df['state'].map(recode_dict['net_meter'])
master_df['tot_funded'] = master_df['state'].map(lambda x: recode_dict['funded_dict'][x]
 if x in recode_dict['funded_dict'] else 0)
master_df['num_funded'] = master_df['state'].map(lambda x: recode_dict['num_funded_dict'][x]
 if x in recode_dict['num_funded_dict'] else 0)
master_df['avg_1603_funding'] = master_df['tot_funded']/master_df['num_funded'] 
 
master_df.to_csv(outputDir + 'annual_state_data.csv')

# add number of unique parent utilities operating in the state? by year 
