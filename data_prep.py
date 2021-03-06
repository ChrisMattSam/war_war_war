# -*- coding: utf-8 -*-
"""
Created on Mon Mar  2 17:16:32 2020

@author: csampah
"""

import pandas as pd
from statsmodels.stats.outliers_influence import variance_inflation_factor

war1 = pd.read_csv("intrastate.csv")

'Step 1.1: Impute for unusable values'
'Change negative month values to month 6, close to the average of all months (6.5)'
war1.start_month1.replace(-9 or -8,6,inplace=True)
war1.end_month1.replace(-9 or -8,6, inplace = True)
war1 = war1[war1.end_month1 != -7] #'Drop the -7 anomaly value'

'Step 1.2: Drop unused time fields'
'time will be calculated exclusively in months s.t. days will be removed'
'pauses in conflict will be ignored for now, time from official start to final conclusion only'
war1.drop(columns = ['start_day1','start_year2', 'end_year2', 'start_month2', 'start_day2',
         'end_day1', 'end_month2','end_day2','previous_war'], inplace=True) 

'Date: Create a new field for the full date'
war1['start_date'] = 'placeholder'
war1['end_date'] = 'placeholder'

for index, row in war1.iterrows():
    war1.start_date = war1.start_month1.apply(str) + '/1/'+ war1.start_year1.apply(str)
    war1.end_date = war1.end_month1.apply(str) + '/1/'+ war1.end_year1.apply(str)
    
war1['start_date']= war1['start_date'].astype('datetime64')
war1['end_date']= war1['end_date'].astype('datetime64')

'Combat fatalities: drop rows with missing battle deaths'
war1 = war1[war1.side1_fatalities != -9]
war1 = war1[war1.side2_fatalities != -9]

'change blank fatality counts to zero'
war1.side1_fatalities.replace(-8,0, inplace = True)
war1.side2_fatalities.replace(-8,0, inplace = True)

k= {'War': list(range(4,8)), 'Type': ['Central Control', "Local Issues", "Regional Internal",  "Intercommunal"]}

'Step 2: Change war outcome to binary'
key = pd.DataFrame(k)
war1.outcome.replace(2 or 3, 1, inplace=True)
war1.outcome.replace(4 or 5 or 6 or 7, 0, inplace=True)

t = ["Win-Loss", "Win-Loss", "Compromise", "Another War", "Ongoing Conflict", "Stalemate", "Grudging Peace"]

kk = {'First Outcome': list(range(1,8)), 'New Outcome': [1]*3 +[0]*4, 'Type': t}
outcome_key = pd.DataFrame(kk)

'Step 3: Create and insert new fields: fatalities and duration of conflict(years) with revised date fields'
war1['fatalities'] = 'placeholder'
war1['duration'] = 'placeholder'

for index, row in war1.iterrows():
    war1.fatalities = war1.side1_fatalities + war1.side2_fatalities
    war1.duration = war1.end_date - war1.start_date

war1.duration= round((war1['duration'].astype('timedelta64[D]'))/365,3)
war1=war1[war1.duration !=0] # zero duration corresponds to bad data


'Step 3.5: Start creating dataset for the model'
include = ['war_id','start_date','start_year1','duration','combat_location', 'fatalities','war_type']
war1 = war1[include]


'Step 3.5.2: Change all start dates to the earliest start date per each repeating war id'
'Get war_ids that repeat'
freq = war1.war_id.value_counts()
repeat_ids = freq[freq !=1]

for index, row in repeat_ids.iteritems():
    test_set = war1[war1['war_id']==index].start_date
    mini = min(test_set)
    print(index)
    print(test_set)
    for index,row in test_set.iteritems():
        test_set[index] = mini

    war1[war1['war_id']==index].start_date = test_set

