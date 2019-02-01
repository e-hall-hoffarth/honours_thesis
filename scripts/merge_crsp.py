import pandas as pd
import csv
import datetime

crsp_location = 'archive/CRSP_small_database.csv'
breach_location = 'data/data_breaches_final.csv'
ff_location = 'data/FF_factors.csv'
out_location = 'data/CRSP_merged.csv'

crsp = pd.read_csv(crsp_location)
breach = pd.read_csv(breach_location)
ff = pd.read_csv(ff_location)

breach['date'] = pd.to_datetime(breach['Date Made Public'], format='%B %d, %Y')
crsp['date'] = pd.to_datetime(crsp['date'], format='%Y%m%d')
ff['date'] = pd.to_datetime(ff['date'], format='%Y%m%d')

breach['TICKER'] = breach['tic']
breach = breach[breach['match'] == 1]

out = pd.merge(crsp, breach, how='outer', on=['date', 'TICKER'])
out = pd.merge(out, ff, how='left', on='date')

out.to_csv(out_location, index=False, quoting=csv.QUOTE_ALL)
