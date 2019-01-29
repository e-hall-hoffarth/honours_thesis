import pandas as pd
import csv
import datetime
import dateparser

crsp_location = '~/Documents/hons_thesis/data/CRSP_database.csv'
breach_location = '~/Documents/hons_thesis/data/data_breaches_final.csv'
out_location = '~/Documents/hons_thesis/data/CRSP_merged.csv'

crsp = pd.read_csv(crsp_location)
breach = pd.read_csv(breach_location)

breach['date'] = breach['Date Made Public'].apply(lambda x: dateparser.parse(x).strftime('%Y%m%d'))
breach['TICKER'] = breach['tic']
breach = breach[breach['match'] == 1]

out = pd.merge(crsp, breach, how='outer', on=['date', 'TICKER'])

out.to_csv(out_location, index=False, quoting=csv.QUOTE_ALL)
