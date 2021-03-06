import pandas as pd
import csv

compustat_location = 'archive/COMPUSTAT_database.csv'
breach_location = 'data/data_breaches_final.csv'
bea_location = 'archive/BEA_database.csv'
out_location = 'data/COMPUSTAT_merged.csv'

compustat = pd.read_csv(compustat_location)
breach = pd.read_csv(breach_location)
bea = pd.read_csv(bea_location)

compustat['datadate'] = pd.to_datetime(compustat['datadate'], format='%Y%m%d')
compustat = compustat[compustat['curcdq'] == 'USD']

breach['datacqtr'] = breach['yearquarter']
breach['gvkey'] = breach['GVKEY']
breach = breach[breach['match'] == 1]
breach['Date Made Public'] = pd.to_datetime(breach['Date Made Public'], format='%B %d, %Y')

bea['datacqtr'] = bea['yearquarter']

out = pd.merge(compustat, breach, how='outer', on=['gvkey', 'datacqtr'])
out = pd.merge(out, bea, how='left', on='datacqtr')

out.to_csv(out_location, index=False, quoting=csv.QUOTE_ALL)
