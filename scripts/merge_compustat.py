import pandas as pd
import csv

compustat_location = '~/Documents/hons_thesis/data/COMPUSTAT_database.csv'
breach_location = '~/Documents/hons_thesis/data/data_breaches_final.csv'
out_location = '~/Documents/hons_thesis/data/COMPUSTAT_merged.csv'

compustat = pd.read_csv(compustat_location)
breach = pd.read_csv(breach_location)

breach['datacqtr'] = breach['yearquarter']
breach['gvkey'] = breach['GVKEY']
breach = breach[breach['match'] == 1]

out = pd.merge(compustat, breach, how='outer', on=['gvkey', 'datacqtr'])

out.to_csv(out_location, index=False, quoting=csv.QUOTE_ALL)
