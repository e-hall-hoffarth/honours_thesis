import pandas
from fuzzywuzzy import process

crsp_database = '~/Documents/hons_thesis/data/CRSP_companies.csv'
data = pandas.read_csv(crsp_database)

def lookup_company(comp):
    choices = list(set(data['conm']))
    matches = process.extract(comp, choices, limit=10)
    print(data[data['conm'].isin([match[0] for match in matches])][['conm', 'GVKEY', 'tic', 'sic']].drop_duplicates())
