import pandas
from fuzzywuzzy import process

compustat_database = 'archive/COMPUSTAT_database.csv'
data = pandas.read_csv(compustat_database)

def lookup_company(comp):
    choices = list(set(data['conm']))
    matches = process.extract(comp, choices, limit=10)
    print(data[data['conm'].isin([match[0] for match in matches])][['conm', 'GVKEY', 'CUSIP', 'tic', 'sic']].drop_duplicates())


def lookup_tic(comp):
    choices = list(set(data['tic']))
    matches = process.extract(comp, choices, limit=10)
    print(data[data['tic'].isin([match[0] for match in matches])][['conm', 'GVKEY', 'CUSIP', 'tic', 'sic']].drop_duplicates())
