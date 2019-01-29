import pandas as pd

dir = '~/Documents/ECON495/data_breach'
file = 'Privacy_Rights_Clearinghouse-Data-Breaches-Export.csv'
raw = pd.read_csv('{}/{}'.format(dir, file))
states = pd.read_csv('{}/states.csv'.format(dir))

states['region'] = states['region'].str.lower()
raw.columns = [x.replace(' ', '_') for x in raw.columns.str.lower()]
print(raw.columns)
data = raw.loc[raw.loc['state'] in states['region'], :]
print(data)
