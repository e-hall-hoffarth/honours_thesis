from pytrends.request import TrendReq
import pandas as pd
import numpy as np
import datetime
import csv
import time
import re

complete = ['BAC','DSW','CPS.1','MCIP','C','KODK','CVS','MSI','AMTD','RL','VZ','JPM','CYN','COF','HRB','BA','F','SWY','WMT','PZZA','MAR','AMP','GUID','HON','AT.2','OMX','FDX','GM','IBM','NYT','MFE','PRU','AFL','AIG','ADP','ED','CFR','EFX','FITB','MTB','LOW','BAC2','WFC','UNP','SRSCQ','BSC.1','PGR','ING','ALL','CVX','DBD','DIS','D','BEN','GE','T','WY','WSM','CLGX','NSANY','TM','BSG.Z','CPRT','MVGRQ','DLTR','GPI','UWBKQ','NNI','AWI','DFS','CI','DEBS','HTZ','IR','SHLDQ','KEY','S','WSC.2','DTV.2', 'ATPL']
remove = ['CORP', 'INC', 'LTD', 'CORPORATION', 'INCORPORATED', 'LLC', 'LTD', 'GROUP', 'NV', 'GRP', 'PLC']
out = 'data/trends.csv'
cs = pd.read_csv('data/COMPUSTAT_merged.csv')
cs['date'] = pd.to_datetime(cs['datadate'], format='%Y-%m-%d')
cs_todo = cs[cs['match'] == 1 & ~cs['tic_x'].isin(complete)]


companies = cs_todo[['tic_x', 'conm_x']].drop_duplicates().dropna()
search_terms = zip(list(companies['tic_x']), list(companies['conm_x']))

pytrends = TrendReq(hl='en_US')
count = 1

def clean_name(name):
    namewords = name.split()
    resultwords = [word for word in namewords if word not in remove]
    result = ' '.join(resultwords)
    result = result.strip(' ')
    return result

for tic, conm in search_terms:
    print(f'\'{tic}\',')
    conm = re.sub(r'[^\s\w]+', '', conm)
    conm = clean_name(conm)
    tic = re.sub(r'[^\s\w]+', '', tic)
    pytrends.build_payload([tic, conm], timeframe='2005-01-01 2017-12-31', geo='US')
    result = pytrends.interest_over_time()
    try:
        del result['isPartial']
    except KeyError:
        pass

    # print(f'Found result:')
    # print(result.head())

    if count == 1:
        trends = result
        result.to_csv(f'data/trends/{tic}_{conm}.csv', index=True, quoting=csv.QUOTE_ALL)
        count += 1

    else:
        trends = trends.join(result)
        result.to_csv(f'data/trends/{tic}_{conm}.csv', index=True,  quoting=csv.QUOTE_ALL)
        # print('Results so far:')
        # print(trends)

    # tic_flat = result[tic].reset_index()
    # tic_flat.columns = ['date', 'trend_tic']
    # tic_flat['tic_x'] = tic
    # tic_flat['date'] = pd.to_datetime(tic_flat['date'], format='%Y-%m-%d')

    # conm_flat = result[conm].reset_index()
    # conm_flat.columns = ['date', 'trend_conm']
    # conm_flat['conm_x'] = conm
    # conm_flat['date'] = pd.to_datetime(conm_flat['date'], format='%Y-%m-%d')

    # print('Converted to tic table:')
    # print(tic_flat.head())
    
    # print('Converted to conm table:')
    # print(conm_flat.head())

    # cs = pd.merge(cs, tic_flat, how='outer', on=['date', 'tic_x'])
    # cs = pd.merge(cs, conm_flat, how='outer', on=['date', 'conm_x'])
    # cs.to_csv('data/COMPUSTAT_merged_trend_work.csv')

    time.sleep(62)

trends.to_csv(out, index=True, quoting=csv.QUOTE_ALL)

