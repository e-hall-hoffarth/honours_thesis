import pandas as pd
import csv
import re
from fuzzywuzzy import process

f = './data_breaches.csv'
d = './CRSP.csv'
out = 'data_breaches_w_company_details_nocrit_rough.csv'
excluded = ['ltd', 'inc', 'corp', 'corporation', 'co', 'fd', 'plc', \
'trust', 'limited', 'grp', 'group', 'lp']

def normalize(name):
    name = str(name).lower()
    name = re.sub('[.,/?$]', '', name)
    name = ' '.join([word for word in name.split(' ') if word not in excluded])
    return name

with open(f, 'r') as breaches_db, open(d, 'r') as companies_db, open(out, 'w+') as outfile:
    lines = csv.DictReader(breaches_db)
    headers = [h for h in lines.fieldnames]
    # Remove unneeded information
    headers.remove('Description of incident')
    headers.remove('Information Source')
    headers.remove('Source URL')
    headers = headers + ['match_score','conm','orig_name','GVKEY','tic','sic']
    writer = csv.DictWriter(outfile, fieldnames=headers, quoting=csv.QUOTE_ALL)
    writer.writeheader()

    db_reader = csv.DictReader(companies_db)
    db_companies = {}
    for row in db_reader:
        db_companies[normalize(row['conm'])] = (row['conm'], row['tic'], row['sic'], row['GVKEY'])

    count = 0
    for line in lines:
        company = normalize(line['Company'])
        match = process.extractOne(company, db_companies.keys())
        print('Matched {} to {}'.format(company, match[0]))
        count = count + 1

        matched_row = db_companies[match[0]]
        conm = matched_row[0]
        tic = matched_row[1]
        sic = matched_row[2]
        gvkey = matched_row[3]

        line['GVKEY'] = gvkey
        line['tic'] = tic
        line['conm'] = conm
        line['orig_name'] = line['Company']
        line['match_score'] = match[1]
        line['sic'] = sic

        del line['Description of incident']
        del line['Information Source']
        del line['Source URL']

        writer.writerow(line)

    print('Found {} matches'.format(count))
