import requests
import csv
import json
import time

f = './Privacy_Rights_Clearinghouse-Data-Breaches-Export_2018.csv'
out = './data-breaches-with-tickers.csv'

with open(f, 'r') as infile, open(out, 'w+') as outfile:
    data = csv.DictReader(infile)
    headers = data.fieldnames
    headers = headers + ['symbol', 'matched_name']
    print(headers)
    writer = csv.DictWriter(outfile, fieldnames=headers, quoting=csv.QUOTE_ALL)
    writer.writeheader()

    found = 0

    for line in data:
        company = line['Company'].strip()
        url = 'http://d.yimg.com/aq/autoc?query={}&region=US&lang=en-US'.format(company)
        resp = requests.get(url)
        try:
            content = resp.json()['ResultSet']['Result']
        except Exception as e:
            print('Problem while searching for {}, skipping...'.format(company))
            continue

        try:
            symbol = content[0]['symbol']
            name = content[0]['name']
            found = found + 1
            print('Company found: {}'.format(company))
        except IndexError:
            print('Company not found: {}'.format(company))
            continue

        line['symbol'] = symbol
        line['matched_name'] = name
        writer.writerow(line)

        time.sleep(0.5)

    print('Matched {} names'.format(found))
