import csv

infile = 'data_breaches_w_company_details_nocrit_clean.csv'
outfile = 'data_breaches_w_company_details_nocrit_clean_parsed.csv'

search_terms = ['customer','employee','credit_card','cvv','social_security','name','address','personal_information']

with open(infile, 'r') as inf, open(outfile, 'w+') as outf:
    reader = csv.DictReader(inf)
    headers = [h for h in reader.fieldnames]
    headers = headers + search_terms
    writer = csv.DictWriter(outf, fieldnames=headers, quoting=csv.QUOTE_ALL)
    writer.writeheader()

    for row in reader:
        info = row['Description of incident'].lower()

        row['customer'] = 1 if 'customer' in info else 0
        row['employee'] = 1 if 'employee' in info and 'customer' not in info else 0
        row['credit_card'] = 1 if 'credit card' in info else 0
        row['cvv'] = 1 if 'cvv' in info else 0
        row['social_security'] = 1 if 'social security' in info or 'ssn' in info else 0
        row['name'] = 1 if 'name' in info else 0
        row['address'] = 1 if 'address' in info else 0
        row['personal_information'] = 1 if 'personal information' in info else 0

        writer.writerow(row)
