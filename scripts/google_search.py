import requests
import json
import pandas

key_location = '~/Documents/hons_thesis/google_api_key'
custom_search_id_location = '~/Documents/hons_thesis/custom_search_id'

api_key = open(key_location).read()
search_id = open(custom_search_id_location).read()
company = 'AAPL'

url = 'https://www.googleapis.com/customsearch/v1?key={}&cx={}&q={}&fields=items/title'.format(api_key, search_id, company)


resp = requests.get(url)
content = resp.json()
