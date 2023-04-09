import os
# from dates import date_ranges

import json
import csv 
from pprint import pprint

#os.system('snscrape --jsonl --max-results 2 reddit-subreddit "proanarexia" > testing.json')
os.system('snscrape --jsonl reddit-subreddit "proanarexia" > proanarexia.json')

filename = "proanarexia.json"

data = []
with open(filename) as f:
    for line in f:
        data.append(json.loads(line))

print(len(data))

for item in data:
    print(item["created"])

# 843, but all from 2022....