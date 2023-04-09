import os
# from dates import date_ranges

import json
import csv 
from pprint import pprint

from tags import queries 

#os.system('snscrape --jsonl --max-results 2 instagram-hashtag "thinspam"> testing.json')

for q in queries:
    os.system('snscrape --jsonl instagram-hashtag\
         "{query}" > data/{query}.json'.format(
             query = q
         ))
    print("done with {query}".format(query = q))

# filename = "testing.json"

# data = []
# with open(filename) as f:
#     for line in f:
#         data.append(json.loads(line))

# pprint(data)

