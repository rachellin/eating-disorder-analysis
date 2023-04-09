import os
from dates import date_ranges

#os.system('snscrape --jsonl --max-results 20 --since 2020-06-01 twitter-search "its the elephant until:2020-07-31" > data/text-query-tweets.json')

for period in date_ranges:
    os.system('snscrape --jsonl --since {since} twitter-search\
         "thinspo until:{until}" > tweet_json/{month}.json'.format(
             since = period["since"],
             until = period["until"],
             month = period["month"]
         ))
    print("done with {month}".format(
        month = period["month"]
    ))
    
# started 5:14 pm
# ended 7:09 pm

# 6/1/22
# started 8:26 pm
# ended ~ 10:21 pm
# use twitter popularity per year to adjust number of tweets 
# ^ using percentage of all tweets would work except there's no data out there on that??