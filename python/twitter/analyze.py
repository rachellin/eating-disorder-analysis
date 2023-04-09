import json
import csv 
import os
from pprint import pprint

# 1. make array of json objects for each json file 
# 2. make array of that array ^ (?) and call it 'all_data'
# 3. count # of tweets ( = # of json objects) per month
# 4. put # of tweets per month in excel
# excel columns: month, # of tweets 

# TO WRITE TO CSV:
# list of dictionaries
# dict: month, tweet_count

# You have a JSON Lines format text file. You need to parse your file line by line:

# make array of json file names 
dir = os.fsencode("tweet_json")
json_files = []
for file in os.listdir(dir):
    filename = os.fsdecode(file)
    json_files.append(filename)

# parse json files line by line
all_data = []
for file in json_files:
    data = []
    filename = 'tweet_json/{file}'.format(file = file)

    with open(filename) as f:
        for line in f:
            data.append(json.loads(line))

    all_data.append(data)
    print("parsed {file}".format(file = file))

#print(all_data)
# TODO: would be smart to put this output in a file so i dont have to parse every time?

# create list of months
month_list = []
for file in json_files:
    month = os.path.splitext(file)[0]
    month_list.append(month)

# count num of tweets per month
# num of tweets = length of each 'data' obj all_data
csv_data_list = []
for index, data in enumerate(all_data):
    tweet_count = len(data)
    csv_data = {
        "month": month_list[index],
        "tweet_count": tweet_count
    }
    csv_data_list.append(csv_data)

pprint(csv_data_list)
print(len(csv_data_list))

# write to csv
with open('thinspo_tweets.csv', mode='w', newline='') as csv_file:
    fieldnames = ["month", "tweet_count"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for month_data in csv_data_list:
        writer.writerow(month_data)

#pprint(data[0])
# for tweet in data:
#     if not tweet["coordinates"] == None:
#         print(tweet["coordinates"])

# for tweet in data:
#     if not tweet["place"] == None:
#         print(tweet["place"])

# TODO: check what the media attribute looks like (array or ??)
