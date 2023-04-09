import json
import csv 
import os
from pprint import pprint

#filename = "text-query-tweets.json"
filename = "tweet_json/2020-08.json"

data = []
with open(filename) as f:
        for line in f:
            data.append(json.loads(line))

#print(data[0]["quotedTweet"])

# print(len(data))

count = 0
# 141, 201 has quotedTweet

#pprint(data[201]["quotedTweet"])
#pprint(data[0].keys())

# sum = 0

# for tweet in data:
#     if tweet["replyCount"] > 5:
#         #print(tweet["retweetCount"])
#         sum = sum + tweet["replyCount"]
#         print(sum)
#         #print(data.index(tweet))
#         count = count + 1

# avg = sum/count
# print("avg: {avg}".format(avg=avg))

# print("tweets with < 20: {count}".format(count=count))
# print("total tweets: {total}".format(total=len(data)))

# print("\n")

for tweet in data:
    if not tweet["place"] == None:
        #pprint(tweet["quotedTweet"])
        #print(data.index(tweet))
        count = count + 1
print(count)
print(len(data))

# print(count)

