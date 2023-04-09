import json
import csv 
import os
from pprint import pprint
from analyze import month_list, json_files, all_data

# csv columns: month, avg reply count, avg retweet count (for the month)

# calculate average reply count for each month
csv_data_list = []
for index, month_data in enumerate(all_data):
    reply_count_sum = 0
    retweet_count_sum = 0
    tweets_reply = 0
    tweets_retweet = 0
    # loop through every tweet 
    for tweet in month_data:
        if tweet["replyCount"] > 20:
            reply_count_sum = reply_count_sum + tweet["replyCount"]
            tweets_reply = tweets_reply + 1
        if tweet["retweetCount"] > 20:
            retweet_count_sum = retweet_count_sum + tweet["retweetCount"]
            tweets_retweet = tweets_retweet + 1
    # calculate average counts
    if tweets_reply > 0:
        reply_count_avg = reply_count_sum/tweets_reply
    else:
        reply_count_avg = 0
    if tweets_retweet > 0:
        retweet_count_avg = retweet_count_sum/tweets_retweet
    else:
        retweet_count_avg = 0
    # create dict
    month_avg_counts = {
        "month": month_list[index],
        "reply_count_avg": reply_count_avg,
        "retweet_count_avg": retweet_count_avg
    }
    # add to list 
    csv_data_list.append(month_avg_counts)


# write to csv
with open('avg_replies_rewteets.csv', mode='w') as csv_file:
    fieldnames = ["month", "reply_count_avg", "retweet_count_avg"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for month_data in csv_data_list:
        writer.writerow(month_data)