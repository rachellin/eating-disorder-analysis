import json
import csv 
import os
from pprint import pprint

from dates import month_days
# .split('T')[0].split('-',1)[1] <- date

# date range: 5/22 - 6/22 .... but that's only a month

# parse json
data = []
filename = "proanarexia.json"
with open(filename) as f:
    for line in f:
        data.append(json.loads(line))

print("parsed")
#pprint(data)

date_data_list = []

for post in data:
    date = post["created"].split('T')[0].split('-',1)[1]
    if not any(item["date"] == date for item in date_data_list):
        post_dict = {
            "date": date,
            "post_count": 1
        }
        date_data_list.append(post_dict)
    else:
        next(item for item in date_data_list if item["date"] == date)["post_count"] += 1

pprint(date_data_list)
print(len(date_data_list))

# WRITE TO CSV
with open('proed_reddit.csv', mode='w', newline='') as csv_file:
    fieldnames = ["date", "post_count"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for item in date_data_list:
        writer.writerow(item)

# create list of months
# month_list = []
# for range in date_ranges:
#     month = range["month"]
#     month_list.append(month)

# SORT BY MONTH
# month_data_list = []

# for month in month_list:
#     month_dict = {
#         "month": month,
#         "posts": []
#     }
#     month_data_list.append(month_dict)




