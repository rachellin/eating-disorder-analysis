import json
import csv 
import os
from pprint import pprint
from venv import create

from dates import date_ranges

# total playCount: 29432738

# make array of json file names 
dir = os.fsencode("data/tags")
json_files = []
for file in os.listdir(dir):
    filename = os.fsdecode(file)
    json_files.append(filename)

#print(json_files)

# parse json files line by line
all_data = []
for file in json_files:
    data = []
    filename = 'data/tags/{file}'.format(file = file)

    with open(filename) as f:
        for line in f:
            data.append(json.loads(line))

    all_data.append(data)
    print("parsed {file}".format(file = file))
    print("\n")

#pprint(all_data)

# create list of months
month_list = []
for range in date_ranges:
    month = range["month"]
    month_list.append(month)
#print(month_list)


# sorted by month data:
# [
#  { month: "2020-06",
#    videos: [{}, {}...]
#   }
#   ]

# SORT VIDEOS BY MONTH
month_data_list = []

for month in month_list:
    month_dict = {
        "month": month,
        "videos": []
    }
    month_data_list.append(month_dict)
#pprint(month_data_list)

for index, data in enumerate(all_data):
    # loop through each video for the hashtag
    for video in data:
        month = video["create_time"].rsplit('-', 1)[0]
        if month in month_list:
            next(item for item in month_data_list if item["month"] == month)["videos"].append(video)

#pprint(month_data_list)

# CALCULATE PLAYCOUNT PER MONTH
playcount_list = []
for month in month_data_list:
    month_playcount = 0
    video_count = len(month["videos"])
    for video in month["videos"]:
        month_playcount = month_playcount + video["stats"]["playCount"]
    month_dict = {
        "month": month["month"],
        "play_count": month_playcount,
        "video_count": video_count
    }
    playcount_list.append(month_dict)

#pprint(playcount_list)

# WRITE TO CSV
with open('proed_tiktok.csv', mode='w', newline='') as csv_file:
    fieldnames = ["month", "play_count", "video_count"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for month_data in playcount_list:
        writer.writerow(month_data)
            
