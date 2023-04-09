import pytumblr
from pprint import pprint
from datetime import datetime
import csv
import time 
import json

# scraping started at 11:14
# end at 11:19
# up to 2021-12-02 01:18:19 GMT
# 
# # startubg at 11:42 am 

from dates import date_ranges

# create list of months
month_list = []
for item in date_ranges:
    month_list.append(item["month"])

#print(month_list)

month_data_list = []

for month in month_list:
    month_dict = {
        "month": month,
        "posts": []
    }
    month_data_list.append(month_dict)

# Authenticate via OAuth
client = pytumblr.TumblrRestClient(
  'l5d2S5QAkmQHnbx3TCqXMPF84LpAaByMv4DEZxUQqSmaJbm3p8',
  '89v9JgwhCjh1FuVvQhk6BoHOnQFawjnM36NFeh5GjcogdIOSm8',
  '7RBOCwctREtqB3c2JrLG8hwGmONIloVf9UbsQVFKGcgTdxrsvD',
  'TSvcJqCSi609HHr8yhZMR6SBrpOCVxjwfaTqiGsrDZfKu0cOcP'
)

# Make the request
#client.info()

def date_to_integer(raw):
    raw = raw.split('-')
    raw = [raw[0], raw[1], raw[2].split(" ")[0]]
    raw = [int(x) for x in raw]
    x = datetime(raw[0], raw[1], raw[2])
    timestamp=int(x.timestamp())
    return timestamp

#date = datetime.now().strftime("%Y-%m-%d")
date = "2018-01-01"
#print(date)
count = 0
post_list = []

# when there's no posts:
# {'meta': {'status': 429, 'msg': 'Limit Exceeded'}, 'response': [], 'errors': [{'title': 'Limit Exceeded', 'code': 0, 'detail': 'Got an error. Try again.'}]}

# 19960 posts by 

loop = True
while loop == True:

    timestamp = date_to_integer(date)
    posts = client.tagged("thinspo", before=timestamp)

    if not type(posts) == list:
        if posts["meta"]["status"] == 429:
            print("\nsleeping for 1 min\n")
            time.sleep(60) # rest one minute 
            continue
        else:
            print(posts)

    # no more posts
    if len(posts) == 0:
        print("NORE MORE POSTS")
        break

    #count = count + len(posts)
    # update date: find oldest post (smallest timestamp)

    oldest = posts[0]
    curr_date = date_to_integer(oldest["date"])

    post_dict = {
        "id": oldest["id"],
        "date": oldest["date"].split(" ")[0],
        "note_count": oldest["note_count"],
        "tags": oldest["tags"]
    }

    with open('data/{month}.json'.format(month=oldest["date"].rsplit('-', 1)[0]), 'a') as f:
        json.dump(post_dict, f)
        f.write("\n")

    #list(filter(lambda item: item['month'] == month, month_data_list))[0]["posts"].append(post_dict)
    count = count + 1
    
    for j in range(1, len(posts)):
        oldest_date = date_to_integer(oldest["date"])
        curr_date = date_to_integer(posts[j]["date"])

        # if curr_date > 1640951999: # after 12/31/2021
        #     posts_ignored = posts_ignored + 1
        #     print("{x} posts ignored so far".format(x = posts_ignored))
        #     print(posts[j]["date"])
        #     continue

        if curr_date < 1483228800: # before 01/01/2017
            loop = False
            break

        # add post to correct month
        month = posts[j]["date"].rsplit('-', 1)[0]
        #print(month)
        post_dict = {
            "id": posts[j]["id"],
            "date": posts[j]["date"].split(" ")[0],
            "note_count": posts[j]["note_count"],
            "tags": posts[j]["tags"]
        }

        #[item for item in month_data_list if item["month"] == month]["posts"].append(post_dict)
        #list(filter(lambda item: item['month'] == month, month_data_list))[0]["posts"].append(post_dict)
        with open('data/{month}.json'.format(month=posts[j]["date"].rsplit('-', 1)[0]), 'a') as f:
            json.dump(post_dict, f)
            f.write("\n")

        if min(oldest_date, curr_date) < oldest_date:
            oldest = posts[j]
        count = count + 1
        print(posts[j]["date"])
    date = oldest["date"]

    # add posts to list
    # for item in posts:
    #     post_list.append(item)


    print("{num} posts so far".format(num=count))

print(count) 

# POSTS PER MONTH
csv_dict_list = []
for month in month_data_list:
    month_postcount = len(month)
    month_dict = {
        "month": month["month"],
        "post_count": month_postcount
    }
    csv_dict_list.append(month_dict)

# write to csv
with open('proed_tumblr.csv', mode='w', newline='') as csv_file:
    fieldnames = ["month", "post_count"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for month_data in csv_dict_list:
        writer.writerow(month_data)