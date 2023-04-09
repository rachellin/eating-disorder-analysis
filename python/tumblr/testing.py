import pytumblr
from pprint import pprint
from datetime import datetime

# Authenticate via OAuth
client = pytumblr.TumblrRestClient(
  'qaJloT1NfVGVgkZ0WZQYZPhyyoRe9Wbe51HQZjsEoxK8kgvk7o',
  'uskFKsSX9Nh0G41OtTa9mcUHC1BoCEjJxwnhrIEpmYam9i5Wku',
  '25ho2oc1wnon54BNgsCkuYHa5Moym5043je9TfckCynVLT81rG',
  'rv3gHp6atmxzBXUMrQ6L7OhfLWlJSTqaRnYNt4KsFmFvrFYhEy'
)

now = int(datetime.now().timestamp())
#print(now)
posts = client.tagged("userkaznej", limit=1, before=now)
pprint(posts[0])

# Make the request
#client.info()

# def date_to_integer(raw):
#     raw = raw.split('-')
#     raw = [raw[0], raw[1], raw[2].split(" ")[0]]
#     raw = [int(x) for x in raw]
#     x = datetime(raw[0], raw[1], raw[2])
#     timestamp=int(x.timestamp())
#     return timestamp

# date = datetime.now().strftime("%Y-%m-%d")
# print(date)
# count = 0
# post_list = []
# for i in range(3):
#     timestamp = date_to_integer(date)
#     posts = client.tagged("thinspo", before=timestamp)
#     count = count + len(posts)
#     # update date: find oldest post (smallest timestamp)
#     oldest = posts[0]
#     for j in range(1, len(posts)):
#         oldest_date = date_to_integer(oldest["date"])
#         curr_date = date_to_integer(posts[j]["date"])
#         if min(oldest_date, curr_date) < oldest_date:
#             oldest = posts[j]
#     date = oldest["date"]

#     # add posts to list
#     for item in posts:
#         post_list.append(item)

# print(count) 

# i = 0
# for item in post_list:
#     print(item["date"])
#     i = i + 1
#     if i % 20 == 0:
#         print()

# posts = client.tagged("cats", before=1591592400)
# pprint(posts[0]["date"])
#pprint(posts)
#print(len(posts))
#'2021-06-01 23:40:51 GMT'
#pprint(posts[0]["date"])

# convert date to integer timestamp
# raw = '2020-06-08 23:40:51 GMT'
# raw = raw.split('-')
# raw = [raw[0], raw[1], raw[2].split(" ")[0]]
# raw = [int(x) for x in raw]
# x = datetime(raw[0], raw[1], raw[2])
# timestamp=int(x.timestamp())