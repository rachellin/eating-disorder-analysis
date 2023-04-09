from TikTokApi import TikTokApi
from pprint import pprint

queries = [
    "thinrspo",
    "thrnspo",
    "iwillbeskinny",
    "ednotsheran", # this seems to mostly be about recovery tho?
    "thinpo",
    "caltok",
    "ëdtøk",
    "skinnyisbetter",
    "kittysspo",
    "edtt",
    "ugw",
    "somidiet",
    "iudiet"
]

with TikTokApi() as api:
    for video in api.hashtag(name='wooyoungdiet').videos():
    #for video in api.search.videos('cats'):
        #print(video.create_time)
        hashtag_objects = video.hashtags
        hashtag_list = []
        for tag in hashtag_objects:
            hashtag_list.append(tag.name)
        pprint(hashtag_list)
        print("\n")