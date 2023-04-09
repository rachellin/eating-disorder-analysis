from TikTokApi import TikTokApi
from pprint import pprint
from datetime import datetime
import json

from tags import queries

# data to save: id, create_time, hashtags 

with TikTokApi() as api:
    # for trending_video in api.trending.videos(count=50):
    #     # Prints the author's username of the trending video.
    #     print("hi")
    #     print(trending_video.author)

    total = 0

    for q in queries:
        dict_list = []
        for video in api.hashtag(name=q).videos():
            #pprint(video.info_full()["itemInfo"]["itemStruct"]["author"])

            #pprint(video.create_time.strftime("%Y-%m-%d"))

            #pprint(video.id)
            total = total + video.stats["playCount"]

            # PUT HASHTAGS IN LIST 
            hashtag_objects = video.hashtags
            hashtag_list = []
            for tag in hashtag_objects:
                hashtag_list.append(tag.name)

            # # CREATE DICT
            video_dict = {
                "id": video.id,
                "create_time": video.create_time.strftime("%Y-%m-%d"),
                "hashtags": hashtag_list,
                "stats": video.stats
            }

            dict_list.append(video_dict)
            #pprint(video_dict)

        with open('data/tags/{tag}.json'.format(tag=q), 'w') as f:
            #f.write("{dict}\n".format(dict=video_dict))
            for vid in dict_list:
                json.dump(vid, f)
                f.write('\n')
        
        print("done with {q}".format(q=q))

            # CONVERTING DATETIME
            #pprint(datetime.date(video.create_time))
            # pprint(video.create_time.replace(hour=0, minute=0, second=0) + 
            #     datetime.timedelta(hours=1))

print("total: {total}".format(total=total))