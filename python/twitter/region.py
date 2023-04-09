
# 1. combine all tweet json objects into one array?? do i still want month data?
# 2. loop through json objects and get location - tweet["place"]["country"]

# dict with region, month, tweet_count, or:
# {
#   "region1": [{
#           "month": month,
#           "tweet_count": tweet_count
#       }, ...],
#    ...
# }

# 3. each time there is location, add to list of locations

# csv columns: region, tweet_count, prevalence 
# i can't just remove month data because the prevalence data is by year 
# i will calculate the average prevalence per year for each region 
# for tweets: average # of tweets per month for each region ? per year?
# the average # of tweets is going to be really low so 
# i guess it doesn't really matter if the precentage of place each month has is ~ same 
# 

