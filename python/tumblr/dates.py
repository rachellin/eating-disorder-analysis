# advanced search template:
# https://twitter.com/search?q=thinspo%20until%3A2015-01-31%20since%3A2015-01-01&src=typed_query
# https://twitter.com/search?q=thinspo%20until%3AYYYY-MM-DD%20since%3AYYYY-MM-DD&src=typed_query
from pprint import pprint

month_days = {
    "01": "31",
    "02": "28",
    "03": "31",
    "04": "30",
    "05": "31",
    "06": "30",
    "07": "31",
    "08": "31",
    "09": "30",
    "10": "31",
    "11": "30",
    "12": "31"
}

date_ranges = []

for year in range(17, 22):
    if year < 10:
        year_str = "0{x}".format(x = year) # add 0 before digit 
    else:
        year_str = str(year)
    for month in range(1, 13): 
        if month < 10:
            month = "0{x}".format(x = month) # add 0 before digit 
        month_date = "20{YY}-{MM}".format(
            YY = year_str,
            MM = month
        )
        since = "{month_date}-01".format(
            month_date = month_date
        )
        if month == "02" and (year == 16 or year == 20 or year == 8 or year == 12):
            # leap year
            until = "{month_date}-29".format(
                month_date = month_date
            )
        else:
            until = "{month_date}-{DD}".format(
                month_date = month_date,
                DD = month_days[str(month)]
            )
        date_range_obj = {
            "month": month_date,
            "since": since,
            "until": until
        }
        date_ranges.append(date_range_obj)
        
#pprint(date_ranges)
# print(len(date_ranges))

# create dict for all the date ranges??
# date_ranges = [
#     {
#         "month": "2015-01",
#         "since": "2015-01-01",
#         "until": "2015-01-31"
#     },
#     {
#         "month": "2015-02",
#         "since": "2015-02-01",
#         "until": "2015-02-28"
#     },
#     {
#         "month": "2015-03",
#         "since": "2015-03-01",
#         "until": "2015-03-31"
#     },
#     {
#         "month": "2015-04",
#         "since": "2015-04-01",
#         "until": "2015-04-30"
#     },
#     {
#         "month": "2015-05",
#         "since": "2015-05-01",
#         "until": "2015-05-31"
#     },
#     {
#         "month": "2015-06",
#         "since": "2015-06-01",
#         "until": "2015-03-30"
#     },
# ]