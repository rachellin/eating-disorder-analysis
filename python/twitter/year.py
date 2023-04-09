import json
import csv 
from pprint import pprint
from analyze import month_list, json_files, all_data

# find average # of tweets per year 
csv_data_list = []
for year in range(7, 20):
    if year < 10:
        year = "0{x}".format(x = year) # add 0 before digit 
    tweet_count_sum = 0
    for index, month_data in enumerate(all_data):
        if month_list[index].split('-')[0] == "20" + str(year):
            tweet_count_sum = tweet_count_sum + len(month_data)
    tweet_count_avg = tweet_count_sum/12
    # create dict
    year_dict = {
        "year": "20" + str(year),
        "tweet_count": tweet_count_avg
    }
    # add to list 
    csv_data_list.append(year_dict)

# find average prevalence per year 
# with open('prevalence.csv') as csv_file:
#     csv_reader = csv.reader(csv_file, delimiter=',')
#     line_count = 0
#     for row in csv_reader:
#         if line_count == 0:
#             print(f'Column names are {", ".join(row)}')
#             line_count += 1
#         else:
#             #print(f'\tregion: {row[0]}, year: {row[1]}, age_range: {row[2]}, prev: {row[3]}')
#             line_count += 1
#         if line_count > 500:
#             break
#     #print(f'Processed {line_count} lines.')

prev_data = []
with open('prevalence.csv', mode='r') as csv_file:
    csv_reader = csv.DictReader(csv_file)
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            #print(f'Column names are {", ".join(row)}')
            line_count += 1
        
        if row["year"] >= "2007":
            prev_dict = {
                "year": row["year"],
                "prevalence": row["prevalence"]
            }
            prev_data.append(prev_dict)

        line_count += 1
    
#pprint(prev_data)
    #print(f'Processed {line_count} lines.')

# find average prevalence per year
index = 0
for curr_year in range(7, 20):
    if curr_year < 10:
        curr_year = "0{x}".format(x = curr_year) # add 0 before digit 
    prev_sum = 0
    rows_in_year = 0
    for row in prev_data:
        if row["year"] == ("20" + str(curr_year)):
            prev_sum = prev_sum + float(row["prevalence"])
            rows_in_year = rows_in_year + 1
        else:
            print(curr_year)
    prev_avg = prev_sum/rows_in_year
    # add to dict
    csv_data_list[index]["prevalence"] = prev_avg
    index = index + 1

pprint(csv_data_list)

# write to csv
with open('prev_vs_tweetcount.csv', mode='w') as csv_file:
    fieldnames = ["year", "tweet_count", "prevalence"]
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    for year_data in csv_data_list:
        writer.writerow(year_data)