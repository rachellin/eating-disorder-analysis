import requests
from pprint import pprint

response = requests.get('https://api.tumblr.com/v2/tagged?tag=kaznej', 
    headers = {'Authorization': 'access_token 25ho2oc1wnon54BNgsCkuYHa5Moym5043je9TfckCynVLT81rG'})

pprint(response)