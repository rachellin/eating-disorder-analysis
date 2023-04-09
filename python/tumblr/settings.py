import os
from os.path import join, dirname
from dotenv import load_dotenv

load_dotenv()

# instagram credentials
CLIENT_KEY= os.environ.get("CLIENT_KEY")
SECRET_KEY = os.environ.get("SECRET_KEY")