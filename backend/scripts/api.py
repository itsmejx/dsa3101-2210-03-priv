import os
import pandas as pd
from flask import Flask, request
import json

app = Flask(__name__)

@app.route("/get_date", methods = ["GET"])
def get_date():
    date = request.args.get('date')
    json_file = f"{date}.json"
    filename = os.path.join('../DB', json_file)
    f = open(filename, "r")
    data = json.loads(f.read())
    return data

@app.route("/get_dates", methods = ["GET"])
def get_dates():
    print(request)
    new_data = request.get_json()
    dates = new_data.get('dates')
    data = []
    for date in dates:
        json_file = f"{date}.json"
        filename = os.path.join('../DB', json_file)
        f = open(filename, "r")
        data_tmp = json.loads(f.read())
        data.append(data_tmp)
    return data

if __name__ == "__main__":
    app.run()
