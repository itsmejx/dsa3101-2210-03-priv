import os
import pandas as pd
from flask import Flask, request
import json

app = Flask(__name__)

@app.route("/get_date", methods = ["GET"])
def get_date():
    date = request.args.get('date')
    json_file = f"{date}.json"
    filename = os.path.join('DB', json_file)
    f = open(filename, "r")
    data = json.loads(f.read())
    return data

if __name__ == "__main__":
    app.run()
