import os
import pandas as pd
from flask import Flask, request

app = Flask(__name__)

files = os.listdir('DB/')
database = {}
for f in files:
    temp = pd.read_json('DB/' + f)
    database[f[:-5]] = temp

file_dict = {}

@app.route("/get_date", methods = ["GET"])
def get_date():
    date = request.args.get('date')
    result = database[date]
    return result.to_json()

if __name__ == "__main__":
    app.run()
