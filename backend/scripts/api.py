#import cv2 as cv
#import numpy as np
#from pathlib import Path
#import datetime
#import os
#import sys
from flask import Flask, request, jsonify, send_file, render_template
from werkzeug.utils import secure_filename
import requests


app = Flask(__name__)

@app.route("/home")
def home():
    name = request.args.get('name')
    return f"Hello from {name}"

@app.route("/upload", methods=["POST"])
def upload():
    print("Hello post")
    f = request.files['json_file'] #looks for an argument json_file
    f.save(f"{secure_filename(f.filename)}")
    return f"{f.filename} has been uploaded"

@app.route("/camera_data", methods = ["GET"])
def camera_data():
    filename = request.args.get('date')
    return "No"

if __name__ == '__main__':
    app.run()

