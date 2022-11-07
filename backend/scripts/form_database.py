import cv2 as cv
import numpy as np
from pathlib import Path
import datetime
import os
import sys

def form_db(folder_dir):
    db = []
    images = Path(folder_dir).glob('*.j*pg')
    count = 1 #number of visitors

    for image in images:
        tmp = image.name[:-4].split("_")
        tmp_date = tmp[0]
        tmp_time = tmp[1]
        date = tmp_date.split(".")
        time = tmp_time.split(".")
        img = cv.imread(str(image))
        date_time = datetime.datetime(int(date[0]), int(date[1]), int(date[2]), int(time[0]), int(time[1]), int(time[2]))
        db.append({'image':img, 'id':count, 'datetime': date_time})
        count += 1 #increment count for next shoe
        
    return db

