# Input Args: string of file directory
# Output Args: array of predicted matching
import cv2 as cv
import numpy as np
from pathlib import Path
import datetime
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.preprocessing.image import array_to_img
import random

def template_matching(folder_dir, db):
    images = Path(folder_dir).glob('*.jpg')
    gen = ImageDataGenerator(rotation_range=60, horizontal_flip = True) #change accordingly
    identified = set()     # stores result from template matching
    
    #image augmentation
    for image in images:
        tmp = image.name[:-4].split("_")
        tmp_date = tmp[0]
        tmp_time = tmp[1]
        date = tmp_date.split(".")
        time = tmp_time.split(".")
        date_time = datetime.datetime(int(date[0]), int(date[1]), int(date[2]), int(time[0]), int(time[1]), int(time[2]))
        
        image = cv.imread(str(image), cv.IMREAD_COLOR)
        original_image = np.expand_dims(image,0) #expand dimensions for compatibility
        aug_iter = gen.flow(original_image, batch_size = 10) #to generate 10 augmented images from original image
        aug_images = [next(aug_iter)[0].astype(np.uint8) for i in range(10)]
        threshold = 0.70   # set threshold; high (ie. 1) means "more strict"
        
        tmp = {}   # stores detected ids for this shoe; if empty means no id is assigned to this shoe
        for to_find in aug_images:
            # perform match operation
            for item in db:
                res = cv.matchTemplate(item['image'], to_find, cv.TM_CCOEFF_NORMED)
                loc = np.where(res >= threshold)
                if (len(loc[0])!=0 and len(loc[1])!=0):    # if there's a match
                    if item['id'] in tmp:
                        tmp[item['id']] += 1
                    else:
                        tmp[item['id']] = 1
        
        # majority vote
        max_keys = [key for key, value in tmp.items() if value == max(tmp.values())]
        if max_keys == []:
            continue
        idx = 0
        if len(max_keys) > 1:    # more than 1 id with the max appearance, break tie randomly
            idx = random.randint(0, len(max_keys)-1)
        id_assigned = max_keys[idx]
        
        tpl = (date_time, id_assigned)
        identified.add(tpl)
    return identified
