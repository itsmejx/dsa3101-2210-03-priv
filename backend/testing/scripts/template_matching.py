# Input Args: string of file directory
# Output Args: array of predicted matching
import cv2 as cv
import numpy as np
from pathlib import Path
import sys

def template_matching(folder_dir, db):
    images = Path(folder_dir).glob('*.jpg')
    identified = []    # stores result from template matching

    for image in images:
        template = cv.imread(str(image), cv.IMREAD_COLOR)
        threshold = 0.70   # set threshold; high (ie. 1) means "more strict"
        tmp = []    # stores detected ids for this shoe; if empty means no id is assigned to this shoe
        
        # perform match operation
        for item in db:
            res = cv.matchTemplate(item['image'], template, cv.TM_CCOEFF_NORMED)
            loc = np.where(res >= threshold)
            if (len(loc[0])!=0 and len(loc[1])!=0):    # if there's a match
                tmp.append(item['id'])
                
        identified.append(tmp)
        
    return(identified)

if __name__ == "__main__":
    folder_dir = sys.argv[1]
    db = sys.agrv[2]
    template_matching(folder_dir, db)
