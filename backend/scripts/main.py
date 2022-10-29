import os
import sys
import pandas as pd
from form_database import *
from template_matching import *
 
def get_output(testing):
    # remove background
    entrance_cam = '../camera/Entrance'
    os.system(f'python3 background_removal.py {entrance_cam} {entrance_cam}/bg_removed')
 
    camera_names = list(os.listdir('../camera'))
    camera_names.remove('Entrance')
    for cam in camera_names:
        tmp = f'../camera/{cam}'
        os.system(f'python3 background_removal.py {tmp} {tmp}/bg_removed')
 
 
    # detect and crop shoes
    for cam in camera_names:
        tmp = f'../camera/{cam}/bg_removed'
        os.system(f'python3 detect_and_crop.py {tmp} cropped_shoes')
 
    # form database from entrance camera
    db = form_db('../camera/Entrance/bg_removed')
 
    # image augmentation and template matching
    rows_lst = []
    this_date = ''
    for cam in camera_names:
        cam_num = cam[3:]
        tmp = f'../camera/{cam}/bg_removed/cropped_shoes'
        identified = template_matching(tmp, db)
        for datetime, id in identified:
            this_date = str(datetime.date())
            rows_lst.append({'id' : id, 'camera' : cam_num, 'datetime' : str(datetime)})
    
    df = pd.DataFrame(rows_lst)
    
    # an id cannot appear at multiple cameras at the same timestamp, so we remove duplicates and choose to keep the first occurrence
    df = df.drop_duplicates(subset=['id', 'datetime'], ignore_index=True, keep='first')
    result = df.to_json(orient='records')
    with open(f'{this_date}.txt', 'w') as f:
        f.write(result)

if __name__ == '__main__':
    testing = sys.argv[1]
    get_output(testing)
