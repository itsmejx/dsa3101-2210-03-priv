# Input Args: path to camera folder, name of output folder to save the background removed pictures 
# Output Args: NA
import cv2 as cv
import numpy as np
from pathlib import Path
import sys
import os
from tensorflow.keras.preprocessing.image import array_to_img

def background_remove(background_pic, directory, save):
    images = Path(directory).glob('*.jpg')
    if not os.path.exists(save):
        os.makedirs(save)
    for image in images:
        name = image.name[-33:-4]
        img = cv.imread(str(image))
        img_nobg = cv.subtract(img, background_pic)
        tmp = array_to_img(img_nobg)
        tmp.save(f"{save}/{name}.jpg")


if __name__ == "__main__":
    folder_dir = sys.argv[1]
    background = cv.imread(f"{folder_dir}/background.jpg")
    pictures_dir = f"{folder_dir}/uploads"
    save_folder = sys.argv[2]
    background_remove(background, pictures_dir, save_folder)
