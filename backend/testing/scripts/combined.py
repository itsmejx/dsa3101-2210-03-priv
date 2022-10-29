import os
import sys

def get_output(testing):
	# remove background
	entrance_cam = '../camera/Entrance'
	if testing == 'True':
		cam1 = '../camera/Entrance/SiYing'
		cam2 = '../camera/Entrance/ShuWen'
		cam3 = '../camera/Entrance/WyeYan'
		os.system(f'python background_removal.py {cam1} ../camera/Entrance/bg_removed')
		os.system(f'python background_removal.py {cam2} ../camera/Entrance/bg_removed')
		os.system(f'python background_removal.py {cam3} ../camera/Entrance/bg_removed')
	else:
		os.system(f'python background_removal.py {entrance_cam} {entrance_cam}/bg_removed')

	camera_names = list(os.listdir('../camera'))
	camera_names.remove('Entrance')
	for cam in camera_names:
		tmp = f'../camera/{cam}'
		os.system(f'python background_removal.py {tmp} {tmp}/bg_removed')
  
if __name__ == '__main__':
    testing = sys.argv[1]
    get_output(testing)
