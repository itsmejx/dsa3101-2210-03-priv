# dsa3101-2210-03-priv
DSA3101 Group Project: Privacy-aware movement tracking


Needed folders/files (included):

- backend
- frontend
- docker-compose.yml
- README.txt
- start_here.html
- StartBoothy.cmd
- StopBoothy.cmd

Needed extra applications:
- Docker Desktop
	https://www.docker.com/products/docker-desktop/
- Git02.37.1-64 bit
	https://www.git-scm.com/downloads
- XAMPP
	https://www.apachefriends.org/download.html 
- Arduino IDE
	https://www.arduino.cc/en/software 

Once you have installed the necessary applications!

Instructions for setup of camera: (All folders/files references are in backend/camera_setup)
1. Open XAMPP and start the Apache Server
2. In the sendimagetolocalserver.ino folder, open the sendimagetolocalserver.ino.ino sketch in the Arduino IDE
3. Change the SSID as well as the SSID's password
4. Change serverName to the IP address of the server receiving the photos 
5. Update serverPath with the directory of the upload.php file
6. Open the upload.php file and change $target_dir field to the directory where the photos should be uploaded to
7. In the Arduino IDE upload the sketch to the camera. The camera can now be started to take pictures which will be uploaded to the server
8. Steps above can be repeated for different cameras. Directories can be changed accordingly so that pictures from different cameras can be uploaded into different folders.

Steps to run dashboard:
1. On your system's application search function, search for "Git Bash" and run it.
2. For first time setup, run the following commands on Git Bash (the line between the ``` ```)
	a. ```cd ~/Desktop/```
	b. ```git clone git@github.com:itsmejx/dsa3101-2210-03-priv```
3. To start, click on "StartBoothy.cmd". (Run as administrator as needed)
4. Once it finishes loading, click on "StartBoothy2.cmd". (Run as administrator as needed)
5. Click on "start_here.html"
6. To stop, click on "StopBoothy.cmd". (Run as administrator as needed)


[If the above instructions do not work]
1. On your system's application search function, search for "Git Bash" and run it.
2. Search for and run the Docker Desktop application.
3. For first time setup, run the following commands on Git Bash (the line between the ``` ```)
	a. ```cd ~/Desktop/```
	b. ```git clone git@github.com:itsmejx/dsa3101-2210-03-priv```
4. To run the dashboard, open the folder that was created and run the following commands on Git Bash
	a. ```cd ~/Desktop/dsa3101-2210-03-priv```
	b. ```docker compose up```, and wait for it to load
5. Open another Git Bash
	a. ```cd ~/Desktop/dsa3101-2210-03-priv```
	b. ```docker exec dsa3101-2210-03-priv-flask-1 python3 -u -m flask run --host=0.0.0.0```
	c. click on "start_here.html" or go to "http://localhost:3838"
6. To end the session, run the following commands on Git Bash
	a. press "Ctrl" + "c"
	b. ```docker compose down```
7. Close any relevant windows.

Note: Under backend/camera there are sample photos that can be used
