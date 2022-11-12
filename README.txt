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

Steps to run dashboard:
1. On your system's application search function, search for "Git Bash" and run it.
2. For first time setup, run the following commands on Git Bash (the line between the ``` ```)
	a. ```cd ~/Desktop/```
	b. ```git clone git@github.com:itsmejx/dsa3101-2210-03-priv```
3. To start, click on "StartBoothy.cmd". (Run as administrator as needed)
4. Click on "start_here.html"
5. To stop, click on "StopBoothy.cmd". (Run as administrator as needed)


[If the above instructions do not work]
1. On your system's application search function, search for "Git Bash" and run it.
2. Search for and run the Docker Desktop application.
3. For first time setup, run the following commands on Git Bash (the line between the ``` ```)
	a. ```cd ~/Desktop/```
	b. ```git clone git@github.com:itsmejx/dsa3101-2210-03-priv```
4. To run the dashboard, open the folder that was created and run the following commands on Git Bash
	a. ```cd ~/Desktop/dsa3101-2210-03-priv```
	b. ```docker compose up```, and wait for it to load
	c. click on "start_here.html" or go to "http://localhost:3838"
5. To end the session, run the following commands on Git Bash
	a. press "Ctrl" + "c"
	b. ```docker compose down```
6. Close any relevant windows.

Refer to backend/camera_setup/README.txt for information to set up ESP32-CAM