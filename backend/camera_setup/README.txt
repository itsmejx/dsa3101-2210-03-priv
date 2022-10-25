Instructions for setup of camera:
1. Download XAMPP from https://www.apachefriends.org/download.html
2. Download Arduino IDE from https://www.arduino.cc/en/software
3. Open XAMPP and start the Apache Server
4. In the sendimagetolocalserver.ino folder, open the sendimagetolocalserver.ino.io sketch in the Arduino IDE
5. Change the SSID as well as the SSID's password
6. Change serverName to the IP address of the server receiving the photos 
7. Update serverPath with the directory of the upload.php file
8. Open the upload.php file and change $target_dir field to the directory where the photos should be uploaded to
9. In the Arduino IDE upload the sketch to the camera. Camera can now be started to take pictures which will be uploaded to the server
10. Steps above can be repeated for different cameras. Directories can be changed accordingly so that pictures from different cameras can be uploaded into different folders. 