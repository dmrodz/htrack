# I. Tablet setup for HTrack in Android devices

## Required apps from Google Play Store  
Currently, the following apps are required to be able to run HTrack on an Android device. Go to the Google Play store and download them.  
+ [UserLand](https://play.google.com/store/apps/details?id=tech.ula&hl=en_US&gl=US) (required)  
+ [DroidScript](https://play.google.com/store/apps/details?id=com.smartphoneremote.androidscriptfree&hl=en_US&gl=US) (required)  
+ [GPS Connected](https://play.google.com/store/apps/details?id=org.bruxo.gpsconnected&hl=en_US&gl=US) (required)  
+ [Hackers Keyboard](https://play.google.com/store/apps/details?id=org.pocketworkstation.pckeyboard&hl=en_US&gl=US) (optional, but recommended)  
  + If downloading this app, make sure it is setup correctly before beginning the setup.

## A. UserLand setup for HTrack  
1. Once UserLand is installed, press the app icon to open it. Several automatic processes will run.Wait for these to be completed before proceeding to step 2.
2. There are three tabs at the bottom. On the app tab, scroll down and select R. Several automatic processes will run; wait for these to be completed before proceeding.  
3. Upon completion, the system will prompt you for a username and password; create them. Once the username and password are created, the system will setup all linux packages needed for R. Wait for these to be completed before proceeding.  
4. After all installations are done. UserLand will ask you to log in using the username and password you just created. If it closes, re-open the app, go back to the main page and scroll down; select the *Session* tab and click on R.
5. You are now in Linux. Perform the following installations:    
  ```  
  sudo apt-get update  
  sudo apt-get upgrade  
  sudo apt-get update --fix-missing  
  sudo apt-get install apt-utils debconf   
  sudo apt-get install nano dialog whiptail python curl links2   
  sudo apt-get install libcurl4-gnutls-dev libgdal20 libgdal-dev libssl-dev libproj-dev  
  sudo apt-get install libgeos-dev libgeos++-dev r-cran-processx r-cran-pkgbuild r-cran-pkgload   
  sudo apt-get install r-cran-httr r-cran-callr r-cran-devtools r-cran-xopen   
  sudo apt-get install r-cran-rcpp r-cran-reshape2 r-cran-rcurl r-cran-roxygen2  
  sudo apt-get install r-cran-httpuv  
  sudo apt-get update --fix-missing  
  ```  
6. These installs will take ~2 hours and will prompt the user whether to install several dependencies.
7. Restart UserLand (re-enter your login and password) and proceed to the next section when ready.  

## B. Create HTrack folder structure  
1. The folder structure for HTrack must be created by entering the following commands in UserLand:  
```
cd ..
cd ..
cd storage/internal  
mkdir htrack
cd htrack
mkdir htrack_archive
mkdir mapTiles
cd mapTiles
mkdir ALL
cd ALL
mkdir OSM
cd ..
cd ..
cd ..
cd ..
cd ..
```  
2. STAY in this directory and proceed to next section.  

## C. Create one-word command and required shells for HTrack  
1. Create a one-word command for HTrack to launch. We are using the word "htrack" as the one-word command.
2. In UserLand, use the following code:  
```  
cd etc
nano bash.bashrc
```
<go to the end of the file using arrow keys – DO NOT DELETE OR EDIT ANYTHING!!>
```
function htrack {
         bash /storage/internal/htrack.sh “$@”
         bash /storage/internal/start_serv.sh “$@”
}
```
3. Press Ctrl X, type: y, press Enter.
4. **Restart UserLand (re-enter your login and password)**.
5. Load R by typing ```R``` within UserLand. This will start R within UserLand's linux distribution.  
6. Install libraries needed for HTrack by entering the following commands, then quit R:  
```  
install.packages(“http://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.1.0.tar.gz”, repos = NULL, type = “source”)
```
<wait for this installation to complete, then install the following:>  
```
install.packages(c(“shinydashboard”, “crosstalk”, “leaflet”, “leaflet.extras”, “DT”, “magrittr”, “rgdal”, “sp”, "dplyr", "rgeos"))
q()
```
6.	Two shell scripts need to be created. Still in UserLand, enter the following commands:  
```  
cd ..
cd ..
cd storage/internal
nano start_serv.sh
```  
<within the editor, enter>
```
#!/bin/bash
cd /storage/internal/htrack/mapTiles
python –m SimpleHTTPServer 8000
```
7.	press Ctrl X, type: y, press Enter
8.	For the second shell script, type the following:
```
nano htrack.sh
```
<within the editor, type the following>  
```
#!/bin/bash
R –e “shiny::runApp(‘/storage/internal/htrack/app.R’, port = 7394)”
```
9.	press Ctrl X, type: y, press Enter
10. Quit UserLand. There should not be any sessions open when quitting UserLand; check by looking into the notifications area of the tablet.

## D. Droidscript setup for HTrack  
1. After installing Droidscript, create a new application called “htrack”, edit the standard provided script with these lines:  
```  
//Called when application begins
function OnStart()
{
    //Create layout with greeting/message centered
    lay = app.CreateLayout(“linear”, “VCenter,FillXY”);

    //Create text label and add to layout
    txt = app.CreateText(“Launching HTrack interface”);
    txt.SetTextSize(32);
    lay.AddChild(txt);
    app.OpenUrl(“http://127.0.0.1:7394”);

    //Add layout to app
    app.AddLayout(lay);
}  
```
2. Add this mini app to the home screen:
    + Press the back button until you are in the main DroidScript page.  
    + Long-press the htrack application you just created.  
    + Select Add Shortcut to Home Screen.

## E. Other dependencies  
HTrack works on the locations and areas of interest of the user.  
+ A preload file (.csv) with this information is needed. The preload file should contain the geolocations of the areas of interest and the following variables:  
    + CLUSTER (initials for the areas of interest)  
    + HHID (a label for the structures of interest)  
    + LAT (latitude of structure of interest)  
    + LONG (longitude of structure of interest)  
**We provide a sample preload file to be able to set up and test HTrack. The user will need to customize the app and preload file as needed.**  
+ When using the application offline, maptiles will need to be generated. We recommend the use of Open Street Map maptiles, which can be obtained through the ```RgoogleMaps``` R package. These are licensed under the Open Data Commons Open Database License (ODbL) by the Open Street Map Foundation [OSMF](https://www.openstreetmap.org/copyright). 
    + The following code was used to generate map tiles for HTrack:  
    ```  
    library(RgoogleMaps)
    latidude = <enter latitude>
    longitude = <enter longitude>
    for (zoom in 15:20)  
    GetMapTiles(c(latitude, longitude), 
                zoom = zoom, nTiles = round(c(18, 18) / (21 - zoom)),
                tileDir = "your/path/")
    ```  
    + These maptiles are later saved in the device under the following device folder:  
    ```Android > data > tech.ula > files > storage > htrack > mapTiles > ALL > OSM``` 


# II. Starting HTrack  
1. The source code of the app, ```app.R```, and .csv preload need to be imported into the main ```htrack``` folder within the tablet (see folder structure below).  
    + Folder location: ```Android > data > tech.ula > files > storage > htrack```    
3. To start HTrack, open UserLand and log in.
4. Type the one-word command: htrack  
    + Several messages will appear while HTrack starts. The very last line will say ```Listening on http://127.0.0.1:7394...```. Once that line appears, proceed to next step.
5. Go to the tablet’s home screen
6. Press the HTrack icon that you created with DroidScript.  
    + HTrack will load in the default internet browser that is set up in the tablet. We recommend that Chrome is set as the default internet browser for the tablet.  
    

# III. Saving data  
1. To save data, a house marker must be activated in the navigation map.  
    + House markers are activated by pressing on the respective dot on the map.  
    + Once pressed, a small label indicated the structure ID will appear; this indicates that the marker is activated.  
    + The user can also select the structure ID using the data table below the navigation map.  
        + Selecting a home through the table rows will isolate the map house marker. THe user can then press that house marker to activate it.  
2. Once a house marker is activated, the user can enter data in the required fields.  

    
# IV. Troubleshooting HTrack  
1. If issues are encountered, please open an issue with the following details.  
    + Specific error message shown in the HTRack screen  
    + Specific error message stated within UserLand  
    + A summary with details explaining what was being done right before the error appeared  
    
# V. General folder structure  
Overall, HTrack uses the same folder structure whether running the app online or offline (see image below).  A main ```htrack``` folder will house the **app.R** file, the **.csv preload**, and two other folders (```htrack_archive``` and ```mapTiles```). If using/testing the app with offline maptiles, these can be saved within ```mapTiles > ALL > OSM``` (this folder structure is created in UserLAnd as explained above).  
  
### NOTE: The source code is set to run online without the map tiles. Follow instructions within the source code to un-comment the lines that will enable using downloaded map tiles.
  
### HTrack folder structure  
<img src="https://user-images.githubusercontent.com/7705604/98297435-c53d2a00-1f8a-11eb-9aab-0c4ea60b7715.png" alt="workflow" width="300" height="400" align="auto">  
