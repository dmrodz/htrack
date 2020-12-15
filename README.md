# HTrack
HTrack (Household Tracking) is an application built in R-Shiny for use on encrypted Android devices. This app was created to allow for offline data collection in studies where stringent data-security requirements must be met, though it can also be run online if needed. The application allows for field navigation and secure field-level data capture. It also serves as a bridge to an alternate software, like EpiInfo, to collect participant-level questionnaire data. This repository houses:  
+ The source code for this application for users or researchers performing field-studies or other field-assessment strategies,
+ Installation instructions and app dependencies.  
+ Troubleshooting is provided through the Issues within this repo as needed.  

HTrack is setup and ready to use by following three main steps, as outlined below:  
<img src="https://user-images.githubusercontent.com/7705604/97463214-e1e7bb00-1915-11eb-97bb-3d01dd31713c.png" alt="workflow" align="auto">

### NOTE: The source code is set to run online without the need for downloaded map tiles, using the provided sample preload file within this repo.  For testing the app using downloaded map tiles, follow the instructions commented within the source code to enable this feature. If testing the app directly through a desktop computer or laptop (not an android device), please use the folder structure specified at the end of the installation manual.  

#### Citation: Rodríguez DM, Ryff K, Sánchez-Gonzalez L, Rivera-Amill V, Paz-Bailey G, Adams L (2020) HTrack: A new tool to facilitate public health field visits and electronic data capture. PLoS ONE 15(12): e0244028. https://doi.org/10.1371/journal.pone.0244028
