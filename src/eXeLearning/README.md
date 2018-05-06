
# Instructions on how to create/update the CMMN tutorial

## Introduction
We used eXe Learning version 2.0.4 (http://exelearning.net/?lang=en) to create/update the tutorial.
The file tutorial.elp is the eXe Learning file containing the tutorial.
You can open that file with eXe Learning and make changes as required. 
Then you can export it as `File -> Export -> Web Site -> Zip file`.
There are two things we need to do to fix-up the zip file:

1. First extract the zip file in a temporary directory, and do the following:

2. rename the index.html to tutorial.html

3. apply the script.vim to all the html files (open the html file in gvim and do :source path/script.vim and save the file). This change Next and Previous to Next tutorial page, and Previous tutorial page.

4. Now you can zip again the tutorial directory.

5. you are now ready to deploy the tutorial as a traditional web application (just unzip the tutorial in a web site directory).

## Files in this directory

* **Pics.zip** -- contains all the resources (images used in the tutorial).
* **README.md** -- this file
* **script.vim** -- vim script to replace Next to Next tutorial page, and Previous to Previous tutorial page in the html files
* **tutorial.elp** -- eXe Learning source file
* **Tutorial.pdf** -- pdf version of the tutorial (eXe Learning -> File -> Print)
* **Tutorial.zip** -- tutorial web page (eXe Learning -> File -> Export -> Web Site -> Zip file)


