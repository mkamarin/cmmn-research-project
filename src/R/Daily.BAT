@ECHO OFF
SETLOCAL ENABLEEXTENSIONS
SET me=%~n0

ECHO DAILY R script to process the CMMN Survey data
ECHO Current directory: %cd%

REM Configuration (must adjust this to your environment)
REM ====================================================

REM PATH should include directories for Rscript and pandoc:
PATH "C:\Program Files\R\R-3.3.1\bin";"C:\Program Files\RStudio\bin\pandoc";%path%

REM Set working directory (where all the R scripts are located)
set wd="D:\src\R"

REM Set fully qualified LimeSurvey results file location
REM (file generated from LimeSurvey server using export results to application, 
REM using: CVS, all responses, question code, and answer codes) 
set rf="D:\data\results-survey338792.csv"

REM Set fully qualified path to pandoc
set pd="C:/Program Files/RStudio/bin/pandoc"

REM End Configuration
REM ====================================================
REM If the configuration is correct the rest below should just work

REM Let be sure we are in the working directory
cd %wd%

REM get rid of the temporary input file
del /F /Q "in-survey-data-file.csv"

REM Instead of just coping the results-survey338792.csv file to in-survey-data-file.csv,
REM we execute an script that will do some fixing
REM This script will copy the file and replace "Y" (Yes) to 1 and "N" (No) to 2.
REM This is done in the R script instead of in LimeSurvey, 
REM because LimeSurvey has a problem were both "N" and NA become 2
ECHO %me%: start Rscript copy-and-fix-file.r
Rscript copy-and-fix-file.r  %wd%  %rf%

REM Now we are ready to process the intermediate file (in-survey-data-file.csv)
REM This next script runs the rest of the process
REM First, it will convert the intermediate file into a clean data set (out-clean-data.csv)
REM That clean data set will be used for all the statistical processing
ECHO %me%: start Rscript daily.r
Rscript daily.r  %wd%  %pd%

ECHO %me%: DONE
PAUSE

