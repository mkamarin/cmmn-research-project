
# Research data, sources and documents for thesis on Exploring Complexity Metrics for Artifact-Centric Business Process Models
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1241595.svg)](https://doi.org/10.5281/zenodo.1241595)

This repository contains the supplemental material for the thesis ["Exploring Complexity Metrics for Artifact-Centric Business Process Models" by Marin, Mike A., Ph.D., University of South Africa (South Africa), 2017, 434; 10759956](https://pqdtopen.proquest.com/pubnum/10759956.html).

Please cite as: Marin, Mike A. (2018). Research data, sources and documents for thesis on Exploring Complexity Metrics for Artifact-Centric Business Process Models (Version v1.0) [Data set]. Zenodo. [http://doi.org/10.5281/zenodo.1241595](http://doi.org/10.5281/zenodo.1241595).


## Data Sets (data folder)
The following files correspond to the data collected during the empirical validation described
in Chapter 8. These can be found in the supplementary material media under the data folder.

* **dataset-all(description).pdf**. This document describes all of the variables present in the data-set – variables in files *data/dataset-all.csv* and *data/dataset-clean.csv*.

* **dataset-all.csv**. This comma separated value file contains all of the data collected from the survey, including data from incomplete surveys. This file was generated using the raw data file *data/in-survey-data-file.csv* and file *src/R/CMMN-Convert-File.Rmd*.

* **dataset-clean.csv**. This comma separated values file contains the data set containing only complete and usable surveys. It is a subset of file *data/dataset-all.csv*.

* **in-independent-variables-map.csv**. This comma separated values file contains a mapping of the independent variables with each of the 30 sets. Each set corresponds to two models (model A and model B) extracted from the six models that were tested (see *docs/The6Models.pdf*).

* **in-independent-variables.csv**. This comma separated values file contains the calculated value of the independent variables.

* **in-survey-data-file.csv**. This comma separated values file contains the raw data from LimeSurvey after being converted to a file format suitable for use with R. This file was generated from raw data file *data/results-survey338792.csv* using file *src/R/copy-and-fix-file.r*.

* **in-survey-var-names.csv**. This comma separated values file contains the survey variable names.

* **in-weights-scaled-ordinal-rounded.csv**. This comma separated values file contains the independent variable weight scaled to the ordinal values.

* **in-weights.csv**. This comma separated values file contains the independent variables for the weights used to calculate CC.

* **out-comments.txt**. This file contains the comments left by the survey participants. The data was extracted from the last optional question in the survey (“Any final comments that you may want to share with the research team?”).

* **results-survey338792 (description).pdf**. This document is the LimeSurvey logic file describing all of the questions included in the survey. Each subject was exposed to a subset of the questions described in this document.

* **results-survey338792.csv**. This comma separated values file contains the raw data exported from LimeSurvey.

* **survey_archive_338792.lsa**. This file is a LimeSurvey archive containing the survey and the responses from all of the subjects.

## Documents (docs folder)
The following documents are related to this research project. These can be found under the docs folder.

* **2012-10-31 MMarin DPSET02 Proposal.pdf**. This document contains the research proposal presented to University of South Africa (UNISA) for this research.

* **2016-05-23 MAMarin_Student_Ethical_Clearance-v5.pdf**. This document contains the research ethical clearance application form presented to the College of Science, Engineering and Technology’s (CSET) Research and Ethics Committee.

* **2016-06-08 Pilot-full-answers.pdf**. This is the output of LimeSurvey containing all of the responses from the pilot survey that included 12 subjects who were used to test the survey instrument.

* **2016-06-10 Survey and Tutorial Pilot.pdf**. This is a short report describing the pilot that was conducted for the survey and the tutorial.

* **2016-06-15 Invitation.pdf**. This document contains the invitation that was posted in the Case Management Model and Notation (CMMN)’s Linkedin group soliciting participants for the survey and tutorial. Similar invitations were posted in other Linkedin groups or emailed to potential participants.

* **2016-06-15 Survey-Example.pdf**. This a sample survey generated by LimeSurvey.  This sample survey uses models one and two, of the six possible models. Each survey presents the subject with only two models, and the possible responses to the questions are ordered in a random fashion, so that almost every subject gets a slightly different version of the survey.

* **2016-06-15 Survey-Tutorial.pdf**. This is a textual version of the CMMN tutorial that was used for the survey. Each page on this document corresponds to a webpage presented to the subjects.

* **2016-08-16 Charity-donations.pdf**. This file contains the receipts or emails confirming payment to the charities that subjects participating in the survey selected.  In total $510.00 was paid to the different charities.

* **2016-08-16 CMMN-basic-stats(raw-dataset).pdf**. This document was generated using R and contains basic statistics that were calculated using the raw data generated from the survey. This file was emailed to the subjects who had requested to be informed of the outcome of the survey.

* **Basic-stats.pdf**. This document was generated using R and contains the statis- tical analysis of the survey using the completed and valid survey data (a subset of the raw data).

* **Cherries.pdf**. This document contains the completed Checklist for Reporting Results of Internet E-Surveys (CHERRIES).

* **The6Models.pdf**. This document describes how the researcher arrived at the six models that were used for the empirical validation.

* **SLR-analysis.pdf**. This document was generated using R and contains the full statistical analysis for the Systematic Literature Review (SLR).

* **FSM-2-GSM.pdf**. This document is an extract from Marin et al. [Mar+16] describing the transformation of Deterministic Finite State Machine (DFSM) into Guard-Stage-Milestone (GSM) types.

## Systematic Literature Review of Metrics (Metrics(SLR) folder)
State of the Art through Systematic Reviews (StArt) [Fab+16] was used for the SLR presented
in Chapter 6.

* **report(full).xls**. This spreadsheet was generated using StArt, and contains all of the informations from the study selection activity performed in the SLR.

* **Analysis.xlsx**. This spreadsheet contains all of the information extracted from the papers during the data extraction activity performed in the SLR.

### State of the Art through Systematic Reviews (StArt sub-folder)
* **StArt/SLR-BPM_Metrics.start**. This file is in the StArt file format and contains all of the information from the study selection activity. This file can be read using StArt version 2.3.4.

### Bibliograpy (bibtex sub-folder)
* **bibtex/2016-07-01_ACM.bib**. This file contains the results from the ACM digital library query.

* **bibtex/2016-07-02_Scopus.bib**. This file contains the results from the Elsevier’s Scopus query.

* **bibtex/2016-07-02_Web-of-science.bib**. This file contains the results from the Thomson Reuters’ Web of Science query.

* **bibtex/2016-07-03_Google-academic.bib**. This file contains the results from the Google Scholar query.

* **bibtex/2016-07-03_IEEE-1.bib**. This file contains the results from the first IEEE Xplore digital library query.

* **bibtex/2016-07-03_IEEE-2.bib**. This file contains the results from the second IEEE Xplore digital library query.

* **bibtex/2016-07-03_Science-direct.bib**. This file contains the results from the Elsevier’s Science Direct query.

* **bibtex/2016-07-03_Springer.bib**. This file contains the results from the Springer’s SpringerLink query.

### SLR Data (data sub-folder)
* **data/in.slr.raw.report.csv**. This file contains the list of all of the papers that were reviewed during the SLR. This file is exported from file *Metrics(SLR)/report(full).xls*) in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.dup-metrics.csv**. This file contains the duplicated metrics that were identified during the review. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.metrics.csv**. This file contains all of the metrics identified by the review. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.papers.csv**. This file contains all of the papers that were accepted during the selection activity. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.theor.vali.csv**. This file contains the theoretical validation in- formation extracted from the papers in this review. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.validated-metrics.csv**. This file contains a record for each metric and for each empirical validation performed in that metric. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/in.slr.raw.validation.csv**. This file contains information for each of the validation studies identified during the SLR. This file was exported from file *Metrics(SLR)/Analysis.xlsx* in a comma separated values file format that can be read by an R program.

* **data/SLR-data(variable-description).pdf**. This file describes the variables used in all of the comma separated values files. The variable names correspond to the names used in the R programs.

## Sources (src folder)
During the production of this research project several sources were created. These can be found in the src folder.

### R sources (R sub-folder)
R was used to perform most of the statistical calculations. With the exception of power calculations that were done using G*Power 3. Some of the files in the R
folder include:

* **R/Instructions(read-me-first).pdf**. Basic instructions on how to perform and build the reports for the empirical validation’s statistical analysis.

* **R/CMMN-basic-stats.Rmd**. Generate basic demographic statistics.

* **R/Basic-stats.Rnw**. This file generates the basic statistical analysis for the survey used in the empirical validation.

* **R/CMMN-Convert-File.Rmd**. Script that generates the dataset-all.csv, and dataset-clean.csv files.

* **R/CMMN-Sample.Rmd**. Compares the data set against the expected sample size for each experiment.

* **R/CMMN-Weights.Rmd**. Recalculates CC (iv.A.CC, iv.B.CC, and iv.C.CC) and generates the dataset-clean-post.csv.

* **R/Results.Rnw**. Contains the main statistical analysis for the survey used for the empirical validation. 

* **R/copy-and-fix-file.r**. Script used to copy and fix the LimeSurvey exported file.

* **R/daily.r**. Main R script that calls all *.Rmd scripts.

* **R/share-my-functions.r**. A set of statistical analysis functions that were developed for the empirical validation.

* **R/share-read-dataset.r**. A set of functions used to implement a common way to read the data sets.

* **R/SLR-analysis.Rmd**. This file contains the processing and analysis of the SLR data sets. 

### eXeLearning
EXeLearning was used to create the online tutorial.

* **eXeLearning/README.txt**. This file contains instructions on how to update the CMMN tutorial using the files in this directory.

* **eXeLearning/Tutorial.pdf**. This is a pdf version of the tutorial.

* **eXeLearning/Tutorial.zip**. This compressed file contains the tutorial as a deployable web application.

* **eXeLearning/pics.zip**. This compressed file contains all of the figures used in the tutorial.

* **eXeLearning/script.vim**. This script improves the page navigation of the tutorial.

* **eXeLearning/tutorial.elp**. eXe Learning version 2.0.4 source of the tutorial.

### LimeSurvey
LimeSurvey [Lim16] was used to develop and to run the online survey.

* **LimeSurvey/CMMN Complexity metrics project.pdf**. LimeSurvey logic file for the CMMNcomplexity metrics survey.

* **LimeSurvey/README.txt**. This file describes how to modify and use the LimeSurvey files in this directory.

* **LimeSurvey/limesurvey_survey_338792.lss**. LimeSurvey version 2.06lts containing the source of the CMMNcomplexity metrics survey.

* **LimeSurvey/resources-survey-338792.zip**. This compressed file contains all of the figures used in the CMMNcomplexity metrics survey.

### MiniZinc
MiniZinc  was used to model and solve the constraints to identify the six models used in the online survey. The MiniZinc folder contains all of the source and data files that were used to solve the constraints required to create the six models with CTS = 90, and the other metrics with very different values.
