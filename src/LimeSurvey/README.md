# Instructions on how to use the survey

The survey was created using [LimeSurvey Version 2.06lts](www.limesurvey.org), and it was hosted at [limeservice.com](https://www.limeservice.com/en/).

To modify the survey, you can import it in an instance of LimeSurvey Version 2.06lts, and you also need to import the resources.
After that, you need to host the tutorial somewhere and then update the tutorial question in the tutorial group.
The survey Tutorial group has the tutorial question, which implements a frame hosting the tutorial (writen in eXe Learning).
The frame html code is:
```
<iframe align="middle" frameborder="1" height="900" scrolling="yes" src="http://cmmn.limequery.org/upload/surveys/338792/media/tutorial.html" width="1100"></iframe>
```

## Files
* **CMMN Complexity metrics project.pdf** -- is a printout of the LimeSurvey logic file.
* **limesurvey_survey_338792.lss** -- is the LimeSurvey Version 2.06lts source of the survey.
* **README.md** -- is this file.
* **resources-survey-338792.zip** -- contains all the figures used in the survey (it was generated from `LimeSurvey -> survey -> general settings -> resources -> export resources as ZIP file`).
