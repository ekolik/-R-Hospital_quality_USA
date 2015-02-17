# Introduction

The data for this assignment come from the <a href=http://hospitalcompare.hhs.gov> Hospital Compare web site </a>
run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and
information about the quality of care at over 4,000 Medicare-certied hospitals in the U.S. This dataset es-
sentially covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining
whether hospitals should be ned for not providing high quality care to patients 

The Hospital Compare web site contains a lot of data and we will only look at a small subset for this
assignment. The<a href = https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip> zip file</a> contains three files:
* `outcome-of-care-measures.csv` - contains information about 30-day mortality and readmission rates
for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
* `hospital-data.csv` - contains information about each hospital.
* `Hospital_Revised_Flatfiles.pdf` - descriptions of the variables in each file (i.e the code book).
* 
# Task: rank hospitals in all states

Write a function called `rankall` that takes two arguments: an outcome name (`outcome`) and a hospital ranking (`num`). The function reads the `outcome-of-care-measures.csv` file and returns a 2-column data frame containing the hospital in each state that has the ranking specified in `num`. For example the function call `rankall("heart attack", "best")` would return a data frame containing the names of the hospitals that are the best in their respective states for 30-day heart attack death rates. The function should return a value for every state (some may be `NA`). The first column in the data frame is named `hospital`, which contains the hospital name, and the second column is named `state`, which contains the 2-character abbreviation for the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

The function should check the validity of its arguments. If an invalid `outcome` value is passed to `rankall`, the function should throw an error via the stop function with the exact message "invalid outcome". The `num` variable can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better). If the number given by `num` is larger than the number of hospitals in that state, then the function should return `NA`.

