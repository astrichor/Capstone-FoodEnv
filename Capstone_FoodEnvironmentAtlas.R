### Springboard - Foundations of Data Science
### Rebecca H. Wang's Capstone Project
### Food Environment Atlas

## Load the data
rawstate <- read.csv("FEA_state_programs.csv", header = TRUE)
rawcounty <- read.csv("FEA_county_populations.csv", header = TRUE)
str(rawcounty)
