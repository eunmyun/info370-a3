# Load necessary libraries
library(dplyr)

# Load necessary data
strava.activity <- read.csv(file='../data/strava_activity.csv', stringsAsFactors = FALSE)

# Exploratory Data Analysis
head(strava.activity)