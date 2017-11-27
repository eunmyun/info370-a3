#install.packages("dplyr")
# Load necessary libraries
library(dplyr)

# Load necessary data
strava.activity <- read.csv(file='../data/strava_activity.csv', stringsAsFactors = FALSE)

# Exploratory Data Analysis
# Choose relevant characteristics
strava.activity <- strava.activity[,c("athlete.country", "athlete.sex", "achievement_count", "average_heartrate", "average_speed", "distance", "elapsed_time", "kilojoules", "location_country", "max_heartrate", "max_speed", "moving_time", "total_elevation_gain", "type", "kudos_count")]
View(head(strava.activity))
# Get rid of rows with no gender
strava.activity <- filter(strava.activity, athlete.sex != "")
View(strava.activity)

# Basic observation
# number of characteristics
ncol(strava.activity)
# number of entries
nrow(strava.activity)
# number of males
nrow(filter(strava.activity, athlete.sex == 'M'))
# number of females
nrow(filter(strava.activity, athlete.sex == 'F'))

# The column average_speed is distance divided by moving time
speed <- strava.activity %>%
          mutate(calculated = distance / moving_time) %>%
          select("average_speed", "calculated")
View(speed)

# Plot distributions of some variables
par(mfrow=c(3,1))
avg.speed <- table(as.integer(strava.activity$average_speed))
barplot(avg.speed, main="Average Speed Distribution", xlab="Average Speed Rounded to Integer", ylab="Number of Athletes")
avg.kj <- table(as.integer(strava.activity$kilojoules))
barplot(avg.kj, main="Average Energe(KJ) Distribution", xlab="Average Energy(KJ)", ylab="Number of Athletes")
avg.heartrate = table(as.integer(strava.activity$average_heartrate))
barplot(avg.heartrate, main="Average Heart Rate Distribution", xlab="Average Heart Rate", ylab="Number of Athletes")

# Group by type and calculated averages
by.type <- strava.activity %>% 
          group_by(type) %>%
          summarise(avg_time = mean(moving_time, na.rm=TRUE),
                    avg_distance = mean(distance, na.rm=TRUE),
                    avg_elevation = mean(total_elevation_gain, na.rm=TRUE),
                    avg_speed = mean(average_speed, na.rm=TRUE),
                    avg_kilojoules = mean(kilojoules, na.rm=TRUE),
                    avg_heartrate = mean(average_heartrate, na.rm=TRUE),
                    total = n(),
                    male = sum(athlete.sex == 'M'),
                    female = sum(athlete.sex == 'F'))
View(by.type)

# Group by gender and calculated averages
by.gender <- strava.activity %>%
              group_by(athlete.sex) %>%
              summarise(avg_time = mean(moving_time, na.rm=TRUE),
                        avg_distance = mean(distance, na.rm=TRUE),
                        avg_elevation = mean(total_elevation_gain, na.rm=TRUE),
                        avg_speed = mean(average_speed, na.rm=TRUE),
                        avg_kilojoules = mean(kilojoules, na.rm=TRUE),
                        avg_heartrate = mean(average_heartrate, na.rm=TRUE),
                        total = n())
View(by.gender)

par(mfrow=c(5,1))

# Data Preparation
mftable <- function(exercise.type) {
  t <- strava.activity %>%
        filter(type == exercise.type) %>%
        group_by(athlete.sex) %>%
        summarise(avg_time = mean(moving_time, na.rm=TRUE),
                  avg_distance = mean(distance, na.rm=TRUE),
                  avg_elevation = mean(total_elevation_gain, na.rm=TRUE),
                  avg_speed = mean(average_speed, na.rm=TRUE),
                  avg_kilojoules = mean(kilojoules, na.rm=TRUE),
                  avg_heartrate = mean(average_heartrate, na.rm=TRUE),
                  total = n())
  return(t)
}

library("nnet")
test <- multinom(athlete.sex ~ average_speed + average_heartrate + kilojoules + total_elevation_gain, data = strava.activity)
summary(test)

strava.activity$athlete.sex <- factor(strava.activity$athlete.sex)
strava.activity$gender <- relevel(strava.activity$athlete.sex, ref = "F")

test <- multinom(gender ~ average_speed + average_heartrate + kilojoules + total_elevation_gain, data = strava.activity)
summary(test)




