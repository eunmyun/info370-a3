# Load necessary libraries
library(dplyr)

# Load necessary data
strava.activity <- read.csv(file='../data/strava_activity.csv', stringsAsFactors = FALSE)

# Exploratory Data Analysis
# View(head(strava.activity))
strava.activity <- strava.activity[,c("athlete.country", "athlete.sex", "achievement_count", "average_heartrate", "average_speed", "average_watts", "distance", "elapsed_time", "elev_high", "elev_low", "kilojoules", "location_country", "max_heartrate", "max_speed", "moving_time", "total_elevation_gain", "type", "kudos_count")]
strava.activity <- mutate(strava.activity, average_speed = distance / moving_time)
by.type <- strava.activity %>% 
          group_by(type) %>%
          summarise(average_speed = mean(average_heartrate, na.rm=TRUE),
          total = n())
View(by.type)

kayaks <- filter(strava.activity, type == "Kayaking") %>%
          select(average_speed)
kayaks <- as.numeric(kayaks)
print(mean(kayaks))
View(strava.activity)