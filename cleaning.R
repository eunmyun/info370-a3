# Load necessary data
strava.activity <- read.csv(file='strava_activity.csv', stringsAsFactors = FALSE)

#Question 1:
# Choose relevant characteristics
strava.activity <- strava.activity[,c("athlete.country", "athlete.sex", "achievement_count", "average_heartrate", "average_speed", "distance", "elapsed_time", "kilojoules", "location_country", "max_heartrate", "max_speed", "moving_time", "total_elevation_gain", "type", "kudos_count")]
View(head(strava.activity))
# Get rid of rows with no gender
strava.activity <- filter(strava.activity, athlete.sex != "")
View(strava.activity)

#write csv
write.csv(strava.activity, file = "cleaned_data.csv")

#Question 2:
df2 <- strava.activity %>%
  select('kudos_count', 'kilojoules', 'average_heartrate') %>%
  filter(average_heartrate != 0.0) %>%
  mutate(energy_per_hr = kilojoules / average_heartrate) %>%
  filter(energy_per_hr != is.na(energy_per_hr)) %>%
  filter(energy_per_hr < 100)
View(df2)

#write csv
write.csv(df2, file = "fitness.csv")