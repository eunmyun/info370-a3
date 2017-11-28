#install.packages("dplyr")
#install.packages("ggplot2")

# Question 2:
df2 <- strava.activity %>%
        select('kudos_count', 'kilojoules', 'average_heartrate') %>%
        filter(average_heartrate != 0.0) %>%
        mutate(energy_per_hr = kilojoules / average_heartrate) %>%
        filter(energy_per_hr != is.na(energy_per_hr)) %>%
        filter(energy_per_hr < 100)
View(df2)

cor(df2[,1], 'kudos_count','energy_per_hr', use = "pairwise.complete.obs")

t.test(df2$energy_per_hr, df2$kudos_count, paired = FALSE)
fit <- lm(energy_per_hr ~ kudos_count, data = df2)
plot(energy_per_hr ~ kudos_count, data = df2)
abline(fit)

# Statistical Analysis: Linear/multiple Regression
fit <- lm(energy_per_hr ~ kudos_count, data = df2)
plot(energy_per_hr ~ kudos_count, data = df2)
abline(fit)
summary(fit)

fit2  <- lm(kudos_count ~ energy_per_hr, data = df2)
plot(kudos_count ~ energy_per_hr, data = df2, main="Fitness vs. Kudos Earned")
abline(fit2)
summary(fit2)

fit3 <- lm(kudos_count ~ kilojoules + average_heartrate, data = df2)
plot(kudos_count ~ kilojoules + average_heartrate, data = df2)
abline(fit3)
summary(fit3)