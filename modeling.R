#import library
library(ggplot2)

#Question 1: 
by.gender <- read.csv(file='gender_data.csv', stringsAsFactors = FALSE)
strava.activity <- read.csv(file='cleaned_data.csv', stringsAsFactors = FALSE)

#putting graph in one sheet
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}

p <- ggplot(data=by.gender, aes(x=athlete.sex, y=avg_kilojoules, fill=athlete.sex)) +
  geom_bar(stat="identity")

p2 <- ggplot(data=by.gender, aes(x=athlete.sex, y=avg_speed, fill=athlete.sex)) +
  geom_bar(stat="identity")

p3 <- ggplot(data=by.gender, aes(x=athlete.sex, y=avg_distance, fill=athlete.sex)) +
  geom_bar(stat="identity")

p4 <- ggplot(data=by.gender, aes(x=athlete.sex, y=avg_heartrate, fill=athlete.sex)) +
  geom_bar(stat="identity")

multiplot(p, p2, p3, p4, cols=2)


# Statistical Modelling: Multinomial Regression Analysis
library("nnet")

strava.activity$athlete.sex <- factor(strava.activity$athlete.sex)
strava.activity$gender1 <- relevel(strava.activity$athlete.sex, ref = "F")
test1 <- multinom(gender1 ~ average_speed + average_heartrate + kilojoules + total_elevation_gain, data = strava.activity)
summary(test1)

strava.activity$gender = relevel(strava.activity$athlete.sex, ref = "M")
test2 <- multinom(gender ~ average_speed + average_heartrate + kilojoules + total_elevation_gain, data = strava.activity)
summary(test2)

#Question 2:
df2 <- read.csv(file='fitness.csv', stringsAsFactors = FALSE)

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