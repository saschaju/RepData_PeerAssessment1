# Load packages
library(dplyr)
library(ggplot2)
# Set working directory
setwd("~/Desktop/coursera/5_Reproducible_Research/Week_2")

# Unzip and read data
unzip(zipfile="repdata%2Fdata%2Factivity.zip")
dat <- read.csv("activity.csv")

# Compute sum of steps by day and plot histogram
dat1 <- dat %>% group_by(date) %>% summarize(sumstepsbyday = sum(steps))
ggplot(dat1, aes(sumstepsbyday)) + geom_histogram() + xlab("Steps per Day") + ylab("Nr. of Days")

# Compute the avg. nr. of steps and the median sum of steps per day
mean_raw <- mean(dat1$sumstepsbyday, na.rm = TRUE)
median_raw <- median(dat1$sumstepsbyday, na.rm = TRUE)
mean_raw
median_raw

# Compute the avg. nr. of steps per interval and plot as time series
dat2 <- dat %>% group_by(interval) %>% summarize(avgstepsbyday = mean(steps, na.rm = TRUE))
ggplot(dat2, aes(x = interval, y = avgstepsbyday, group = 1)) +
        geom_line() + geom_smooth() + xlab("Time") + ylab("Avg. Steps per Day")

# Get the row with the max avg. nr. of steps
dat2[which.max(dat2$avgstepsbyday), ]

# Count nr. of rows with NA
na_count <- sum(is.na(dat$steps))
na_count

# Replace NA with average steps of corresponding interval
dat3 <- dat %>%
        group_by(interval) %>% 
        mutate_each(funs(replace(., which(is.na(.)),
                                 mean(., na.rm = TRUE))))

# Compute the sum of steps per day and plot histogram after missing data treatment
dat4 <- dat3 %>% group_by(date) %>% summarize(sumstepsbyday = sum(steps))
ggplot(dat4, aes(sumstepsbyday)) + geom_histogram() + xlab("Steps per Day") + ylab("Nr. of Days")

# Compute the avg. nr. of steps and the median sum of steps per day after missing data treatment
dat5 <- dat3 %>% group_by(date) %>% summarize(sumstepsbyday = sum(steps))
mean_clean <- mean(dat5$sumstepsbyday, na.rm = TRUE)
median_clean <- median(dat5$sumstepsbyday, na.rm = TRUE)
mean_clean
median_clean

# Convert "date" to date format
dat3$date <- as.Date(dat3$date)

# Compute dummy variable for weekday and weekend
dat3$weekday <- ifelse(weekdays(dat3$date) == "Samstag" | weekdays(dat3$date) == "Sonntag",
                       "weekend", "weekday")
# Compute avg. nr. of steps per interval and weekday and plot as time series per weekday (panel) 
dat6 <- dat3 %>% group_by(weekday, interval) %>% summarize(avgbyweekday = mean(steps))
ggplot(dat6, aes(interval, avgbyweekday)) + geom_line() + facet_grid(weekday ~.) + 
        xlab("Interval") + ylab("Avg. Nr. of Steps")

