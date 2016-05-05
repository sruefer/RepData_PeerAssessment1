## Load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

## Loading and Preprocessing the Data
x.df <- read.csv("activity.csv")          # Read csv
x.df[[2]] <- as.Date(x.df[[2]])           # convert date
x.df$na <- as.numeric(is.na(x.df$steps))  # New feature "na"
x.df$day <- weekdays(x.df$date)           # New feature "day"
x.df$day.type <- "weekday"                # New feature: "day.type"
x.df$day.type[x.df$day %in% c("Saturday", "Sunday")] <- "weekend"
x.df$day.type <- factor(x.df$day.type)
x.df$day <- factor(x.df$day, 
                   levels = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"))

x.tbl <- tbl_df(x.df)  # create tbl format dataset

# Show information
str(x.tbl)
summary(x.tbl)


## What is mean total number of steps taken per day?

### 1) Total steps per day
daily <- group_by(x.tbl, date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))
head(daily)

### 2) Histogram of daily total steps
p1 <- ggplot(daily, aes(total_steps)) + geom_histogram() +
        theme_bw() + ggtitle("Daily Total Steps") + 
        xlab("Total Steps per day") + 
        ylab("Frequency") + scale_y_continuous(breaks = c(2, 4, 6, 8, 10))
p1

### 3) Mean and Median of daily steps
mean_steps <- round(mean(daily$total_steps, na.rm = TRUE), digits = 1)
median_steps <- round(median(daily$total_steps, na.rm = TRUE), digits = 1)
print(paste0("Mean = ", mean_steps, " steps"))
print(paste0("Median = ", median_steps, " steps"))


## What is the average daily activity pattern?

### 1) Time Series Plot
daily2 <- group_by(x.tbl, interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE))
p2 <- ggplot(daily2, aes(x = interval, y = avg_steps)) + 
        geom_line() + ggtitle("Daily Average") +
        xlab("Interval (5-min)") +
        ylab("Average Steps per Interval") +
        theme_bw()
p2

### 2) Maximum Steps
max_interval <- daily2[which.max(daily2$avg_steps), 1]
print(paste0("Interval with maximum average steps: ", max_interval))


## Imputing Missing Values

### 1) Number of NA's (rows)
incomplete_rows <- sum(!complete.cases(x.df))
print(paste0("Number of rows with NA's: ", incomplete_rows))

### 2) Strategy to impute missing values

# Show map of missing values
missmap(x.df, y.labels = c(), y.at = c())

# Strategy for imputing NA's: Use interval mean steps as NA replacement.

### 3) Dataset with imputed values

x2.df <- x.df
# Loop through each row
for (i in 1:nrow(x2.df)) {
      # Impute for NA's
      if (is.na(x2.df$steps[i])) {
            x2.df$steps[i] <- round(daily2$avg_steps[daily2$interval == x2.df$interval[i]], 0)
      }
}

# Check summary of imputed dataset
summary(x2.df)

# -> no more NA's

### 4) Histogram made with new dataset
x2.tbl <- tbl_df(x2.df)
daily3 <- group_by(x2.tbl, date) %>% summarise(total_steps = sum(steps))
p3 <- ggplot(daily3, aes(total_steps)) + geom_histogram() +
         theme_bw() + ggtitle("Daily Total Steps (IMPUTED)") + 
         xlab("Total Steps per day") + 
         ylab("Frequency") + scale_y_continuous(breaks = c(2, 4, 6, 8, 10))
grid.arrange(p3, p1, nrow = 2)

summary(daily3)
mean_steps3 <- round(mean(daily3$total_steps, na.rm = TRUE), digits = 1)
median_steps3 <- median(daily3$total_steps, na.rm = TRUE)
print(paste0("Mean = ", mean_steps3, " steps"))
print(paste0("Median = ", median_steps3, " steps"))

# -> new mean and median are very similar. Previous values were smaller and
# quite different from each other.


## Differences between weekdays and weekends

wd <- filter(x2.tbl, day.type == "weekday") %>%
      group_by(interval) %>%
      summarise(avg_steps = round(mean(steps), 1))
wd$day.type <- "weekday"

we <- filter(x2.tbl, day.type == "weekend") %>%
      group_by(interval) %>%
      summarise(avg_steps = round(mean(steps), 1))
we$day.type <- "weekend"

x3 <- rbind(wd, we)
x3$day.type <- factor(x3$day.type)

p4 <- ggplot(x3, aes(x = interval, y = avg_steps)) +
      facet_wrap(~day.type, nrow = 2) +
      geom_line() +
      ggtitle("Daily Average") +
      xlab("Interval (5-min)") +
      ylab("Average Steps per Interval") +
      theme_bw()
p4
