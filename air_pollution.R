# library
library(openair)
library(ggplot2)
library(dplyr)
library(PostcodesioR)
library(lubridate)
library(lattice)
library(phecharts)

# data for site "my1" - Westminster - Marylebone Road
data <- importKCL(site = c("my1"), year = 2019:2020,
                  pollutant = c("nox", "no2", "o3"))

data <- data %>%
  mutate(month = format(date,"%Y-%m"),
         week = week(date),
         date_only = format(date,"%Y-%m-%d"),
         week_day = wday(date, label = TRUE),
         hour = hour(date),
         day_hour = paste(week_day, hour, sep = " "),
         week_start = floor_date(as.Date(date_only), unit = "week", 
                                 week_start = 1))

# get 2020 data
data_2020 <- data %>%
  filter(year(date) == 2020)

summaryPlot(data_2020)

timePlot(data_2020, pollutant = c("nox"), smooth=TRUE, avg.time = "day")

data %>%
  filter(month == "2020-03") %>%
  timeVariation(pollutant = "nox", group = "week_start", difference = FALSE)

calendarPlot(data, pollutant = "nox", year = 2019:2020)

trendLevel(data_2020, x = "week_start", y = "hour", pollutant = "nox")


# filter data
march_2020_rush_data <- data %>%
  filter(month == "2020-03" & hour %in% c(9, 5)) %>%
  select(date_only, date, nox, hour) %>%
  splitByDate(dates= "24/3/2020", labels = c("pre", "post")) %>%
  rename(pre_post = split.by) %>%
  mutate(hour = as.character(hour))

# get pre post average
pre_post_average <- march_2020_rush_data %>%
  group_by(hour, pre_post) %>%
  summarise(average = mean(nox, na.rm = TRUE)) %>%
  merge(march_2020_rush_data, by = c("hour", "pre_post")) %>%
  select(date_only, average, hour) %>%
  rename(nox = average)

# plot
march_2020_rush_data %>%
  ggplot(aes(x = date_only, y = nox, group = hour, colour = hour)) +
  geom_line() +
  geom_vline(xintercept = "2020-03-23", linetype = "dashed", color = "red", size = 1) +
  geom_line(data = pre_post_average, aes(x = date_only, y = nox), linetype = "dashed", size = 1.5) +
  labs(title = "Levels of nox at rush hour Marylebone Road, Westminster, March 2020") +
  theme(axis.text.x = element_text(angle = 90),
        title = element_text(size = 8))
