library(dplyr)
library(ggplot2)
library(car)
library(stringr)
library(lubridate)
library(zoo)

num.cols <- c("twd", "pr", "at", "ws", "dp", "sr", "lr", "day.of.year")

tstoy04_series <- read.csv("tstoy04/tstoy04_series.csv")
tstoy04_series$tree.id <- 1:nrow(tstoy04_series)
tstoy04_sites <- read.csv("tstoy04/tstoy04_sites.csv")

list.ts.files <- list.files("tstoy04/SeparateSeries", full.names = TRUE)
list.ts <- lapply(list.ts.files, read.csv)
df.ts <- bind_rows(list.ts, .id = "tree.id")

df.ts.series <- merge(df.ts, tstoy04_series)

length(unique(df.ts.series$tree.id))
length(unique(df.ts.series$site))

class(df.ts.series$ts)

df.ts.series <- df.ts.series %>%
  group_by(tree.id) %>%
  mutate(
    year =  substr(ts, 1, 4),
    ts = as.Date(ts),
    diff.ts = ts - lag(ts),
    diff.days = as.numeric(diff.ts, units = 'days'),
    diff.days = ifelse(is.na(diff.days), 0, diff.days),
    cum.time.days = cumsum(diff.days),
    day.of.year =  yday(ts),
    month = month(ts),
    week = week(ts)
)

class(df.ts.series$day.of.year)

first.time.point <- df.ts.series %>%
  group_by(tree.id) %>%
  slice(1) %>%
  pull(ts)
all(first.time.point == "2020-01-01")

# Moving average

df.ts.series <- df.ts.series %>%
  mutate(
    across(
      all_of(num.cols),
      ~ rollmean(.x, k = 7, fill = NA, align = "right"),
      .names = "{col}_rolling7"
    ),
    across(
      all_of(num.cols),
      ~ rollmean(.x, k = 30, fill = NA, align = "right"),
      .names = "{col}_rolling30"
    )
  )

# merge df.ts.series with tstoy04\tstoy04_sites.csv
# column site in df.ts.series is the same as tree.id in tstoy04_sites.csv
tstoy04_sites <- read.csv("tstoy04/tstoy04_sites.csv")
colnames(tstoy04_sites)

df.ts.series2 <- merge(df.ts.series, tstoy04_sites, by.x = "site", by.y = "site_name") 
nrow(df.ts.series2) == nrow(df.ts.series)
write.csv(df.ts.series2, "df.ts.series2.csv")
