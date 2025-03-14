library(dplyr)
library(ggplot2)
library(car)
library(stringr)
library(lubridate)

tstoy04_series <- read.csv("tstoy04/tstoy04_series.csv")
tstoy04_series$site.id <- 1:nrow(tstoy04_series)
tstoy04_sites <- read.csv("tstoy04/tstoy04_sites.csv")

list.ts.files <- list.files("tstoy04/SeparateSeries", full.names = TRUE)
list.ts <- lapply(list.ts.files, read.csv)
df.ts <- bind_rows(list.ts, .id = "site.id")

df.ts.series <- merge(df.ts, tstoy04_series)

class(df.ts.series$ts)

df.ts.series <- df.ts.series %>%
  group_by(site.id) %>%
  mutate(
    year =  substr(ts, 1, 4),
    ts = as.Date(ts),
    diff.ts = ts - lag(ts),
    diff.days = as.numeric(diff.ts, units = 'days'),
    diff.days = ifelse(is.na(diff.days), 0, diff.days),
    cum.time.days = cumsum(diff.days),
    day.of.year =  yday(ts),
    month = month(ts)
)

class(df.ts.series$day.of.year)

first.time.point <- df.ts.series %>%
  group_by(site.id) %>%
  slice(1) %>%
  pull(ts)
all(first.time.point == "2020-01-01")

plot_basic <- ggplot(df.ts.series, aes(x = cum.time.days, y = twd, group = site.id)) +
  geom_line(aes(color = site.id))

plot_basic +
  theme_bw() +
  facet_grid(vars(species), vars(site))

plot_basic +
  geom_vline(xintercept = 365, color = "blue") +
  geom_vline(xintercept = 730, color = "blue") +
  facet_wrap(~species)

ggplot(
  df.ts.series,
  aes(x = day.of.year, y = twd,
  group = interaction(year, site.id))) +
  geom_line(aes(color = year)) +
  facet_wrap(~species)

cor(df.ts.series$twd, df.ts.series$at)
cor(df.ts.series$twd, df.ts.series$day.of.year)
cor(df.ts.series$twd, df.ts.series$year)

lapply(df.ts.series, class)
num.cols <- c("twd", "pr", "at", "ws", "dp", "sr", "lr", "day.of.year")
num.vars.df <- df.ts.series[,num.cols]
cor_matrix <- cor(num.vars.df, use = "complete.obs")
cor_matrix

pairs(num.vars.df)

ggplot(
  df.ts.series,
  aes(x = at, y = twd,
  group = interaction(year, site.id))) +
  geom_point(aes(color = year)) +
  facet_wrap(~species)

# Moving average

library(zoo)

df.ts.series <- df.ts.series %>%
  mutate(
    TWD_rolling7 = rollmean(twd, k = 7, fill = NA, align = "right")
  )

ggplot(
  df.ts.series,
  aes(x = ts, y = TWD_rolling7,
  group = interaction(year, site.id))) +
  geom_point(aes(color = year)) +
  facet_wrap(~species)

acf(df.ts.series$twd, lag.max = 500)
# clear cycl because high correlation again at 365 days

acf(df.ts.series$at, lag.max = 500)

