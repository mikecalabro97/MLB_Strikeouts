#I will try to make this scratch doc read like a story for someone to follow
#Hopefully this will make my comments informative and keep me organized

#READ THE COMMENTS, THEN COMMAND-ENTER THE CODE UNDERNEATH TO FOLLOW ALONG

#These are libraries which contain functions I suspect I need
#They do not need to be in this scratch file, but they're here for reference

library(knitr)
library(janitor)
library(readr)
library(readxl)
library(tidyverse)

#I began by downloading three files - naming them raw_bat, pitch, and salary
#These were Downloaded from the Lahman Baseball Database
#Sean Lahman has collected statistics from the MLB from 1871(!) up until present day
#The info goes up to 2017 for bat and pitch, and 2016 for salaries

raw_bat <- read_excel("Lahman_Batting.xlsx") %>%
  clean_names()

raw_pitch <- read_excel("Lahman_pitch.xlsx") %>%
  clean_names()

raw_salary <- read_csv("Lahman_Salaries.csv") %>%
  clean_names()

#Now, I am going to create a graphic which visualizes the statistic I read online that piqued my interest
#using the batting data, I will show total strikeouts in the MLB for every year since 2005
#I will do this using a pretty standard group_by, summarize combo


initial_strikeout_data <- raw_bat %>%
  filter(year_id >= 2005) %>%
  group_by(year_id) %>%
  summarize(total_strikeouts = sum(so))

#Here is a table of the data... Every year since 2007 has been a new MLB record!

initial_strikeout_data %>%
  kable()

#And here is a scatterplot, using ggplot2

initial_strikeout_data %>%
  ggplot(aes(x = year_id, y = total_strikeouts)) + geom_line()

# 30,000 strikeouts in '05 to 40,000 seems like a huge jump! A ~30% increase in a dozen years!

#Looking through older data, I realized there were less total At bats back in the day
#due to shorter seasons and fewer teams in the league. That's why this graphic is misleading

old_strikeout_data <- raw_bat %>%
  filter(year_id >= 1910) %>%
  group_by(year_id) %>%
  summarize(total_strikeouts = sum(so))

old_strikeout_data %>%
  ggplot(aes(x = year_id, y = total_strikeouts)) + geom_line()

#It became apparent that the statistic of interest was ACTUALLY strikeouts per at bat
#That's how you can tell if people are truly stiking out significantly more

strikeout_perAB_data <- raw_bat %>%
  filter(year_id >= 2005) %>%
  group_by(year_id) %>%
  summarize(so_per_ab = sum(so)/sum(ab))

#Turns out this is increasing quite significantly also! ALSO up ~30% since 2005

strikeout_perAB_data %>%
  kable()

strikeout_perAB_data %>%
  ggplot(aes(x = year_id, y = so_per_ab)) + geom_line()

#This makes sense - there haven't been more teams added to the league, or more games
#added to the schedule since 2005, so total SO and average SO should align

#What I wasn't expecting was what how SO per AB would look since 1910

old_strikeout_perAB_data <- raw_bat %>%
  filter(year_id >= 1910) %>%
  group_by(year_id) %>%
  summarize(so_per_ab = sum(so)/sum(ab))

old_strikeout_perAB_data %>%
  ggplot(aes(x = year_id, y = so_per_ab)) + geom_line()

#Now this intrigues me.. and is what I want to explore througout the rest of this project
#How has the league changed over time, and why has the strikeout increased so drastically




