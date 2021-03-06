#I will try to make this rmarkdown read like a story for someone to follow
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
#How has the league changed over time, and why has "the strikeout" increased so drastically

#I first decided to check how stats like Batting Average and HRs per AB changed over time
#Tod do this, I summarized some new stats for each year since 1910

old_batting_filtered <- raw_bat %>%
  filter(year_id >= 1910) %>%
  group_by(year_id) %>%
  summarize(total_so = sum(so),
            mean_ba = sum(h)/sum(ab),
            total_ab = sum(ab),
            total_hr = sum(hr),
            so_per_ab = sum(so)/sum(ab),
            hr_per_ab = sum(hr)/sum(ab))

#From the graphic below, it seems as though players are striking out more,
#but also hitting more homeruns (in general)

old_batting_filtered %>%
  ggplot(aes(x = year_id)) +
  geom_line(aes(y = so_per_ab, color = "SO per AB")) +
  geom_line(aes(y = mean_ba, color = "Mean BA")) +
  geom_line(aes(y = hr_per_ab, color = "HR per AB"))

#However, if we just look from 2005 on, this relationship doesn't seem to hold as much

batting_filtered <- raw_bat %>%
  filter(year_id >= 2005) %>%
  group_by(year_id) %>%
  summarize(total_so = sum(so),
            mean_ba = sum(h)/sum(ab),
            total_ab = sum(ab),
            total_hr = sum(hr),
            so_per_ab = sum(so)/sum(ab),
            hr_per_ab = sum(hr)/sum(ab))

batting_filtered %>%
  ggplot(aes(x = year_id)) +
  geom_line(aes(y = so_per_ab, color = "SO per AB")) +
  geom_line(aes(y = hr_per_ab, color = "HR per AB"))

#The same goes for this stretch in the 50s and 60s, where a similar relationship holds

batting_filtered_50s <- raw_bat %>%
  filter(year_id %in% c(1951:1963)) %>%
  group_by(year_id) %>%
  summarize(total_so = sum(so),
            mean_ba = sum(h)/sum(ab),
            total_ab = sum(ab),
            total_hr = sum(hr),
            so_per_ab = sum(so)/sum(ab),
            hr_per_ab = sum(hr)/sum(ab))

batting_filtered_50s %>%
  ggplot(aes(x = year_id)) +
  geom_line(aes(y = so_per_ab, color = "SO per AB")) +
  geom_line(aes(y = hr_per_ab, color = "HR per AB"))

#So that is where I am now
#My next idea is to see the stats for the highest payed players on each team over the years
#My suspicion is that maybe these stats can show me what types of players are most valued over time
#and how that may have been related to the rise in strikeouts

#This gets the highest paid player from each team in each year since 1985 

max_salaries <- raw_salary %>%
  group_by(year_id, team_id) %>%
  filter(salary == max(salary))

#Now I want to get the stats for each of these players
#I need to combine the pitcher and batter data, because salary data includes all positions

#the code below seems a little off, because the sum of raw_bat and raw pitch observations
#is not the same as the total all_players observations, but for now i'll roll with it

all_players <- full_join(raw_bat, raw_pitch, by = c("player_id", "year_id")) %>%
  rename(team_id = team_id.x) %>%
  rename(lg_id = lg_id.x)

#This code below gives me any player who was EVER a top paid player in the league for a team
#and shows the stats for their whole career... I don't know if that's what I want

#I don't know why, but that filter code below seemed to be what made the left_join below work

top_players <- all_players %>%
  filter((player_id %in% max_salaries$player_id) & (year_id %in% max_salaries$year_id))

salaries_and_stats <- left_join(max_salaries, top_players)

salaries_and_stats

#Now we have top paid players for each year and their stats

#Some things I want to investigate -
#1. are top paid players over time pitchers or batters? Has it evolved?
#2. Are the batters getting paid the most becoming better homerun hitters? Or OBP guys? etc
#3. Are there other trends I can find?

#The data seems to be slightly off here, but I think it is good enough to stick with

total_max_salary_players <- salaries_and_stats %>%
  group_by(year_id) %>% 
  summarize(total_players = n()) 

total_max_salary_players %>%
  kable()

#Now I want to see how many are pitchers
#Tod do this, I will filter out players whose pitching stats are NA (here, wins suffices)

total_max_salary_pitchers <- salaries_and_stats %>%
  filter(! is.na(w)) %>%
  group_by(year_id) %>% 
  summarize(total_pitchers = n()) 

total_max_salary_pitchers %>%
  kable()

total_max_salary_pitchers %>%
  ggplot(aes(x = year_id, y = total_pitchers)) + geom_point()

#There might be a slight trend up towards paying pitchers, but not much
#And the uptick may be accounted for by the growing number of teams in the MLB
#Not worth further investigation

#Let's look at the batters and their stats now
#I'm gonna peek at the same stats as I had above, but only for these top players

top_paid_batter_totals <- salaries_and_stats %>%
  filter(is.na(w)) %>%
  filter(!is.na(ab)) %>%
  group_by(year_id) %>%
  summarize(mean_ba = sum(h.x)/sum(ab),
            total_ab = sum(ab),
            total_hr = sum(hr.x),
            so_per_ab = sum(so.x)/sum(ab),
            hr_per_ab = sum(hr.x)/sum(ab))

top_paid_batter_totals %>%
  ggplot(aes(x = year_id)) +
  geom_point(aes(y = so_per_ab, color = "SO per AB")) +
  geom_point(aes(y = mean_ba, color = "Mean BA")) +
  geom_point(aes(y = hr_per_ab, color = "HR per AB"))

#It seems like These top batters are staying pretty consistent with HR per AB
#Their batting averages have fallen decently since '06, but still seem to be
#on par with a historical trend around .280 (since 1985)

#Now I am going to download some data from fangraphs.com on pitch velocity
#I am thinking that maybe pitchers have started to throw harder over the past years
#Or maybe they're throwing more offspeed pitches?

pitch_velocities <- read_csv("pitch_velocities.csv") %>%
  clean_names()

velo_summaries <- pitch_velocities %>%
  filter(! is.na(v_fa)) %>%
  group_by(season) %>%
  summarize(avg_fastball_velo = mean(v_fa), max_fastball_velo = max(v_fa))

velo_summaries %>%
  kable()

velo_summaries %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = avg_fastball_velo, color = "Average Fastball Velo")) +
  geom_point(aes(y = max_fastball_velo, color = "Max Fastball Velo"))

curve_velo_summaries <- pitch_velocities %>%
  filter(! is.na(v_cu)) %>%
  group_by(season) %>%
  summarize(avg_curveball_velo = mean(v_cu), max_curveball_velo = max(v_cu))

curve_velo_summaries %>%
  kable()

curve_velo_summaries %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = avg_curveball_velo, color = "Average Curveball Velo")) +
  geom_point(aes(y = max_curveball_velo, color = "Max Curveball Velo"))

#Now I am going to add some advanced batting stats from fangraphs
#I want to see if fly ball % has increased over the years since 2007
#I'm thinking more people are swinging for the fences, trying to hit homeruns,
#sacrificing their SO percentage for the chance to get a run in one swing

adv_batting_stats <- read_csv("advanced_batting_stats.csv") %>%
  clean_names()

fly_balls <- adv_batting_stats %>%
  filter(! is.na(fb_percent)) %>%
  group_by(season) %>%
  summarize(mean_flyball_percent = mean(fb_percent),
            mean_groundball_per = mean(gb_percent),
            mean_linedrive_per = mean(ld_percent))

fly_balls %>%
  kable

#Not much there, but now I want to see if exit velocity off batted balls has increased
#And if batters have been seeing less fastballs over time?

pitches_seen <- read_csv("pitches_seen.csv") %>%
  clean_names()

fastball_percent_summary <- pitches_seen %>%
  group_by(season) %>%
  summarize(avg_fastball_percentage = mean(fb_percent), avg_curveball_percentage = mean(cb_percent))

fastball_percent_summary %>%
  kable()

#NOW we're getting somewhere

fastball_percent_summary %>%
  ggplot(aes(x = season, y = avg_fastball_percentage)) + 
  geom_point(aes(color = "Fastball Percentage")) +
  geom_smooth(method = "lm")

fastball_percent_summary %>%
  ggplot(aes(x = season, y = avg_curveball_percentage)) + 
  geom_point(aes(color = "Curveball Percentage")) +
  geom_smooth(method = "lm")

#This next set of data I got from Statcast Baseball Savant
#I want to see if average ball off bat velocity has increased year over year

exit_velocity <- read_csv("exit_velocity.csv") %>%
  clean_names()

exit_velocity %>%
  group_by(year) %>%
  summarize(avg_exit_velo = mean(launch_speed))

#There is simply not enough data on this that I can find to see the progression over time
#but the data may be interesting nonetheless, so I will leave it here


#This data from fangraphs shows different swing percentages for players
#The three I look at are % of pitches outside the zone that players swing at
#number of swings / total putches, and times made contact / total swings
#I looked at the mean from year to year in these categories

swing_stats <- read_csv("swing_stats.csv") %>%
  clean_names()

swing_summaries <- swing_stats %>%
  group_by(season) %>%
  summarize(swing_outside_zone = mean(o_swing_percent),
            contact_percent = mean(contact_percent),
            swing_percent = mean(swing_percent)) 

swing_summaries %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = swing_outside_zone, color = ""))

swing_summaries %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = contact_percent, color = ""))

swing_summaries %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = swing_percent, color = ""))

#Now I am going to make my app
#To do that most efficiently, I will make an RDS file to move to my app
#I will use the old_batting_filtered data from above
#I will also use the pitch_velocities data
#I will also use the fastball_percent_summary data

write_rds(old_batting_filtered, path = "mlb_strikeouts_app/final_app_old_data")
write_rds(pitch_velocities, path = "mlb_strikeouts_app/final_app_velo_data")
write_rds(fastball_percent_summary, path = "mlb_strikeouts_app/final_app_pitch_type_data")



    
    
    
    
    
    
    
