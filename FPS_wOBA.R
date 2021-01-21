# FPS and wOBA
library(tidyverse)
library(ggplot2)
library(dplyr)
library(Lahman)
library(ggrepel)

statcast2020 <- read_csv("statcast2020.csv")

# see the relationship between pull % and wOBA
wOBAmodel <- lm(woba ~ f_strike_percent, data= statcast2020)
summary(wOBAmodel)
cor(statcast2020$xwoba, statcast2020$f_strike_percent)

statcast2020 <- statcast2020 %>%
  unite("Name", first_name:last_name)

ggplot(statcast2020, aes(f_strike_percent, woba)) + 
  geom_point() +
  geom_smooth() + 
  xlab("First Pitch Strike %") + 
  ylab("wOBA") + 
  ggtitle("Relationship between First Pitch Strike % and wOBA") + 
  geom_text_repel(data=filter(statcast2020, (f_strike_percent <50 & woba <0.3) | 
                                (f_strike_percent <50 & woba>0.4) |
                                (f_strike_percent > 70 & woba<0.25) |
                                (woba > 0.42) | 
                                (f_strike_percent >74)),
                  aes(f_strike_percent, woba, label=Name))
       
                  
greater70 <- statcast2020 %>%
  filter(f_strike_percent>70)

meanwoba = mean(greater70$woba)
meanwoba

garver <- statcast2020 %>%
  filter(Name == "Mitch_Garver")
garver
