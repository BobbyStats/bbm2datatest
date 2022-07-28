install.packages("tidyverse", type = "binary")
install.packages("ggrepel", type = "binary")
install.packages("ggimage", type = "binary")
install.packages("nflfastR", type = "binary")
install.packages("nflreadr", type = "binary")
install.packages("ggplot2")
install.packages("readr")
install.packages("gt")
install.packages("rlang")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggplot2)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(readr)
library(gt)

#load regular season
regpbp<-load_pbp(2021)%>%
  filter(week<19)

#load roster data
roster2021<-load_rosters(2021)
roster2020<-load_rosters(2020)
#adding henry ruggs data back into 2021 df
ruggsdf<-roster2020%>%
  filter(gsis_id=="00-0036357"|gsis_id=="00-0036078")
rosternew2021<-rbind(roster2021,ruggsdf)
rosternew2021[4840:4841,1]<-2021 #column1 is season
rosternew2021[4840,24]<-2 #column2 is years_exp
#adding Antonio Brown team, Tyrell Williams team,Jalen Hurd team
rosternew2021[4654,2]<-"DET"
rosternew2021[4679,2]<-"TB"
rosternew2021[4539,2]<-"SF"
#changing David Johnson, Michael Thomas, AJ Green,Lamar Jackson,Michael Carter,Josh ALlen,David Moore duplicates
rosternew2021[4363,7]<-"David Johnson2"
rosternew2021[513,7]<-"Michael Thomas2"
rosternew2021[593,7]<-"A.J. Green2"
rosternew2021[421,7]<-"Lamar Jackson2"
rosternew2021[2036,7]<-"Michael Carter2"
rosternew2021[3367,7]<-"Josh Allen2"
rosternew2021[1199,7]<-"Josh Allen3"
rosternew2021[617,7]<-"David Moore2"
rosternew2021[2508,7]<-"Ryan Griffin2"
roster2021<-rosternew2021 #change back to roster2021 name

#loadBBM2 dataset
BBM2ds<- read.csv("https://assets.underdogfantasy.com/underblog/BBM_II_Data_Dump_Regular_Season_01312022.csv", header = TRUE)
names(BBM2ds)
#filter out draft with incomplete data
BBM2ds<-BBM2ds%>%
  filter(!draft_id %in% c("27bbd807-bca3-4486-b8aa-7101fda4c9eb"))
#merging BBM2 data with roster data
BBM2dfwteams<-BBM2ds%>%
  left_join(roster2021, by = c('player_name'="full_name"))
#weekly data
#load weekly stats
weekly <- calculate_player_stats(regpbp, weekly = TRUE)
#edit Josh allen name week 3
weekly[3372,2]<-"J.Allen" #column1 is name
#add underdog scoring column
weekly<-weekly%>%
  mutate(
    underdogscoring= (passing_yards*0.04)+(rushing_yards*0.1)+(receiving_yards*0.1)+
      (passing_tds*4)+(rushing_tds*6)+(receiving_tds*6)+(passing_2pt_conversions*2)+
      (rushing_2pt_conversions*2)+(receiving_2pt_conversions*2)+(special_teams_tds*6)-
      (interceptions*1)-(sack_fumbles_lost*2)-(rushing_fumbles_lost*2)-(receiving_fumbles_lost*2)+(receptions*0.5))

weeklybbmdf<-weekly%>%
  select(player_id,player_name,week,underdogscoring)
weeklybbmdf1<-weeklybbmdf%>%
  filter(week==1)%>%
  mutate(week1scoring=underdogscoring)%>%
  select(player_id,week1scoring)
#combine the BBM2 df with the roster df to get position.
BBM2dsv4<-BBM2ds%>%
  left_join(roster2021, by = c('player_name'="full_name"))
BBM2dsv4<-BBM2dsv4%>%
  select(tournament_entry_id,gsis_id,position,overall_pick_number)
BBM2dsv4<-BBM2dsv4%>%
  left_join(weeklybbmdf1,by=c("gsis_id"="player_id"))
#create additional columns to determine the scoring rank for each position
BBM2roundscombineddfv2<-BBM2dsv4%>%
  group_by(tournament_entry_id,position)%>%
  arrange(overall_pick_number,.by_group=TRUE)%>%
  mutate(
    week1posrank=rank(-week1scoring,na.last=TRUE,ties.method="first"))
#Create flex option to identify players to potentially compare for flex output
BBM2roundscombineddfv2<-BBM2roundscombineddfv2%>%
  mutate(
    flex_optionW1=
      if_else((position=="RB"&week1posrank==3)|(position=="WR"&week1posrank==4)|
                (position=="TE"&week1posrank==2),1,0))
#Create a column that shows if a player scored or not, and how many points they scored if so
teamweeklyscoringtest<-BBM2roundscombineddfv2%>%
  group_by(tournament_entry_id)%>%
  mutate(
    points_scoredW1=
      if_else((position=="QB"&week1posrank==1)|(position=="TE"&week1posrank==1)|
                (position=="RB"&week1posrank<3)|(position=="WR"&week1posrank<4),
              week1scoring,0))

tibble(teamweeklyscoringtest)
