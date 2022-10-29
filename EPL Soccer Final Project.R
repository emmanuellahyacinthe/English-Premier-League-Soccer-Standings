#Emmanuella Hyacinthe
#English Premier League Soccer Standings Final Project 

#libraries
library(tidyverse)
library(dplyr)
library(gapminder)
library(stringr)
library(lubridate)

EPL_Standings <- function(date, season) {
  
  #Making sure the season input is in the correct format,"YYYY/YY", with a length requirement of 7
  
  if(str_length(season) != 7) {
    return
    
  }
  
  #Taking last 2 numbers of the input season years to read correct csv  
  
  year1 <- str_sub(season, 3,4)
  year2 <- str_sub(season,6,7)
  input_season <- str_c(year1,year2)
  
  orig.epl <- read.csv(url(str_c("https://www.football-data.co.uk/mmz4281/",input_season,"/E0.csv")))
  
  #Selecting only needed columns in the dataframe
  
  epl <- orig.epl %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  
  #Formatting input date into a universal format
  
  date_input <- mdy(date)
  
  #Formatting date column in dataframe into a universal format
  
  epl <- epl %>% mutate(Date = dmy(Date))
  
  #Filtering the matches given a certain input date
  
  epl <- filter(epl, Date <= date_input)
  
  #Separating the original dataframe into 2, home team and away team
  
  home_teams <- epl[,c(1:2,4:6)]
  home_teams <- rename(home_teams, TeamGoals = FTHG)
  home_teams <- rename(home_teams, TeamName = HomeTeam)
  home_teams <- rename(home_teams, OpponentGoals = FTAG)
  home_teams <- within(home_teams, Points <- ifelse(TeamGoals > OpponentGoals, 3,
                                                 ifelse(TeamGoals == OpponentGoals, 1,0)))
  home_teams <- within(home_teams, W <- ifelse(TeamGoals > OpponentGoals, 1, 0))
  home_teams <- within(home_teams, L <- ifelse(OpponentGoals > TeamGoals, 1, 0))
  home_teams <- within(home_teams, D <- ifelse(OpponentGoals == TeamGoals, 1, 0))
  
  away_teams <- epl[,c(1,3,4:6)]
  away_teams <- rename(away_teams, TeamGoals = FTAG)
  away_teams <- rename(away_teams, TeamName = AwayTeam)
  away_teams <- rename(away_teams, OpponentGoals = FTHG)
  away_teams <- within(away_teams, Points <- ifelse(TeamGoals > OpponentGoals, 3,
                                                 ifelse(TeamGoals == OpponentGoals, 1,0)))
  away_teams <- within(away_teams, W <- ifelse(TeamGoals > OpponentGoals, 1, 0))
  away_teams <- within(away_teams, L <- ifelse(OpponentGoals > TeamGoals, 1, 0))
  away_teams <- within(away_teams, D <- ifelse(OpponentGoals == TeamGoals, 1, 0))
  
  #Combining the two dataframes together
  
  epl <- bind_rows(home_teams,away_teams)
  
  #Getting the Home and Away Records
  
  home_team_record <- home_teams %>%
    group_by(TeamName) %>%
    summarize(HomeRec = paste(sum(W),"-",sum(L),"-",sum(D)))
  
  away_team_record <- away_teams %>%
    group_by(TeamName) %>%
    summarize(AwayRec = paste(sum(W),"-",sum(L),"-",sum(D)))
  
  #Doing all of the aggregation, grouped by team 
  
  epl <- epl %>%
    group_by(TeamName) %>%
    summarize(Record = paste(sum(W),"-",sum(L),"-",sum(D)),
              GS = sum(TeamGoals),
              Points = sum(Points),
              GA = sum(OpponentGoals),
              MatchesPlayed = n()) %>%
    mutate(HomeRec = home_team_record$HomeRec,
           AwayRec = away_team_record$AwayRec,
           PPM = Points / MatchesPlayed,
           PtPct = (Points / 3) * MatchesPlayed,
           GSM = GS / MatchesPlayed,
           GAM = GA / MatchesPlayed)
  
  #Reordering column headers in dataframe
  
  epl <- epl %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM)
  
  #Sorting the data by the requirements in descending order
  
  epl <- epl %>% arrange(desc(PPM), 'Date', GS, GA)
  
  #Return the output/final dataframe

  return(epl)
}
  
EPL_Standings("04/25/2019", "2018/19")
