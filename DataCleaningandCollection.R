library(tidyverse)
library(naniar)
library(sjlabelled)
library(sjmisc)

# cleaning the data:
WorldCupsCleaned <- mutate_if(WorldCups,
                              is.character,
                              stringr::str_replace_all, pattern = "Germany FR", replacement = "Germany")

WorldCupMatchesCleaned <- mutate_if(WorldCupMatches,
                                    is.character,
                                    stringr::str_replace_all, pattern = "Germany FR", replacement = "Germany")



# creating a codebook for the data

get_label(WorldCupsCleaned)
get_label(WorldCupMatchesCleaned)


wc_data_codebook <- tibble(
  variables = c("Year", "Country", "Winner","Runners-Up", "Third", "Fourth",
  "GoalsScored", "QualifiedTeams", "MatchesPlayed", "Attendance"),
  descriptions = c("Year of the worldcup",
                   "Country of the worldcup",
                   "Team who won the worldcup",
                   "Team who was the second place",
                   "Team who was the third place",
                   "Team who was the fourth place",
                   "Total goals scored in the worldcup",
                   "Total participating teams",
                   "Total matches played in the cup",
                   "Total attendance of the worldcup"
                   ))



matches_data_codebook <- tibble(
  variables = c("Year", "Datetime", "Stage", "Stadium", "City", "Home Team Name", 
  "Home Team Goals", "Away Team Goals", "Away Team Name", "Win conditions", 
  "Attendance", "Half-time Home Goals" , "Half-time Away Goals", "Referee",
  "Assistant 1", "Assistant 2", "RoundID", "MatchID", "Home Team Initials", 
  "Away Team Initials"),
  descriptions = c("Year match was played",
                   "The date and time the match was played",
                   "Stage at which the match was played",
                   "Stadium name where the match was held",
                   "The city name, where the match was played",
                   "Home team country name",
                   "Total goals scored by the home team by the end of the match",
                   "Total goals scored by the away team by the end of the match",
                   "Away team country name",
                   "Special win condition (if any)",
                   "Total crowd present at the satdium",
                   "Goals scored by the home team until half time",
                   "Goals scored by the away team until half time",
                   "Name of the first refree",
                   "Name of the first assistant referee (linesman)",
                   "Name of the second assistant referee (linesman)",
                   "Unique ID of the Round",
                   "Unique ID of the match",
                   "Home team country's three letter initials",
                   "Away team country's three letter initials"))
