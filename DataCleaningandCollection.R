library(tidyverse)
library(naniar)
library(sjlabelled)
library(sjmisc)
install.packages("kableExtra")
library(kableExtra)
library(lubridate)

# cleaning the data:

glimpse(WorldCupMatches)
glimpse(WorldCups)

#recoding the Germany value
WorldCupsCleaned <- mutate_if(WorldCups,
                              is.character,
                              stringr::str_replace_all, pattern = "Germany FR", replacement = "Germany")

WorldCupMatchesCleaned <- mutate_if(WorldCupMatches,
                                    is.character,
                                    stringr::str_replace_all, pattern = "Germany FR", replacement = "Germany")

WorldCupsCleaned  |>  summarize(n=n(),.by=Winner) 

#making datetime var into a date object
WorldCupMatchesCleaned <- WorldCupMatchesCleaned  %>%
  mutate(Datetime = dmy_hm(Datetime))



# merging the World Cups Matches and World Cups datasets
Cups_Matches_joined <- WorldCupMatchesCleaned |> left_join(WorldCupsCleaned, by = "Year")



# renaming the var names that have spaces
Cups_Matches_joined <- Cups_Matches_joined |> 
  rename("Home_Team_Name" = `Home Team Name`,
         "Away_Team_Name" = `Away Team Name`,
         "Win_Conditions" = `Win conditions`,
         "Half-time_Home_Goals" = `Half-time Home Goals`,
         "Half-time_Away_Goals" = `Half-time Away Goals`,
         "Home_Team_Goals" = `Home Team Goals`,
         "Away_Team_Goals" = `Away Team Goals`,
         "Assistant_1" = `Assistant 1`,
         "Assistant_2" = `Assistant 2`,
         "Home_Team_Initials" = `Home Team Initials`,
         "Away_Team_Initials" = `Away Team Initials`,
         "Match_Attendance" = `Attendance.x`,
         "Total_Attendance_of_WC" = `Attendance.y`)

WorldCupMatchesCleaned <- WorldCupMatchesCleaned |> 
  rename("Home_Team_Name" = `Home Team Name`,
         "Away_Team_Name" = `Away Team Name`,
         "Win_Conditions" = `Win conditions`,
         "Half-time_Home_Goals" = `Half-time Home Goals`,
         "Half-time_Away_Goals" = `Half-time Away Goals`,
         "Home_Team_Goals" = `Home Team Goals`,
         "Away_Team_Goals" = `Away Team Goals`,
         "Assistant_1" = `Assistant 1`,
         "Assistant_2" = `Assistant 2`,
         "Home_Team_Initials" = `Home Team Initials`,
         "Away_Team_Initials" = `Away Team Initials`)

Cups_Matches_joined <- Cups_Matches_joined |> 
  rename("Match_Attendance" = `Attendance.x`,
"Total_Attendance_of_WC" = `Attendance.y`)

Cups_Matches_joined <- subset(Cups_Matches_joined, select = -c(...1))
WorldCupsCleaned <- subset(WorldCupsCleaned, select = -c(...1,...2))
WorldCupMatchesCleaned <- subset(WorldCupMatchesCleaned, select = -c(...1,...2))

# fix coding of attendance variable
WorldCupsCleaned <- WorldCupsCleaned |> 
  mutate(Attendance = c(590549, 363000  ,375700,1045246, 768607,819810,893172,  
                        1563135, 1603975, 1865753, 1545791, 2109723, 2394031,
                        2516215,3587538, 2785100 ,2705197, 3359439, 3178856, 3386810))

WorldCupMatchesCleaned <- WorldCupMatchesCleaned[-c(853:4572), ]

write.csv(WorldCupsCleaned, "data/WorldCupsCleaned.csv")
write.csv(WorldCupMatchesCleaned, "data/WorldCupMatchesCleaned.csv")
write.csv(Cups_Matches_joined, "data/Cups_Matches_joined.csv")


# investigating the data
glimpse(Cups_Matches_joined)
str(Cups_Matches_joined)
sum<- summary(Cups_Matches_joined)



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


wc_codebook <- wc_data_codebook %>%
  kbl() %>%
  kable_styling()

wc_codebook

wc_match_codebook <- matches_data_codebook %>%
  kbl() %>%
  kable_styling()

wc_match_codebook
