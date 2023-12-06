library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)

# read in data
WorldCupMatchesCleaned <- read_csv("data/WorldCupMatchesCleaned.csv")
WorldCupsCleaned <- read_csv("data/WorldCupsCleaned.csv")
Cups_Matches_joined <- read_csv("data/Cups_Matches_joined.csv")
goals_bycountry_perWC <- read_csv("data/goals_bycountry_perWC.csv")


# creating goals_bycountry_perWC
data1 <- Cups_Matches_joined |>  select(-c("Away_Team_Goals", "Away_Team_Name"))
data1 <- data1 |> rename("Team_Goals" = "Home_Team_Goals", "Team_Name" = "Home_Team_Name")

data2 <- Cups_Matches_joined |>  select(-c("Home_Team_Goals", "Home_Team_Name"))
data2 <- data2 |> rename("Team_Goals" = "Away_Team_Goals", "Team_Name" = "Away_Team_Name")

binded_team_goals <- rbind(data1, data2)
binded_team_goals <- binded_team_goals |> arrange(Year)

goals_bycountry_perWC <- binded_team_goals |> group_by(Year, Team_Name) |> 
  summarize(total_goals = sum(Team_Goals, na.rm = TRUE))



# Univariate analysis of Winner var in WorldCupsCleaned dataset
winner_plot <- WorldCupsCleaned |> 
  ggplot(aes(x= Winner)) + 
  geom_bar(fill = "deepskyblue3") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Distribution of World Cup Wins by Country", x = "Country", y = "Total Wins",
       caption = "Source: Fifa World Cup Archive")
ggsave("plots/winner_plot.png")

#Univariate analysis of GoalsScored var in WorldCupsCleaned dataset
totalgoals_plot<- WorldCupsCleaned |> 
  ggplot(aes(x= GoalsScored)) + 
  geom_histogram(color = "white", fill = "coral1", bins = 10) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Distribution of Total Goals Scored in Each World Cup", 
       x = "Number of Goals", y = "Frequency",
       caption = "Source: Fifa World Cup Archive")

ggsave("plots/totalgoals_plot.png")


# Univar analysis of match attendance var
match_atten_dist_plot <- Cups_Matches_joined |> 
  ggplot(aes(x = Match_Attendance)) + geom_density()

match_atten_dist_plot


