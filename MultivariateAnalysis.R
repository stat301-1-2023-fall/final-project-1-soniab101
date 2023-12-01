library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)

WorldCupMatchesCleaned <- read_csv("data/WorldCupMatchesCleaned.csv")
WorldCupsCleaned <- read_csv("data/WorldCupsCleaned.csv")
Cups_Matches_joined <- read_csv("data/Cups_Matches_joined.csv")


# Years vs Attendance for World Cup

years_atten_plot <- WorldCupsCleaned |> ggplot(aes(x = Year, y = Attendance)) + 
  geom_line(color = "magenta") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total Attendance at Each World Cup Over Since 1930", 
       x = "Year", y = "Total Attendance", caption = "Source: Fifa World Cup Archive")

ggsave("plots/years_atten.png")

years_atten_plot



# Bivariate analysis of GoalsScored and Year vars in WorldCupsCleaned dataset
years_goals_scored_plot <- WorldCupsCleaned |> ggplot(aes(x=Year,y=GoalsScored)) +
  geom_line(color = "darkorchid") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total Goals Scored in the World Cup over the Years", x = "Year", y = "Total Goals", caption = "Source: Fifa World Cup Archive")

ggsave("plots/years_goals_scored.png")

years_goals_scored_plot

# country vs attendance - need to fix
country_atten_plot <- Cups_Matches_joined |> filter(!is.na(Match_Attendance) & !is.na(Year)) |> 
ggplot(aes(x= Country, y = Match_Attendance)) +
  geom_boxplot() + 
  coord_flip() + 
  labs(title = "Distribution of Match Attendance by World Cup Host Country", 
       x = "Year", y = "Total Attendance", caption = "Source: Fifa World Cup Archive")

ggsave("plots/country_atten.png")

country_atten_plot


# Creating a yr, home, home pts dataset

data1 <- Cups_Matches_joined |>  select(-c("Away_Team_Goals", "Away_Team_Name"))
data1 <- data1 |> rename("Team_Goals" = "Home_Team_Goals", "Team_Name" = "Home_Team_Name")

data2 <- Cups_Matches_joined |>  select(-c("Home_Team_Goals", "Home_Team_Name"))
data2 <- data2 |> rename("Team_Goals" = "Away_Team_Goals", "Team_Name" = "Away_Team_Name")

binded_team_goals <- rbind(data1, data2)
binded_team_goals <- binded_team_goals |> arrange(Year)

goals_bycountry_perWC <- binded_team_goals |> group_by(Year, Team_Name) |> 
  summarize(total_goals = sum(Team_Goals, na.rm = TRUE))

goals_bycountry_perWC |> slice_max(total_goals, n=6)



goals_bycountry_plot <- goals_bycountry_perWC |> slice_max(total_goals, n=6) |> 
  ggplot(aes(x=Team_Name, y=total_goals)) + geom_boxplot()

goals_year_bycountry_plot <- binded_team_goals |> summarize(total_goals = sum(Team_Goals)) |> filter("Team_Name" == "Argentina") |> 
  ggplot(aes(x=Year, y= Team_Goals)) +
  geom_line() 

goals_year_bycountry_plot


