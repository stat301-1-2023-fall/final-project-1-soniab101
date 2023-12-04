library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)
library(forcats)
library(RColorBrewer)


# read in data
WorldCupMatchesCleaned <- read_csv("data/WorldCupMatchesCleaned.csv")
WorldCupsCleaned <- read_csv("data/WorldCupsCleaned.csv")
Cups_Matches_joined <- read_csv("data/Cups_Matches_joined.csv")
goals_bycountry_perWC <- read_csv("data/goals_bycountry_perWC.csv")


# analysis

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

#save goals_bycountry_perWC to data folder
write_csv(goals_bycountry_perWC, "data/goals_bycountry_perWC.csv")

# Top scoring countries total goals for the 6 most recent world cups
countrygoals_recentWC_plot <- goals_bycountry_perWC |> 
  filter(Year %in% c(1994, 1998, 2002, 2006, 2010, 2014)) |> 
  filter(Team_Name %in% c("Argentina", "Brazil", "Germany", "Italy")) |> 
  ggplot(aes(x=Team_Name, y = total_goals, fill = Team_Name)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~factor(Year)) +
  labs(title = "Each Country's Total Goals per World Cup",
       subtitle = "For the World Cups Between 1994 - 2014",
       x = "Country",
       y = "Total Goals") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold")) +
  theme_linedraw()

countrygoals_recentWC_plot
ggsave("plots/countrygoals_recentWC_plot.png")



goals_bycountry_plot <- goals_bycountry_perWC |> slice_max(total_goals, n=6) |> 
  ggplot(aes(x=Team_Name, y=total_goals)) + geom_boxplot()

goals_year_bycountry_plot <- binded_team_goals |> 
  summarize(total_goals = sum(Team_Goals)) |> filter("Team_Name" == "Argentina") |> 
  ggplot(aes(x=Year, y= Team_Goals)) +
  geom_line() 

goals_year_bycountry_plot


# Total attendance by WC country host

# change countries that have hosted multiple times to be coded slightly differently
WorldCupsCleaned1<- WorldCupsCleaned
WorldCupsCleaned1$Country[20] <- 'Brazil1'
WorldCupsCleaned1$Country[14] <- 'Italy1'
WorldCupsCleaned1$Country[16] <- 'France1'
WorldCupsCleaned1$Country[18] <- 'Germany1'
WorldCupsCleaned1$Country[13] <- 'Mexico1'

country1 <- WorldCupsCleaned$Country
WorldCupsCleaned1$Country1 <- country1

WC_country_attendance <- WorldCupsCleaned1 |> ggplot(aes(x= Country, y= Attendance, fill = Country1)) + 
  geom_col() + 
  geom_text(aes(label = Year), vjust = 1.5, colour = "white") +
  labs(title = "Total World Cup Attendance By Host Country",
       subtitle = "Countries that have hosted twice are adjacent by year they hosted",
       x = "Host Country",
       y = "Total Attendance") + 
  theme(axis.text.x = element_text(angle = 45)) 

WC_country_attendance

ggsave("plots/WC_country_attendance.png")



# Investigating the match attendance at different stages of the world cup
stages_attendance_df <- Cups_Matches_joined 
stages_attendance_df <- stages_attendance_df |> 
  mutate(Stage = if_else(Stage %in% c("Quarter-finals", 
                                      "Semi-finals",
                                      "Match for third place",
                                      "Final"), Stage, "Early round"
                         ))
    
stages_attendance_df <- stages_attendance_df |> 
  mutate(Stage = factor(Stage,
                        levels = c("Early round", 
                                   "Quarter-finals",
                                   "Match for third place",
                                   "Semi-finals",
                                   "Final")))

stages_attendance_df <- stages_attendance_df |> 
  mutate(Stage = fct_relevel(Stage, levels = c("Early round", 
                                               "Quarter-finals",
                                               "Match for third place",
                                               "Semi-finals",
                                               "Final")))


stages_attendance_df |> ggplot(aes(x=Stage, y = Match_Attendance, fill = Stage)) +
  geom_boxplot() + theme_minimal() + scale_color_gradient() +
  labs(title = "Distribution of Match Attendance by Round of the World Cup",
       subtitle = "All rounds before the Quarter-finals are considered an early round",
       x = "Round",
       y = "Attendance") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold")) 


