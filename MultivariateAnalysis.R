library(tidyverse)
library(naniar)
#install.packages("kableExtra")
library(kableExtra)
library(forcats)
library(png)


# top 5 countries = 5 countries with the highest number of world cup wins:
# Brazil, Germany, Italy, Argentina, Uruguay

# read in data
WorldCupMatchesCleaned <- read_csv("data/WorldCupMatchesCleaned.csv")
WorldCupsCleaned <- read_csv("data/WorldCupsCleaned.csv")
Cups_Matches_joined <- read_csv("data/Cups_Matches_joined.csv")
goals_bycountry_perWC <- read_csv("data/goals_bycountry_perWC.csv")

# read in plots


# analysis

# Years vs Attendance for World Cup

years_atten_plot <- WorldCupsCleaned |> ggplot(aes(x = Year, y = Attendance)) + 
  geom_line(color = "magenta") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total Attendance at Each World Cup Since 1930", 
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

# stages_attendance_df <- stages_attendance_df |> 
#   mutate(Stage = fct_relevel(Stage, levels = c("Early round", 
#                                                "Quarter-finals",
#                                                "Match for third place",
#                                                "Semi-finals",
#                                                "Final")))


stages_atten_plot <- stages_attendance_df |> ggplot(aes(x=Stage, y = Match_Attendance, fill = Stage)) +
  geom_boxplot(alpha = 0.7) + theme_minimal() + scale_color_gradient() +
  labs(title = "Distribution of Match Attendance by Round of the World Cup",
       subtitle = "All rounds before the Quarter-finals are considered an early round",
       x = "Round",
       y = "Attendance") +
  scale_fill_manual(name = "Stage", 
                    values = c("skyblue", "deepskyblue", "deepskyblue2", "deepskyblue3", "deepskyblue4")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold")) 

stages_atten_plot
ggsave("plots/stages_atten_plot.png")



# Density plot of the distribution of match attendance for my fav 4 countries to watch
match_atten_dist_favcountry_plot <- binded_team_goals |> 
filter(Team_Name %in% c("Brazil", "Germany", "USA", "Argentina")) |> 
  ggplot(aes(x = Match_Attendance, fill = Team_Name, color = Team_Name)) + 
  geom_density(alpha = 0.3) + 
  labs(title = "Distribution of Match Attendance for Selected Teams",
       x = "Match Attendance") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold")) 

match_atten_dist_favcountry_plot

ggsave("plots/match_atten_dist_favcountry_plot.png")

# Density plot of the distribution of match attendance for top 5 countries to watch
match_atten_dist_top5_plot <- binded_team_goals |> 
  filter(Team_Name %in% c("Brazil", "Germany", "Italy", "Argentina", "Uruguay")) |> 
  ggplot(aes(x = Match_Attendance, fill = Team_Name, color = Team_Name)) + 
  geom_density(alpha = 0.3) + 
  labs(title = "Distribution of Match Attendance for Top Five Teams",
       subtitle = "These are the five teams that have the highest number of World Cup Wins",
       x = "Match Attendance") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold")) 

match_atten_dist_top5_plot

ggsave("plots/match_atten_dist_top5_plot.png")


# boxplot of team goals for the match vs match attendance (UNSURE ABOUT THE CONCLUSION)
match_atten_game_goals_plot <- binded_team_goals |> 
  ggplot(aes(x = Match_Attendance, y = factor(Team_Goals), fill = factor(Team_Goals),
             color = factor(Team_Goals))) + 
  geom_boxplot(alpha = 0.5) +
  labs(title = "Distribution of Match Attendance vs Goals Scored in the Match",
       x = "Match Attendance",
       y = "Team Goals in the Match") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold")) 



match_atten_game_goals_plot


# Box plot of the distribution of match attendance for my fav 4 countries to watch
binded_team_goals |> 
  filter(Team_Name %in% c("Brazil", "Germany", "USA", "Argentina")) |> 
  ggplot(aes(x = Match_Attendance,  fill = Team_Name)) + 
  geom_boxplot(alpha = 0.7) + 
  labs(title = "Distribution of Match Attendance for Selected Teams",
       x = "Match Attendance") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold")) 


# scatterplot of wc attendance vs qualified teams
WorldCupsCleaned |> ggplot(aes(x= QualifiedTeams, y = Attendance)) + 
  geom_line() + geom_point()



# Goals Scored: 

# Using function to create plots with the 5 highest scoring countries in each world cup
# from 1994 - 2014
year_vec <-  c(1994, 1998, 2002, 2006, 2010, 2014)

for(year in year_vec) {
year_histogram_func <- function(year) {
  goals_bycountry_perWC |> 
  filter(Year == {{year}})  |> 
    arrange(desc(total_goals)) |> 
    slice_max(total_goals, n=5) |> 
  ggplot(aes(x= Team_Name, y = total_goals, fill = Team_Name)) + 
    geom_col() +
    labs(title = "Top 5 Teams with the Highest Number of Goals per each Selected World Cups",
         subtitle = "For the World Cups Between 1994 - 2014",
         x = "Team (Country)",
         y = "Total Goals") +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
          plot.subtitle = element_text(hjust = 0.5, size = 10), 
          axis.title.x = element_text(hjust = 0.5, size = 10, face = "bold"),
          axis.title.y = element_text(hjust = 0.5, size = 10, face = "bold")) +
    theme_linedraw()
   
}
plots <- list()
plots[[year]] <- year_histogram_func(year)

ggsave(plots[[year]],
       file = paste0("plots/countrygoals_recentWC_top", year, "_plot.png"))
}
  
# use patchwork to put the plots side by side
countrygoals_recentWC_top1994_plot <- readPNG("plots/countrygoals_recentWC_top1994_plot.png", package = "png")
countrygoals_recentWC_top1994_plot


countrygoals_recentWC_alltimetop5_plot <- goals_bycountry_perWC |> 
  filter(Year %in% c(1994, 1998, 2002, 2006, 2010, 2014)) |> 
  filter(Team_Name %in% c( "Brazil", "Germany", "Italy", "Argentina", "Uruguay")) |> 
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

countrygoals_recentWC_alltimetop5_plot
ggsave("plots/countrygoals_recentWC_alltimetop5_plot.png")
