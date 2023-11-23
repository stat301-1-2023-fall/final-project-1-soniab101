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