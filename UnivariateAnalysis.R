library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)


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
