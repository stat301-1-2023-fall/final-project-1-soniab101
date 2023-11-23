library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)


# Univariate analysis of Winner var in WorldCupsCleaned dataset
WorldCupsCleaned |> 
  ggplot(aes(x= Winner)) + 
  geom_bar(fill = "deepskyblue3") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Distribution of World Cup Wins by Country", x = "Country", y = "Total Wins",
       caption = "Source: Fifa World Cup Archive")

  
  
# Bivariate analysis of GoalsScored and Year vars in WorldCupsCleaned dataset
WorldCupsCleaned |> ggplot(aes(x=Year,y=GoalsScored)) +
  geom_line(color = "darkorchid") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total Goals Scored in the World Cup over the Years", x = "Year", y = "Total Goals", caption = "Source: Fifa World Cup Archive")