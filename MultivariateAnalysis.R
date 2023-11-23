library(tidyverse)
library(naniar)
install.packages("kableExtra")
library(kableExtra)

WorldCupMatchesCleaned <- read_csv("data/WorldCupMatchesCleaned.csv")
WorldCupsCleaned <- read_csv("data/WorldCupsCleaned.csv")
Cups_Matches_joined <- read_csv("data/Cups_Matches_joined.csv")

WorldCupsCleaned |> ggplot(aes(x = Year, y = Attendance)) + geom_point()
