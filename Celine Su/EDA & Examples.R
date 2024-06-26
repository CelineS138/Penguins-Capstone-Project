
# Load data set from Hockey Reference 

Stats_AD23.24 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD23-24.csv")
Stats_AD22.23 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD22-23.csv")
Stats_AD21.22 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD21-22.csv")
Stats_AD20.21 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD20-21.csv")
Stats_AD19.20 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD19-20.csv")
Stats_AD18.19 <- read.csv("~/Desktop/Penguins Capstone Project/Stats_AD18-19.csv")

combined_df <- rbind(Stats_AD23.24, Stats_AD22.23, Stats_AD21.22, Stats_AD20.21, Stats_AD19.20, Stats_AD18.19)


# EDAs
library(ggplot2)
library(tidyverse)

combined_df |> 
  select("Player", "Pos", "CF.", "FF.", "oiSH.", "oiSV.", "oZS.", "dZS.", "Year") |> 
  filter(Player == "Jake Guentzel") |> 
  pivot_longer(cols = c(CF., FF., oiSH., oiSV., oZS., dZS.),
               names_to = "Metric",
               values_to = "Value") |> 
  ggplot(aes(x = Year, y = Value, color = Metric)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(title = "Metrics Over the Years for Evgeni Malkin",
       x = "Year",
       y = "Metric Value",
       color = "Metric") +
  theme_minimal()



