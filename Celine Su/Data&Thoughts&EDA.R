library(tidyverse)
library(broom)

## Thoughts on measuring consistency

# 1.  Derive a consistency variable (consistency score) by calculating the differences in performance statistics across seasons; apply different weights by position.

# 2.  Weight the metrics differently based on importance for each position

# 3.  Consistency score will have more consistent players near 0 & inconsistent players much greater than 0

# 4.  Performance metrics must be `per game` or `per 60 minutes` to make it consistent and unbiased among player

# Metrics to measure consistency among forwards:
  - Pts/gp

-   +/-
  
  -   Offensive point shares

-   Shooting percentage / shots per game

-   GF/60 (EV, PP, SH)

-   Takeaways? Giveaways?
  
  -   Corsi for %? Fenwick for %?
  
  Metrics to measure consistency among defensemen:
  
  -   +/-
  
  -   Defensive point shares

-   GA/60 (EV, PP, SH)

-   Blocks and hits

-   Takeaways? Giveaways?
  
  Metrics to measure consistency among goalies:
  
  -   Save percentage

-   Goalies against average

-   Quality start percentage

-   Goalie point shares


## Data

### [Hockey-reference.com](http://hockey-reference.com/)

### Basic Statistics

basic_2023_24 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_23_24.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2023)

basic_2022_23 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_22_23.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2022)

basic_2021_22 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_21_22.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2021)

basic_2020_21 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_20_21.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2020)

basic_2019_20 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_19_20.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2019)

basic_2018_19 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_18_19.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2018)

basic_2017_18 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_17_18.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2017)

basic_2016_17 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_16_17.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2016)



misc_df <- rbind(misc_2023_24, misc_2022_23, misc_2021_22, misc_2020_21, misc_2019_20,
                 misc_2018_19, misc_2017_18, misc_2016_17)

misc_df <- misc_df |> 
  mutate(Pos = str_extract(Pos, "[A-Z]"))

num_seasons <- misc_df |> 
  filter(Pos != "G") |>
  group_by(Player) |> 
  summarize(num_seasons = n()) |> 
  ungroup()
  
player_pos <- misc_df |> 
  filter(Pos != "G") |> 
  group_by(Player) |> 
  summarize(Pos = ifelse(Pos %in% c("C", "L", "R", "W", "F"), "F", "D")) |> 
  unique() 


long_misc_df <- misc_df |> 
  filter(Pos != "G", GP >= 20) |> 
  pivot_wider(id_cols = c(Player), 
              values_from = c(`G/GP`, `A/GP`, `PTS/GP`, `GC/GP`, `S/GP`, `OPS`, `DPS`, `PS`), 
              names_from = start_year)

long_misc_df <- long_misc_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)
# mutate(across(`G/GP_2023`:`PS_2016`, ~replace_na(.x, 0)))



### Advanced Statistics

advanced_2023_24 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_23_24.csv") |> 
  mutate(start_year = 2023)

advanced_2022_23 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_22_23.csv") |> 
  mutate(start_year = 2022)

advanced_2021_22 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_21_22.csv") |> 
  mutate(start_year = 2021)

advanced_2020_21 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_20_21.csv") |> 
  mutate(start_year = 2020)

advanced_2019_20 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_19_20.csv") |> 
  mutate(start_year = 2019)

advanced_2018_19 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_18_19.csv") |> 
  mutate(start_year = 2018)

advanced_2017_18 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_17_18.csv") |> 
  mutate(start_year = 2017)

advanced_2016_17 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Advanced%20Stats/advanced_stats_16_17.csv") |> 
  mutate(start_year = 2016)

advanced_df <- rbind(advanced_2023_24, advanced_2022_23, advanced_2021_22, advanced_2020_21,
                     advanced_2019_20, advanced_2018_19, advanced_2017_18, advanced_2016_17)


## EDAs

library(ggplot2)

### EDAs using Basic Stats





### EDAs using Miscellaneous Stats

long_misc_df_exp <- long_misc_df |>
  group_by(Player) |> 
  summarize(ptspg_diff_23_22 = abs(`PTS/GP_2023` - `PTS/GP_2022`),
            ptspg_diff_22_21 = abs(`PTS/GP_2022` - `PTS/GP_2021`),
            ptspg_diff_21_20 = abs(`PTS/GP_2021` - `PTS/GP_2020`),
            ptspg_diff_20_19 = abs(`PTS/GP_2020` - `PTS/GP_2019`),
            ptspg_diff_19_18 = abs(`PTS/GP_2019` - `PTS/GP_2018`),
            ptspg_diff_18_17 = abs(`PTS/GP_2018` - `PTS/GP_2017`),
            ptspg_diff_17_16 = abs(`PTS/GP_2017` - `PTS/GP_2016`),
            ops_diff_23_22 = abs(OPS_2023 - OPS_2022),
            ops_diff_22_21 = abs(OPS_2022 - OPS_2021),
            ops_diff_21_20 = abs(OPS_2021 - OPS_2020),
            ops_diff_20_19 = abs(OPS_2020 - OPS_2019),
            ops_diff_19_18 = abs(OPS_2019 - OPS_2018),
            ops_diff_18_17 = abs(OPS_2018 - OPS_2017),
            ops_diff_17_16 = abs(OPS_2017 - OPS_2016),
            total_ptspg_diff = sum(ptspg_diff_23_22, ptspg_diff_22_21, ptspg_diff_21_20, ptspg_diff_20_19, 
                                   ptspg_diff_19_18, ptspg_diff_18_17, ptspg_diff_17_16, na.rm = TRUE),
            total_ops_diff = sum(ops_diff_23_22, ops_diff_22_21, ops_diff_21_20, ops_diff_20_19, 
                                 ops_diff_19_18, ops_diff_18_17, ops_diff_17_16, na.rm = TRUE),
            consistency_score = 0.7 * total_ptspg_diff + 0.3 * total_ops_diff
  )

long_misc_df_exp <- long_misc_df_exp |> full_join(player_pos)

long_misc_df_exp <- long_misc_df_exp |> full_join(num_seasons) 


# visualization 1
long_misc_df_exp |> 
  group_by(Player) |> 
  summarise(total_ptspg_diff = sum(ptspg_diff_23_22, ptspg_diff_22_21, ptspg_diff_21_20, ptspg_diff_20_19, ptspg_diff_19_18, ptspg_diff_18_17, ptspg_diff_17_16)) |> 
  arrange(desc(total_ptspg_diff)) |> 
  head(n = 10) |> 
  ggplot(aes(y = reorder(Player, total_ptspg_diff), x = total_ptspg_diff)) +
  geom_col(fill = "purple") +
  geom_label(aes(label = total_ptspg_diff), size = 5) +
  theme_bw() +
  labs(
    x = "Total Points Per Game Difference",
    y = "Players",
    caption = "Data courtesy of Hockey-Reference"
  )

# visualization 2
long_misc_df_exp |> 
  group_by(Player) |> 
  summarise(total_ops_diff = sum(ops_diff_23_22, ops_diff_22_21, ops_diff_21_20, ops_diff_20_19, ops_diff_19_18, ops_diff_18_17, ops_diff_17_16)) |> 
  arrange(desc(total_ops_diff)) |> 
  head(n = 10) |> 
  ggplot(aes(y = reorder(Player, total_ops_diff), x = total_ops_diff)) +
  geom_col(fill = "purple") +
  geom_label(aes(label = total_ops_diff), size = 5) +
  theme_bw() +
  labs(
    x = "Total Points Per Game Difference",
    y = "Players",
    caption = "Data courtesy of Hockey-Reference"
  )

# visualization 3
long_misc_df_exp |> 
  filter(Pos == "F", num_seasons >= 3) |> 
  group_by(Player) |> 
  arrange(desc(consistency_score)) |> 
  head(n = 10) |> 
  ggplot(aes(y = reorder(Player, consistency_score), x = consistency_score)) +
  geom_col(fill = "purple") +
  geom_label(aes(label = consistency_score), size = 5) +
  theme_bw() +
  labs(
    x = "Preliminary Consistency Score",
    y = "Players",
    caption = "Data courtesy of Hockey-Reference"
  ) +
  scale_x_continuous(limits = c(0, 9), n.breaks = 6)


# visualization 4
long_misc_df_exp |> 
  filter(Pos == "F", num_seasons >= 3) |> 
  filter(consistency_score >= 0.5) |> 
  group_by(Player) |> 
  arrange(consistency_score) |> 
  head(n = 10) |> 
  ggplot(aes(y = reorder(Player, -consistency_score), x = consistency_score)) +
  geom_col(fill = "purple") +
  geom_label(aes(label = consistency_score), size = 5) +
  theme_bw() +
  labs(
    x = "Preliminary Consistency Score",
    y = "Players",
    caption = "Data courtesy of Hockey-Reference"
  ) +
  scale_x_continuous(limits = c(0, 0.6), n.breaks = 7)


# visualization 5
long_misc_df_exp |> 
   group_by(Player) |> 
   summarise(total_ptspg_diff = sum(ptspg_diff_23_22, ptspg_diff_22_21, ptspg_diff_21_20, ptspg_diff_20_19, ptspg_diff_19_18, ptspg_diff_18_17, ptspg_diff_17_16)) |> 
   arrange(total_ptspg_diff) |> 
   head(n = 10) |> 
   ggplot(aes(y = reorder(Player, total_ptspg_diff), x = total_ptspg_diff)) +
   geom_col(fill = "purple") +
   geom_label(aes(label = total_ptspg_diff), size = 5) +
   theme_bw() +
   labs(
     x = "Total Points Per Game Difference",
     y = "Players",
     caption = "Data courtesy of Hockey-Reference")



# - shooting percentage difference
long_misc_df |>
  group_by(Player) |> 
  
  summarize(spg_diff_23_22 = abs(`S/GP_2023` - `S/GP_2022`),
            spg_diff_22_21 = abs(`S/GP_2022` - `S/GP_2021`),
            spg_diff_21_20 = abs(`S/GP_2021` - `S/GP_2020`),
            spg_diff_20_19 = abs(`S/GP_2020` - `S/GP_2019`),
            spg_diff_19_18 = abs(`S/GP_2019` - `S/GP_2018`),
            spg_diff_18_17 = abs(`S/GP_2018` - `S/GP_2017`),
            spg_diff_17_16 = abs(`S/GP_2017` - `S/GP_2016`),
            total_spg_diff = sum(spg_diff_23_22, spg_diff_22_21, spg_diff_21_20, spg_diff_20_19,
                                 spg_diff_19_18, spg_diff_18_17, spg_diff_17_16)) |> 
  arrange(desc(total_spg_diff)) |> 
  head(n = 10) |> 
  ggplot(aes(y = reorder(Player, total_spg_diff), x = total_spg_diff)) +
  geom_col(color = "grey", fill = "lightblue") +
  geom_label(aes(label = total_spg_diff), size = 3) +
  labs(x = "Difference in Total Shooting Percentage Per Game", y = "Players") +
  theme_minimal()


### EDAs using Advanced Stats




advanced_df |> 
  select("Player", "Pos", "CF%", "FF%", "oiSH%", "oiSV%", "oZS%", "dZS%", "start_year") |> 
  filter(Player == "Jake Guentzel") |> 
  pivot_longer(cols = c("CF%", "FF%", "oiSH%", "oiSV%", "oZS%", "dZS%"),
               names_to = "Stats",
               values_to = "Value") |> 
  ggplot(aes(x = start_year, y = Value, color = Stats)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(title = "Metrics Over the Years for Jake Guentzel",
       x = "Year",
       y = "Stats Value",
       color = "Stats") +
  theme_minimal() +
  scale_x_continuous(n.breaks = 8)










