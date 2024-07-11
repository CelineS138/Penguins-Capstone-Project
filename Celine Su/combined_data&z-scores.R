
library(tidyverse)
library(broom)
library(ggplot2)

# Data

## Basic Stats
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

basic_df <- rbind(basic_2016_17, basic_2017_18, basic_2018_19, basic_2019_20, 
                  basic_2020_21, basic_2021_22, basic_2022_23, basic_2023_24)

basic_df <- basic_df |> 
  filter(Pos != "G", GP >= 20)

long_basic_df <- basic_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), values_from = c(G, A, PTS, EVG, PPG, SHG, EVA, PPA, SHA, `+/-`, TOI, `S%`, PS), names_from = start_year) |> 
  unique()

long_basic_df <- long_basic_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)


long_basic_df <- long_basic_df |> 
  arrange(Player, PlayerID)


## Advanced Stats 
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

advanced_df <- advanced_df |> 
  filter(GP >= 20, Pos != "G") |> 
  unique()

long_adv_df <- advanced_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), values_from = c(GP, CF, CA, FF, FA, `oiSH%`, `oiSV%`, PDO, TK, GV, `CF%`, `FF%`), names_from = start_year) |> 
  unique()

long_adv_df <- long_adv_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)


## Miscellaneous Stats
misc_2023_24 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_23_24.csv") |> 
  mutate(start_year = 2023)

misc_2022_23 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_22_23.csv") |>
  mutate(start_year = 2022)

misc_2021_22 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_21_22.csv") |> 
  mutate(start_year = 2021)

misc_2020_21 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_20_21.csv") |> 
  mutate(start_year = 2020)

misc_2019_20 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_19_20.csv") |> 
  mutate(start_year = 2019)

misc_2018_19 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_18_19.csv") |> 
  mutate(start_year = 2018)

misc_2017_18 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_17_18.csv") |> 
  mutate(start_year = 2017)

misc_2016_17 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_16_17.csv") |> 
  mutate(start_year = 2016)


misc_df <- rbind(misc_2023_24, misc_2022_23, misc_2021_22, misc_2020_21, misc_2019_20,
                 misc_2018_19, misc_2017_18, misc_2016_17)

misc_df <- misc_df |> 
  filter(Pos != "G", GP >= 20)

misc_df_pos <- misc_df |> 
  mutate(Pos = str_extract(Pos, "[A-Z]"))

num_seasons <- misc_df |> 
  group_by(Player, PlayerID) |> 
  count() |> 
  ungroup() |> 
  mutate(num_seasons = n) |> 
  unique()

player_pos <- misc_df_pos |> 
  group_by(Player) |> 
  summarise(Pos = ifelse(Pos %in% c("C", "L", "R", "W", "F"), "F", "D")) |> 
  unique()

long_misc_df <- misc_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), 
              values_from = c(`G/GP`, `A/GP`, `PTS/GP`, `GC/GP`, `S/GP`, `+/-`, `OPS`, `DPS`, `PS`), 
              names_from = start_year)

long_misc_df <- long_misc_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)


## Skater Time on Ice Stats (toi)

toi_2023_24 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_23_24.csv") |> 
  mutate(start_year = 2023)

toi_2022_23 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_22_23.csv") |>
  mutate(start_year = 2022)

toi_2021_22 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_21_22.csv")|> 
  mutate(start_year = 2021)

toi_2020_21 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_20_21.csv") |> 
  mutate(start_year = 2020)

toi_2019_20 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_19_20.csv") |> 
  mutate(start_year = 2019)

toi_2018_19 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_18_19.csv") |> 
  mutate(start_year = 2018)

toi_2017_18 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_17_18.csv") |> 
  mutate(start_year = 2017)

toi_2016_17 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_16_17.csv") |> 
  mutate(start_year = 2016)

toi_df <- rbind(toi_2016_17, toi_2017_18, toi_2018_19, toi_2019_20, 
                toi_2020_21, toi_2021_22, toi_2022_23, toi_2023_24)

toi_df <- toi_df |> 
  select(-`...7`, -`...12`, -`...17`)

toi_df <- toi_df |> 
  filter(Pos != "G", GP >= 20, PlayerID != "ahose02")

long_toi_df <- toi_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), values_from = c(`TOI (EV)`, `CF% Rel (EV)`, `GF/60 (EV)`, `GA/60 (EV)`, `TOI (PP)`, `CF% Rel (PP)`, `GF/60 (PP)`, `GA/60 (PP)`, `TOI (SH)`, `CF% Rel (SH)`, `GF/60 (SH)`, `GA/60 (SH)`), names_from = start_year) |> 
  unique()

long_toi_df <- long_toi_df %>% 
  unnest() %>%
  replace(. == "NULL", NA)

long_toi_df <- long_toi_df |> arrange(Player, PlayerID)


# Combined Data

complete_df3 <- cbind(long_basic_df, long_adv_df, long_toi_df, long_misc_df)

complete_df <- complete_df3[ , !duplicated(colnames(complete_df3))]

complete_df <- cbind(complete_df, player_pos, num_seasons)
complete_df <- complete_df[ , !duplicated(colnames(complete_df))]; complete_df <- complete_df[, -276]

complete_df_sel <- complete_df |> 
  filter(num_seasons >= 5)


# Scorer

## Data manipulation
scorer <- complete_df_sel |> 
  select(Player, PlayerID, starts_with("TOI_"), starts_with("GC"), contains("EVG_"), contains("PPA_"))

scorer_60 <- scorer |> 
  group_by(Player, PlayerID) |> 
  summarize(`EVG/60_2023` = (EVG_2023 / TOI_2023) * 60,
            `EVG/60_2022` = (EVG_2022 / TOI_2022) * 60,
            `EVG/60_2021` = (EVG_2021 / TOI_2021) * 60,
            `EVG/60_2020` = (EVG_2020 / TOI_2020) * 60,
            `EVG/60_2019` = (EVG_2019 / TOI_2019) * 60,
            `EVG/60_2018` = (EVG_2018 / TOI_2018) * 60,
            `EVG/60_2017` = (EVG_2017 / TOI_2017) * 60,
            `EVG/60_2016` = (EVG_2016 / TOI_2016) * 60,
            
            `PPA/60_2023` = (PPA_2023 / TOI_2023) * 60,
            `PPA/60_2022` = (PPA_2022 / TOI_2022) * 60,
            `PPA/60_2021` = (PPA_2021 / TOI_2021) * 60,
            `PPA/60_2020` = (PPA_2020 / TOI_2020) * 60,
            `PPA/60_2019` = (PPA_2019 / TOI_2019) * 60,
            `PPA/60_2018` = (PPA_2018 / TOI_2018) * 60,
            `PPA/60_2017` = (PPA_2017 / TOI_2017) * 60,
            `PPA/60_2016` = (PPA_2016 / TOI_2016) * 60,
            
            `GC/60_2023` = (`GC/GP_2023` / TOI_2023) * 60,
            `GC/60_2022` = (`GC/GP_2022` / TOI_2022) * 60,
            `GC/60_2021` = (`GC/GP_2021` / TOI_2021) * 60,
            `GC/60_2020` = (`GC/GP_2020` / TOI_2020) * 60,
            `GC/60_2019` = (`GC/GP_2019` / TOI_2019) * 60,
            `GC/60_2018` = (`GC/GP_2018` / TOI_2018) * 60,
            `GC/60_2017` = (`GC/GP_2017` / TOI_2017) * 60,
            `GC/60_2016` = (`GC/GP_2016` / TOI_2016) * 60
            )

scorer_stats <- scorer_60 |> 
  ungroup() |> 
  mutate(mean_EVG_2023 = mean(`EVG/60_2023`, na.rm = TRUE),
         mean_EVG_2022 = mean(`EVG/60_2022`, na.rm = TRUE),
         mean_EVG_2021 = mean(`EVG/60_2021`, na.rm = TRUE),
         mean_EVG_2020 = mean(`EVG/60_2020`, na.rm = TRUE),
         mean_EVG_2019 = mean(`EVG/60_2019`, na.rm = TRUE),
         mean_EVG_2018 = mean(`EVG/60_2018`, na.rm = TRUE),
         mean_EVG_2017 = mean(`EVG/60_2017`, na.rm = TRUE),
         mean_EVG_2016 = mean(`EVG/60_2016`, na.rm = TRUE),
         
         mean_PPA_2023 = mean(`PPA/60_2023`, na.rm = TRUE),
         mean_PPA_2022 = mean(`PPA/60_2022`, na.rm = TRUE),
         mean_PPA_2021 = mean(`PPA/60_2021`, na.rm = TRUE),
         mean_PPA_2020 = mean(`PPA/60_2020`, na.rm = TRUE),
         mean_PPA_2019 = mean(`PPA/60_2019`, na.rm = TRUE),
         mean_PPA_2018 = mean(`PPA/60_2018`, na.rm = TRUE),
         mean_PPA_2017 = mean(`PPA/60_2017`, na.rm = TRUE),
         mean_PPA_2016 = mean(`PPA/60_2016`, na.rm = TRUE),
         
         mean_GC_2023 = mean(`GC/60_2023`, na.rm = TRUE),
         mean_GC_2022 = mean(`GC/60_2022`, na.rm = TRUE),
         mean_GC_2021 = mean(`GC/60_2021`, na.rm = TRUE),
         mean_GC_2020 = mean(`GC/60_2020`, na.rm = TRUE),
         mean_GC_2019 = mean(`GC/60_2019`, na.rm = TRUE),
         mean_GC_2018 = mean(`GC/60_2018`, na.rm = TRUE),
         mean_GC_2017 = mean(`GC/60_2017`, na.rm = TRUE),
         mean_GC_2016 = mean(`GC/60_2016`, na.rm = TRUE),
         
         
         sd_EVG_2023 = sd(`EVG/60_2023`, na.rm = TRUE),
         sd_EVG_2022 = sd(`EVG/60_2022`, na.rm = TRUE),
         sd_EVG_2021 = sd(`EVG/60_2021`, na.rm = TRUE),
         sd_EVG_2020 = sd(`EVG/60_2020`, na.rm = TRUE),
         sd_EVG_2019 = sd(`EVG/60_2019`, na.rm = TRUE),
         sd_EVG_2018 = sd(`EVG/60_2018`, na.rm = TRUE),
         sd_EVG_2017 = sd(`EVG/60_2017`, na.rm = TRUE),
         sd_EVG_2016 = sd(`EVG/60_2016`, na.rm = TRUE),
         
         sd_PPA_2023 = sd(`PPA/60_2023`, na.rm = TRUE),
         sd_PPA_2022 = sd(`PPA/60_2022`, na.rm = TRUE),
         sd_PPA_2021 = sd(`PPA/60_2021`, na.rm = TRUE),
         sd_PPA_2020 = sd(`PPA/60_2020`, na.rm = TRUE),
         sd_PPA_2019 = sd(`PPA/60_2019`, na.rm = TRUE),
         sd_PPA_2018 = sd(`PPA/60_2018`, na.rm = TRUE),
         sd_PPA_2017 = sd(`PPA/60_2017`, na.rm = TRUE),
         sd_PPA_2016 = sd(`PPA/60_2016`, na.rm = TRUE),
         
         sd_GC_2023 = sd(`GC/60_2023`, na.rm = TRUE),
         sd_GC_2022 = sd(`GC/60_2022`, na.rm = TRUE),
         sd_GC_2021 = sd(`GC/60_2021`, na.rm = TRUE),
         sd_GC_2020 = sd(`GC/60_2020`, na.rm = TRUE),
         sd_GC_2019 = sd(`GC/60_2019`, na.rm = TRUE),
         sd_GC_2018 = sd(`GC/60_2018`, na.rm = TRUE),
         sd_GC_2017 = sd(`GC/60_2017`, na.rm = TRUE),
         sd_GC_2016 = sd(`GC/60_2016`, na.rm = TRUE)
  )  


## Z-score  

scorer_zscore <- scorer_stats |> 
  group_by(Player, PlayerID) |>
  summarize(EVG_zscore_2023 = (`EVG/60_2023` - `mean_EVG_2023`) / `sd_EVG_2023`,
            EVG_zscore_2022 = (`EVG/60_2022` - `mean_EVG_2022`) / `sd_EVG_2022`,
            EVG_zscore_2021 = (`EVG/60_2021` - `mean_EVG_2021`) / `sd_EVG_2021`,
            EVG_zscore_2020 = (`EVG/60_2020` - `mean_EVG_2020`) / `sd_EVG_2020`,
            EVG_zscore_2019 = (`EVG/60_2019` - `mean_EVG_2019`) / `sd_EVG_2019`,
            EVG_zscore_2018 = (`EVG/60_2018` - `mean_EVG_2018`) / `sd_EVG_2018`,
            EVG_zscore_2017 = (`EVG/60_2017` - `mean_EVG_2017`) / `sd_EVG_2017`,
            EVG_zscore_2016 = (`EVG/60_2016` - `mean_EVG_2016`) / `sd_EVG_2016`,
            
            PPA_zscore_2023 = (`PPA/60_2023` - `mean_PPA_2023`) / `sd_PPA_2023`,
            PPA_zscore_2022 = (`PPA/60_2022` - `mean_PPA_2022`) / `sd_PPA_2022`,
            PPA_zscore_2021 = (`PPA/60_2021` - `mean_PPA_2021`) / `sd_PPA_2021`,
            PPA_zscore_2020 = (`PPA/60_2020` - `mean_PPA_2020`) / `sd_PPA_2020`,
            PPA_zscore_2019 = (`PPA/60_2019` - `mean_PPA_2019`) / `sd_PPA_2019`,
            PPA_zscore_2018 = (`PPA/60_2018` - `mean_PPA_2018`) / `sd_PPA_2018`,
            PPA_zscore_2017 = (`PPA/60_2017` - `mean_PPA_2017`) / `sd_PPA_2017`,
            PPA_zscore_2016 = (`PPA/60_2016` - `mean_PPA_2016`) / `sd_PPA_2016`,
            
            GC_zscore_2023 = (`GC/60_2023` - `mean_GC_2023`) / `sd_GC_2023`,
            GC_zscore_2022 = (`GC/60_2022` - `mean_GC_2022`) / `sd_GC_2022`,
            GC_zscore_2021 = (`GC/60_2021` - `mean_GC_2021`) / `sd_GC_2021`,
            GC_zscore_2020 = (`GC/60_2020` - `mean_GC_2020`) / `sd_GC_2020`,
            GC_zscore_2019 = (`GC/60_2019` - `mean_GC_2019`) / `sd_GC_2019`,
            GC_zscore_2018 = (`GC/60_2018` - `mean_GC_2018`) / `sd_GC_2018`,
            GC_zscore_2017 = (`GC/60_2017` - `mean_GC_2017`) / `sd_GC_2017`,
            GC_zscore_2016 = (`GC/60_2016` - `mean_GC_2016`) / `sd_GC_2016`,
            )


scorer_zscore_long <- scorer_zscore |> 
  select(-PlayerID) |> 
  pivot_longer(-c(Player),
               names_to = c(".value", "Year"), 
               names_sep = "_zscore_")


scorer_zscore_long |> 
  filter(Player == "Sidney Crosby") |> 
  ggplot(aes(x = Year, y = EVG)) +
  geom_point() + 
  geom_hline(yintercept = 0.5, linetype = "dashed")










