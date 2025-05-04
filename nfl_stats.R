# Project Ideas ----
# Take one draft class (2020)
# Analyze stats over contract period
#   Needs:
#   - Player contract stats (rookie_contracts)
#   - Player stats by year 
#     - College (college_rookie_stats)
#       - Senior year only
#     - NFL
#       - Stats per year through 2024

# Packages ----
# install.packages("rvest")
# install.packages("tidyverse")
# install.packages("googlesheets4")
# install.packages("nflreadr")
# install.packages("ggplot2")
# install.packages("dplyr")
library(nflreadr)
library(rvest)
library(tidyverse)
library(googlesheets4)
library(ggplot2)
library(dplyr)

# Scraping ----
# NFL signing and salary table (only top 100 due to paywall)
nfl_salary_raw <- read_html_live("https://www.spotrac.com/nfl/contracts") %>%
  html_elements(css = "table") %>%
  html_table()
nfl_salary <- nfl_salary_raw[[2]]

# NFL rookie contract "signing scale" for the 2020 draft year by pick
rookie_scale_raw <- read_html_live("https://www.spotrac.com/nfl/cba/rookie-scale/_/year/2020") %>%
  html_elements(css = "table") %>%
  html_table()
rookie_scale <- rookie_scale_raw[[1]]
rookie_scale$`Total Value` = as.numeric(gsub("[\\$,]","", rookie_scale$`Total Value`))
rookie_scale$`Year 1` = as.numeric(gsub("[\\$,]","", rookie_scale$`Year 1`))
rookie_scale$`Year 2` = as.numeric(gsub("[\\$,]","", rookie_scale$`Year 2`))
rookie_scale$`Year 3` = as.numeric(gsub("[\\$,]","", rookie_scale$`Year 3`))
rookie_scale$`Year 4` = as.numeric(gsub("[\\$,]","", rookie_scale$`Year 4`))

# 2020 NFL draftee college stats
college_rookie_stats_raw <- read_html_live("https://www.pro-football-reference.com/years/2020/draft.htm") %>%
  html_elements(css = "table") %>%
  html_table()
college_rookie_stats <- college_rookie_stats_raw[[1]]
header_names <- as.character(college_rookie_stats[1, ])
header_names[29] <- "dropME" # MAKE SURE TO DROP LAST COL
for (i in 1:29){
  if (i == 26) {
    header_names[i] <- paste0("defense_", header_names[i])
  } else if (between(i, 14, 18)) {
    header_names[i] <- paste0("passing_", header_names[i])
  } else if (between(i, 19, 21)) {
    header_names[i] <- paste0("rushing_", header_names[i])
  } else if (between(i, 22, 24)) {
    header_names[i] <- paste0("receiving_", header_names[i])
  } else {
    next
  }
}
colnames(college_rookie_stats) <- header_names
college_rookie_stats <- college_rookie_stats %>%
  filter(Rnd != "Rnd")
college_rookie_stats[[11]] <- as.integer(college_rookie_stats[[11]])
college_rookie_stats[[2]] <- as.integer(college_rookie_stats[[2]])



# 2020-2024 NFL draftee stats per year

player_stats_2020 <- load_player_stats(2020)
colnames(player_stats_2020)[3] <- "Player"

player_stats_2021 <- load_player_stats(2021)
colnames(player_stats_2021)[3] <- "Player"

player_stats_2022 <- load_player_stats(2022)
colnames(player_stats_2022)[3] <- "Player"

player_stats_2023 <- load_player_stats(2023)
colnames(player_stats_2023)[3] <- "Player"

player_stats_2024 <- load_player_stats(2024)
colnames(player_stats_2024)[3] <- "Player"

# 2020 NFL draft table with contract
rookie_contracts_raw <- read_html_live("https://www.spotrac.com/nfl/draft/_/year/2020/sort/pickhttps://www.spotrac.com/nfl/draft/_/year/2024/sort/pick") %>%
  html_elements(css = "table") %>%
  html_table()
rookie_contracts <- rookie_contracts_raw[[1]]

# Tidying ----
# For rookies only, filter player stats 2020-2024
rookie_stats_2020 <- semi_join(player_stats_2020, college_rookie_stats, by = "Player")
rookie_stats_2021 <- semi_join(player_stats_2021, college_rookie_stats, by = "Player")
rookie_stats_2022 <- semi_join(player_stats_2022, college_rookie_stats, by = "Player")
rookie_stats_2023 <- semi_join(player_stats_2023, college_rookie_stats, by = "Player")
rookie_stats_2024 <- semi_join(player_stats_2024, college_rookie_stats, by = "Player")

# Visualizations ----
# Contract signing scale
marks_no_sci <- function(x) format(x, big.mark = ",", decimal.mark = ",", scientific = FALSE)
draft_contract_scale <- ggplot(rookie_scale, aes(x = Pick, y = `Total Value` )) +
  geom_line() +
  scale_y_continuous(labels = marks_no_sci) + 
  coord_cartesian(ylim = c(0, 40000000)) +
  ylab("Total Value in Dollars ($)") +
  scale_x_continuous(breaks = seq(0, 255, by = 32))

# Draft pick vs. weighted approximate value
pick_vs_wav <- ggplot(data = college_rookie_stats, aes(x = Pick, y = wAV)) + 
  geom_point() + 
  geom_smooth() +
  labs(x = "Draft Pick", y = "Weighted Approximate Value") +
  scale_x_continuous(breaks = seq(0, 257, by = 32))
pick_vs_wav





