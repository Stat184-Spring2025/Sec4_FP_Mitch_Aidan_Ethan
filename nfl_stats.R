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
# install.packages("hrbrthemes")
# install.packages("viridis")
library(nflreadr)
library(rvest)
library(tidyverse)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(data.table)

# Scraping ----
# NFL signing and salary table 
nfl_salary_raw <- read_html_live("https://www.spotrac.com/nfl/contracts") %>%
  html_elements(css = "table") %>%
  html_table()
nfl_salary <- nfl_salary_raw[[2]]

# NFL contract extensions since 2021
extensions_2021_raw <- read_html_live("https://www.spotrac.com/nfl/contracts/extensions/_/year/2021") %>%
  html_elements(css = "table") %>%
  html_table()
extensions_2021 <- extensions_2021_raw[[2]]

extensions_2022_raw <- read_html_live("https://www.spotrac.com/nfl/contracts/extensions/_/year/2022/") %>%
  html_elements(css = "table") %>%
  html_table()
extensions_2022 <- extensions_2022_raw[[2]]

extensions_2023_raw <- read_html_live("https://www.spotrac.com/nfl/contracts/extensions/_/year/2023/") %>%
  html_elements(css = "table") %>%
  html_table()
extensions_2023 <- extensions_2023_raw[[2]]

extensions_2024_raw <- read_html_live("https://www.spotrac.com/nfl/contracts/extensions/_/year/2024/") %>%
  html_elements(css = "table") %>%
  html_table()
extensions_2024 <- extensions_2024_raw[[2]]

extensions_2025_raw <- read_html_live("https://www.spotrac.com/nfl/contracts/extensions/_/year/2025/") %>%
  html_elements(css = "table") %>%
  html_table()
extensions_2025 <- extensions_2025_raw[[2]]
 
names(extensions_2021)[2] <- "Player"
names(extensions_2022)[2] <- "Player"
names(extensions_2023)[2] <- "Player"
names(extensions_2024)[2] <- "Player"
names(extensions_2025)[2] <- "Player"

extensions_all <- bind_rows(
  list(extensions_2021, extensions_2022, extensions_2023, extensions_2024, extensions_2025)
)

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
rookie_contracts_raw <- read_html_live("https://www.spotrac.com/nfl/draft/_/year/2020/sort/pickhttps://www.spotrac.com/nfl/draft/_/year/2020/sort/pick") %>%
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

# Skill Position wrangling ----
## QB
rookie_stats_2020_qb <- rookie_stats_2020 %>%
  filter(position == "QB") %>%
  group_by(Player) %>%
  summarise(total_yds = sum(passing_yards),
            comp = sum(completions),
            att = sum(attempts),
            yds = sum(passing_air_yards),
            passing_td = sum(passing_tds),
            ints = sum(interceptions),
            rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            )
rookie_stats_2021_qb <- rookie_stats_2021 %>%
  filter(position == "QB") %>%
  group_by(Player) %>%
  summarise(total_yds = sum(passing_yards),
            comp = sum(completions),
            att = sum(attempts),
            yds = sum(passing_air_yards),
            passing_td = sum(passing_tds),
            ints = sum(interceptions),
            rushes = sum(carries),
            rush_yds = sum(rushing_yards),
  )
rookie_stats_2022_qb <- rookie_stats_2022 %>%
  filter(position == "QB") %>%
  group_by(Player) %>%
  summarise(total_yds = sum(passing_yards),
            comp = sum(completions),
            att = sum(attempts),
            yds = sum(passing_air_yards),
            passing_td = sum(passing_tds),
            ints = sum(interceptions),
            rushes = sum(carries),
            rush_yds = sum(rushing_yards),
  )
rookie_stats_2023_qb <- rookie_stats_2023 %>%
  filter(position == "QB") %>%
  group_by(Player) %>%
  summarise(total_yds = sum(passing_yards),
            comp = sum(completions),
            att = sum(attempts),
            yds = sum(passing_air_yards),
            passing_td = sum(passing_tds),
            ints = sum(interceptions),
            rushes = sum(carries),
            rush_yds = sum(rushing_yards),
  )
rookie_stats_2024_qb <- rookie_stats_2024 %>%
  filter(position == "QB") %>%
  group_by(Player) %>%
  summarise(total_yds = sum(passing_yards),
            comp = sum(completions),
            att = sum(attempts),
            yds = sum(passing_air_yards),
            passing_td = sum(passing_tds),
            ints = sum(interceptions),
            rushes = sum(carries),
            rush_yds = sum(rushing_yards),
  )
## RB
rookie_stats_2020_rb <- rookie_stats_2020 %>%
  filter(position == "RB") %>%
  group_by(Player) %>%
  summarise(rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            fumbles = sum(rushing_fumbles_lost, receiving_fumbles_lost),
            receptions = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_tds = sum(receiving_tds),
            total_yds = sum(rushing_yards, receiving_yards)
  )
rookie_stats_2021_rb <- rookie_stats_2021 %>%
  filter(position == "RB") %>%
  group_by(Player) %>%
  summarise(rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            fumbles = sum(rushing_fumbles_lost, receiving_fumbles_lost),
            receptions = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_tds = sum(receiving_tds),
            total_yds = sum(rushing_yards, receiving_yards)
  )
rookie_stats_2022_rb <- rookie_stats_2022 %>%
  filter(position == "RB") %>%
  group_by(Player) %>%
  summarise(rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            fumbles = sum(rushing_fumbles_lost, receiving_fumbles_lost),
            receptions = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_tds = sum(receiving_tds),
            total_yds = sum(rushing_yards, receiving_yards)
  )
rookie_stats_2023_rb <- rookie_stats_2023 %>%
  filter(position == "RB") %>%
  group_by(Player) %>%
  summarise(rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            fumbles = sum(rushing_fumbles_lost, receiving_fumbles_lost),
            receptions = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_tds = sum(receiving_tds),
            total_yds = sum(rushing_yards, receiving_yards)
  )
rookie_stats_2024_rb <- rookie_stats_2024 %>%
  filter(position == "RB") %>%
  group_by(Player) %>%
  summarise(rushes = sum(carries),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            fumbles = sum(rushing_fumbles_lost, receiving_fumbles_lost),
            receptions = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_tds = sum(receiving_tds),
            total_yds = sum(rushing_yards, receiving_yards)
  )
## WR
rookie_stats_2020_wr <- rookie_stats_2020 %>%
  filter(position == "WR") %>%
  group_by(Player) %>%
  summarise(catches = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_td = sum(receiving_tds),
            fumbles = sum(receiving_fumbles_lost, rushing_fumbles_lost),
            yac = sum(receiving_yards_after_catch),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            total_yds = sum(receiving_yards, rushing_yards),
            total_tds = sum(receiving_tds, rushing_tds),
  )
rookie_stats_2021_wr <- rookie_stats_2021 %>%
  filter(position == "WR") %>%
  group_by(Player) %>%
  summarise(catches = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_td = sum(receiving_tds),
            fumbles = sum(receiving_fumbles_lost, rushing_fumbles_lost),
            yac = sum(receiving_yards_after_catch),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            total_yds = sum(receiving_yards, rushing_yards),
            total_tds = sum(receiving_tds, rushing_tds),
  )
rookie_stats_2022_wr <- rookie_stats_2022 %>%
  filter(position == "WR") %>%
  group_by(Player) %>%
  summarise(catches = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_td = sum(receiving_tds),
            fumbles = sum(receiving_fumbles_lost, rushing_fumbles_lost),
            yac = sum(receiving_yards_after_catch),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            total_yds = sum(receiving_yards, rushing_yards),
            total_tds = sum(receiving_tds, rushing_tds),
  )
rookie_stats_2023_wr <- rookie_stats_2023 %>%
  filter(position == "WR") %>%
  group_by(Player) %>%
  summarise(catches = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_td = sum(receiving_tds),
            fumbles = sum(receiving_fumbles_lost, rushing_fumbles_lost),
            yac = sum(receiving_yards_after_catch),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            total_yds = sum(receiving_yards, rushing_yards),
            total_tds = sum(receiving_tds, rushing_tds),
  )
rookie_stats_2024_wr <- rookie_stats_2024 %>%
  filter(position == "WR") %>%
  group_by(Player) %>%
  summarise(catches = sum(receptions),
            rec_yds = sum(receiving_yards),
            rec_td = sum(receiving_tds),
            fumbles = sum(receiving_fumbles_lost, rushing_fumbles_lost),
            yac = sum(receiving_yards_after_catch),
            rush_yds = sum(rushing_yards),
            rush_tds = sum(rushing_tds),
            total_yds = sum(receiving_yards, rushing_yards),
            total_tds = sum(receiving_tds, rushing_tds)
  )
total_rookie_stats_wr <-bind_rows(
  list(rookie_stats_2020_wr, rookie_stats_2021_wr, rookie_stats_2022_wr, rookie_stats_2023_wr, rookie_stats_2024_wr)
) %>%
  group_by(Player) %>%
  summarise(catches = sum(catches),
            rec_yds = sum(rec_yds),
            rec_td = sum(rec_td),
            fumbles = sum(fumbles),
            yac = sum(yac),
            rush_yds = sum(rush_yds),
            rush_tds = sum(rush_tds),
            total_yds = sum(total_yds),
            tds = sum(total_tds)
  )
total_rookie_stats_wr_salary <- inner_join(
  x = total_rookie_stats_wr,
  y = extensions_wr,
  by = join_by(Player == Player)
)
total_rookie_stats_wr_salary$`Value` = as.numeric(gsub("[\\$,]","", total_rookie_stats_wr_salary$`Value`))


total_rookie_stats_rb <-bind_rows(
  list(rookie_stats_2020_rb, rookie_stats_2021_rb, rookie_stats_2022_rb, rookie_stats_2023_rb, rookie_stats_2024_rb)
)  %>%
  group_by(Player) %>%
  summarise(rushes = sum(rushes),
            rush_yds = sum(rush_yds),
            rush_tds = sum(rush_tds),
            fumbles = sum(fumbles),
            receptions = sum(receptions),
            rec_yds = sum(rec_yds),
            tds = sum(rec_tds),
            total_yds = sum(total_yds)
  )
total_rookie_stats_rb_salary <- inner_join(
  x = total_rookie_stats_rb,
  y = extensions_rb,
  by = join_by(Player == Player)
)
total_rookie_stats_rb_salary$`Value` = as.numeric(gsub("[\\$,]","", total_rookie_stats_rb_salary$`Value`))


# 2020 Draft Class extensions
rookie_extensions <- semi_join(extensions_all, college_rookie_stats, by = "Player")
extensions_qb <- rookie_extensions %>% filter(Pos == "QB")
extensions_wr <- rookie_extensions %>% filter(Pos == "WR")
extensions_rb <- rookie_extensions %>% filter(Pos == "RB")
extensions_te <- rookie_extensions %>% filter(Pos == "TE")
extensions_db <- rookie_extensions %>% filter(Pos == "CB" | Pos == "S")
extensions_ol <- rookie_extensions %>% filter(Pos == "LT" | Pos == "RT" | Pos == "G" | Pos == "C")
extensions_dl <- rookie_extensions %>% filter(Pos == "DT")
extensions_lb <- rookie_extensions %>% filter(Pos == "ILB" | Pos == "OLB")
extensions_st <- rookie_extensions %>% filter(Pos == "LS" | Pos == "K")

# Visualizations ----
# Contract signing scale
marks_no_sci <- function(x) format(x, big.mark = ",", decimal.mark = ",", scientific = FALSE)
draft_contract_scale <- ggplot(rookie_scale, aes(x = Pick, y = `Total Value` )) +
  geom_line() +
  scale_y_continuous(labels = marks_no_sci) + 
  coord_cartesian(ylim = c(0, 40000000)) +
  ylab("Total Value in Dollars ($)") +
  scale_x_continuous(breaks = seq(0, 255, by = 32)) + 
  geom_vline(xintercept = 32, color = "orange", linewidth = 1, alpha = 0.4) +
  geom_vline(xintercept = 64, color = "orange", linewidth = 1, alpha = 0.4) +
  geom_vline(xintercept = 106, color = "orange", linewidth = 1, alpha = 0.4)
  

## Position vs. average contract value
player_contracts <- load_contracts()
player_contracts[[7]] <- as.integer(player_contracts[[7]])
player_contracts <- player_contracts %>%
  filter(year_signed >= 2020) %>%
  filter(value > 0)
pos_vs_contract <- ggplot(player_contracts, aes(x = position, y = value, fill = position)) +
  geom_point() +
  xlab("Player Position") +
  ylab("Contract Value (in millions)") +
  guides(fill = FALSE)
pos_vs_contract

# Draft pick vs. weighted approximate value
pick_vs_wav <- ggplot(data = college_rookie_stats, aes(x = Pick, y = wAV)) + 
  geom_point() + 
  geom_smooth() +
  labs(x = "Draft Pick", y = "Weighted Approximate Value") +
  scale_x_continuous(breaks = seq(0, 257, by = 32))
pick_vs_wav

# QBR vs Salary
total_rookie_stats_qb <- bind_rows(
  list(rookie_stats_2020_qb, rookie_stats_2021_qb, rookie_stats_2022_qb, rookie_stats_2023_qb, rookie_stats_2024_qb)) %>%
  group_by(Player) %>%
  summarise(total_yds = sum(total_yds),
            comp = sum(comp),
            att = sum(att),
            yds = sum(yds),
            tds = sum(passing_td),
            ints = sum(ints),
            rushes = sum(rushes),
            rush_yds = sum(rush_yds),
            qbr = (((((sum(comp) / sum(att)) - 0.3) * 5) +
                    (((sum(yds)/sum(att)) - 3) * 0.25) +
                    ((sum(passing_td)/sum(att) * 20)) +
                    (2.375 - (sum(ints)/sum(att)) * 25)
                    ) / 6) * 100
  )
total_rookie_stats_qb_salary <- inner_join(
  x = total_rookie_stats_qb,
  y = extensions_qb,
  by = join_by(Player == Player)
)
total_rookie_stats_qb_salary$`Value` = as.numeric(gsub("[\\$,]","", total_rookie_stats_qb_salary$`Value`))

rating_vs_salary <- ggplot(data = total_rookie_stats_qb_salary,
                           aes(x = qbr, y = Value, size = passing_td)) +
  geom_point() +
  labs(x = "Passer Rating", y = "Next Contract Value in dollars ($)") + 
  coord_cartesian(ylim = c(0, 300000000)) +
  scale_y_continuous(labels = marks_no_sci)

# Skill position scores vs. salary
View(total_rookie_stats_qb_salary)
View(total_rookie_stats_rb_salary)
View(total_rookie_stats_wr_salary)

stats_salary <- bind_rows(
  list(total_rookie_stats_qb_salary, total_rookie_stats_rb_salary, total_rookie_stats_wr_salary)
)

stats_salary_plot <- ggplot(data = stats_salary,
       aes(x = tds, y = Value, shape = Pos, color = Pos)) + 
  geom_point(size = 6) +
  labs(x = "Total Touchdowns", y = "Next Contract Value") + 
  coord_cartesian(ylim = c(0, 300000000)) +
  scale_y_continuous(labels = marks_no_sci)
stats_salary_plot


