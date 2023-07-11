library(tidyverse)
library(readxl)
library(rstanarm)
library(rayshader)
library(rgl)
#combining results for 2021 - 2023
data1 <- read_excel("data/2023ATPResults3.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2023")
data2 <- read_excel("data/2022ATPResults.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2022")
data3 <- read_excel("data/2021ATPResults.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2021")
data <- bind_rows(data1, data2, data3)



#creating each stat as own tibble
player_wins <- data |> 
  count(Winner, surface = Surface, year) |>
  rename(name = Winner, num_wins = n)

player_losses <- data |>
  count(Loser, surface = Surface, year) |>
  rename(name = Loser, num_losses = n)

player_upset_wins <- data |>
  summarize(num_upsets = sum(upset), .by = c(Surface, Winner, year)) |>
  rename(surface = Surface, name = Winner)

player_upset_losses <- data |>
  summarize(num_was_upset = sum(upset), .by = c(Surface, Loser, year)) |>
  rename(surface = Surface, name = Loser)

player_win_sets <- data |>
  summarize(wsets_won = sum(Wsets, na.rm = TRUE), 
            wsets_lost = sum(Lsets, na.rm = TRUE),
            avg_sets_lost = mean(Lsets, na.rm = TRUE),
            .by = c(Surface, Winner, year)) |>
  rename(surface = Surface, name = Winner)

player_loss_sets <- data |>
  summarize(lsets_won = sum(Lsets, na.rm = TRUE), 
            lsets_lost = sum(Wsets, na.rm = TRUE), 
            .by = c(Surface, Loser, year)) |>
  rename(surface = Surface, name = Loser)



#joining tibbles
upsets <- left_join(player_upset_wins, player_upset_losses, by = c("name", "surface", "year")) |>
  replace_na(list(num_was_upset = 0))
wins_and_losses <- left_join(player_wins, player_losses, by = c("name", "surface", "year")) |>
  replace_na(list(num_losses = 0))
won_lost_sets <- left_join(player_win_sets, player_loss_sets, by = c("name", "surface", "year")) |>
  replace_na(list(lsets_won = 0, lsets_lost = 0, wsets_won = 0, wsets_lost = 0))
win_loss_upset <- left_join(wins_and_losses, upsets, by = c("name", "surface", "year"))
all_stats <- left_join(win_loss_upset, won_lost_sets, by = c("name", "surface", "year")) |>
  mutate(sets_won = wsets_won + lsets_won,
         sets_lost = wsets_lost + lsets_lost) |>
  select(name:num_was_upset, sets_won, sets_lost, avg_sets_lost) |>
  mutate(name = tolower(name))

#selecting only the people in wimbledon
ppl_in_wimbledon <- read_excel("data/Wimbledon2023Draw.xlsx") |>
  mutate(name = tolower(name))

name_vector <- ppl_in_wimbledon$name

#all_stats
#names w/ ' are just spaces, i.e O'Connell = O Connell
#Wolf J.J. instead of Wolf J.

selected_rows <- all_stats[all_stats$name %in% name_vector, ]
top_ten <- selected_rows |>
  mutate(odds1 = if_else(surface == "Grass", 
                         2.5*((num_wins - num_losses)*50 + 2*num_upsets - 2*num_was_upset + (sets_won - sets_lost)/2),
                         (num_wins - num_losses)*50 + 2*num_upsets - 2*num_was_upset + (sets_won - sets_lost)/2)) |>
  select(name, surface, year, odds1) |>
  mutate(odds = if_else(year == 2023, odds1 * 2, 
                        if_else(year == 2022, odds1 * 1,
                                odds1 * 0.5))) |>
  group_by(name) |>
  summarize(total_odds = sum(odds)) |>
  filter(total_odds >= 0) |>
  mutate(prob = total_odds / sum(total_odds)) |>
  arrange(desc(prob)) |>
  slice(1:10)

#Yuhan's work
top_ten$name <- factor(top_ten$name, levels = c("alcaraz c.", "medvedev d.", "djokovic n.",
                                                "sinner j.", "rublev a.", "norrie c.",
                                                "tsitsipas s.", "fritz t.", "ruud c.",
                                                "zverev a."))

player_names <- c("alcaraz c.", "medvedev d.", "djokovic n.", "sinner j.", "rublev a.",
                  "norrie c.", "tsitsipas s.", "fritz t.", "ruud c.", "zverev a.")

modelfit <- stan_glm(top_ten, formula = prob ~ name, family = gaussian, refresh = 0, iter = 5000,
                     seed = 76)

newdata <- tibble(name = factor(player_names, levels = levels(top_ten$name)))

pp <- posterior_epred(modelfit, newdata = newdata) |>
  as_tibble() |>
  set_names(c("alcaraz c.", "medvedev d.", "djokovic n.",
              "sinner j.", "rublev a.", "norrie c.",
              "tsitsipas s.", "fritz t.", "ruud c.	",
              "zverev a.")) |> 
  rowwise()

pregraph <- pp |>
  pivot_longer(cols = c("alcaraz c.", "medvedev d.", "djokovic n.",
                        "sinner j.", "rublev a.", "norrie c.",
                        "tsitsipas s.", "fritz t.", "ruud c.	",
                        "zverev a."),
               names_to = "name",
               values_to = "probability") 

#rayshader
pregraph$name <- factor(pregraph$name, levels = c("alcaraz c.", "medvedev d.", "djokovic n.",
                                                  "sinner j.", "rublev a.", "norrie c.",
                                                  "tsitsipas s.", "fritz t.", "ruud c.	",
                                                  "zverev a."))

write_csv(pregraph, "pregraph.csv")