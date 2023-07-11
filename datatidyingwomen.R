library(tidyverse)
library(readxl)
library(rstanarm)
library(rayshader)
library(rgl)
data1w <- read_excel("data/2023WTAResults.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2023")
data2w <- read_excel("data/2022WTAResults.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2022")
data3w <- read_excel("data/2021WTAResults.xlsx") |>
  select(Surface, Winner:LRank, Wsets, Lsets, Comment) |>
  filter(Comment == "Completed") |>
  mutate(WRank = as.double(WRank), LRank = as.double(LRank)) |>
  mutate(WRank = replace(WRank, is.na(WRank), 10000),
         LRank = replace(LRank, is.na(LRank), 10000)) |>
  mutate(upset = if_else(WRank > LRank, 1, 0)) |> 
  mutate(year = "2021")
dataw <- bind_rows(data1w, data2w, data3w)


player_winsw <- dataw |> 
  count(Winner, surface = Surface, year) |>
  rename(name = Winner, num_wins = n)

player_lossesw <- dataw |>
  count(Loser, surface = Surface, year) |>
  rename(name = Loser, num_losses = n)

player_upset_winsw <- dataw |>
  summarize(num_upsets = sum(upset), .by = c(Surface, Winner, year)) |>
  rename(surface = Surface, name = Winner)

player_upset_lossesw <- dataw |>
  summarize(num_was_upset = sum(upset), .by = c(Surface, Loser, year)) |>
  rename(surface = Surface, name = Loser)

player_win_setsw <- dataw |>
  summarize(wsets_won = sum(Wsets, na.rm = TRUE), 
            wsets_lost = sum(Lsets, na.rm = TRUE),
            avg_sets_lost = mean(Lsets, na.rm = TRUE),
            .by = c(Surface, Winner, year)) |>
  rename(surface = Surface, name = Winner)

player_loss_setsw <- dataw |>
  summarize(lsets_won = sum(Lsets, na.rm = TRUE), 
            lsets_lost = sum(Wsets, na.rm = TRUE), 
            .by = c(Surface, Loser, year)) |>
  rename(surface = Surface, name = Loser)


upsetsw <- left_join(player_upset_winsw, player_upset_lossesw, by = c("name", "surface", "year")) |>
  replace_na(list(num_was_upset = 0))
wins_and_lossesw <- left_join(player_winsw, player_lossesw, by = c("name", "surface", "year")) |>
  replace_na(list(num_losses = 0))
won_lost_setsw <- left_join(player_win_setsw, player_loss_setsw, by = c("name", "surface", "year")) |>
  replace_na(list(lsets_won = 0, lsets_lost = 0, wsets_won = 0, wsets_lost = 0))
win_loss_upsetw <- left_join(wins_and_lossesw, upsetsw, by = c("name", "surface", "year"))

all_statsw <- left_join(win_loss_upsetw, won_lost_setsw, by = c("name", "surface", "year")) |>
  mutate(sets_won = wsets_won + lsets_won,
         sets_lost = wsets_lost + lsets_lost) |>
  select(name:num_was_upset, sets_won, sets_lost, avg_sets_lost) |>
  mutate(name = tolower(name))

ppl_in_wimbledonw <- read_excel("data/Wimbledon2023DrawWomens.xlsx") |>
  mutate(name = tolower(name))

name_vectorw <- ppl_in_wimbledonw$name

selected_rowsw <- all_statsw[all_statsw$name %in% name_vectorw, ]
top_tenw <- selected_rowsw |>
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

#Yuhan's work part 2
top_tenw$name <- factor(top_tenw$name, levels = c("swiatek i.", "sabalenka a.", "jabeur o.", 
                                                  "ostapenko j.", "rybakina e.", "kvitova p.", 
                                                  "garcia c.", "pegula j.", "bencic b.", "alexandrova e."))

player_namesw <- c("swiatek i.", "sabalenka a.", "jabeur o.", "ostapenko j.", "rybakina e.", 
                   "kvitova p.", "garcia c.", "pegula j.", "bencic b.", "alexandrova e.")

modelfitw <- stan_glm(top_tenw, formula = prob ~ name, family = gaussian, refresh = 0, iter = 5000,
                      seed = 76)

newdata2 <- tibble(name = factor(player_namesw, levels = levels(top_tenw$name)))

ppw <- posterior_epred(modelfitw, newdata = newdata2) |>
  as_tibble() |>
  set_names(c("swiatek i.", "sabalenka a.", "jabeur o.", "ostapenko j.", "rybakina e.", 
              "kvitova p.", "garcia c.", "pegula j.", "bencic b.", "alexandrova e.")) |> 
  rowwise() 

pregraphw <- ppw |>
  pivot_longer(cols = c("swiatek i.", "sabalenka a.", "jabeur o.", "ostapenko j.", "rybakina e.", 
                        "kvitova p.", "garcia c.", "pegula j.", "bencic b.", "alexandrova e."),
               names_to = "name",
               values_to = "probability") 

#rayshader
pregraphw$name <- factor(pregraphw$name, levels = c("swiatek i.", "sabalenka a.", "jabeur o.", 
                                                    "ostapenko j.", "rybakina e.", "kvitova p.", 
                                                    "garcia c.", "pegula j.", "bencic b.", 
                                                    "alexandrova e."))

write_csv(pregraphw, "pregraphw.csv")