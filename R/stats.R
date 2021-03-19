
library(tidyverse)
library(nflfastR)
library(DT)
library(plotly)

# LOAD IN SEASON DATA
season <- load_data(2020)

# Get player stats to plot --- FPTS + RUSH + PASS + REC YARDS IN EACH GAME FOR GIVEN WEEKS
  filter(full_name == "Lamar Jackson") %>%
  select(1:5, 42, 8, 20, 28, 37:40) %>%
  # filter(week >= 4, week <= 10) %>%
  mutate(rush_per_game = (sum(rushing_yards)/nrow(.)),
         rec_per_game = (sum(receiving_yards)/nrow(.)),
         pass_per_game = (sum(passing_yards)/nrow(.)),
         fpts_per_game = (sum(fpts_hppr)/nrow(.)))

player_data <- function(df_season, name) {
  player2 <- df_season %>%
  filter(full_name == as.character(name)) %>%
  select(1:5, 42, 8, 20, 28, 37:40) %>%
  # filter(week >= 4, week <= 10) %>%
  mutate(rush_per_game = (sum(rushing_yards)/nrow(.)),
         rec_per_game = (sum(receiving_yards)/nrow(.)),
         pass_per_game = (sum(passing_yards)/nrow(.)),
         fpts_per_game = (sum(fpts_hppr)/nrow(.)))
}



# Fantasy points + RUSH YARDS + PASS + REC  in game in weeks
plot_ly(player, x = ~week, y = ~rushing_yards,
            type = 'bar',
            textposition = 'auto',
            name = "RUSHING",
            marker = list(color = 'rgba(219, 64, 82, 0.7)',
            line = list(color = 'rgba(219, 64, 82, 1.0)',
            width = 2))) %>%
  add_trace(player, x = ~week, y = ~receiving_yards,
            type = 'bar',
            name = "RECIEVING",
            marker = list(color = 'rgba(10, 150, 24, 0.7)',
            line = list(color = 'rgba(55, 128, 191, 0.7)',
            width = 2)))%>%
  add_trace(player, x = ~week, y = ~passing_yards,
            type = 'bar',
            name = "PASSING",
            marker = list(color = 'rgba(55, 128, 191, 0.7)',
            line = list(color = 'rgba(55, 128, 191, 0.7)',
            width = 2))) %>%
  add_trace(player, x = ~week, y = ~fpts_hppr,
            type = "scatter",
            mode = "lines",
            name = "FPTS",
            # color = I("green"),
            line = list(color = 'rgba(0, 0, 0, 1)',
                        width = 4)) %>%
  # add_segments(x = min(~week), xend = max(~week), y = ~rush_per_game, yend = ~rush_per_game) %>%
  layout(barmode = "stack")

# PLAYER PROFILE INFO FUNCTION
install.packages("ggimage")

p1 <- player_data(season, "Tom Brady")

ggplot(p1, aes(x = week, y = week)) +
  ggimage::geom_image(aes(image = headshot_url))












# find position rank
position_rank <- season %>%
  filter(position == tmp1)
  group_by(player_id) %>%
  filter(week >= 4, week <= 10) %>%
  mutate(total_fpts = sum(fpts_hppr)) %>%
  group_by(full_name) %>%
  slice(n = 1) %>%
  arrange(-total_fpts) %>%
  ungroup() %>%
  mutate(rank = 1:n())

# player2 <- season %>%
#   filter(full_name == "Dalvin Cook") %>%
#   select(1:5, 8, 20, 28, 37:40)

plot_ly(player2, x = ~week, y = ~rushing_yards, type = 'bar',
        textposition = 'auto',
        name = "Rush",
        marker = list(color = 'rgba(219, 64, 82, 0.7)',
                      line = list(color = 'rgba(219, 64, 82, 1.0)',
                                  width = 2))) %>%
  add_trace(player2, x = ~week,
            y = ~receiving_yards,
            type = 'bar',
            name = "Rec",
            marker = list(color = 'rgba(55, 128, 191, 0.7)',
                          line = list(color = 'rgba(55, 128, 191, 0.7)',
                                      width = 2))) %>%
  add_trace(player2, x = ~week,
            y = ~fpts_hppr, type = "scatter",
            mode = "lines",
            color = I("black")) %>%
              # list(color = 'rgb(49,130,189)')) %>%
  # add_lines(player2, x = ~week, y = ~fpts_hppr, inherit = TRUE) %>%
  layout(barmode = "stack")
  add_markers(player2, x = ~week,
              y = ~fpts_hppr, color = "black")
  add_trace(player2, x = ~week,
            y = ~fpts_hppr, type = "scatter", mode = "lines", fill = "black")
  layout(
         xaxis = list(title = "",
                      tickfont = list(size = 14,color = 'rgb(107, 107, 107)')),
         yaxis = list(title = 'Yards',
                      titlefont = list(size = 16, color = 'rgb(107, 107, 107)'),
                      tickfont = list(size = 14,color = 'rgb(107, 107, 107)')),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group',
         bargap = 0.2,
         bargroupgap = 0.15) %>%
  config(displayModeBar = FALSE)

# player2 <- player2 %>%
#   pivot_longer(c(6:8), names_to = "yard_type", values_to = "yards")

player3 <- player2 %>%
  mutate(yards_per_game = sum(yards)/max(week))

ggplot(player3, aes(x = week, y = yards)) +
  geom_col(aes(fill = yard_type)) +
  geom_col(aes(y = yards_per_game))

highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Yards"),
                          min=0,
                          max = max(player$rushing_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "FPTS (0.5 PPR)"),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_add_series(player, type = "column", hcaes(x = week, y = rushing_yards)) %>%
  hc_add_series(player, type = "line",
                hcaes(x = week, y = fpts_hppr), yAxis = 0) %>%
  hc_colors(c("darkcyan", "darkred")) %>%
  hc_size(height = 450)

highchart() %>%
  hc_add_series(player, type = "column", hcaes(x = week, y = rushing_yards)) %>%
  hc_add_series(player, type = "column", hcaes(x = week, y = receiving_yards))  %>%
  hc_add_series(player, type = "column", hcaes(x = week, y = passing_yards)) %>%
  hc_colors(c("darkcyan", "darkred")) %>%
  hc_size(height = 450)

# for ( col in 1:ncol(water_use)){
#   colnames(water_use)[col] <-  sub("_.*", "", colnames(water_use)[col])
# }
#
# water_use <- water_use %>%
#   mutate(Public = public + domestic,
#          Thermoelectric = total,
#          Irrigation = livestock1 + livestock2 + aquacultere1 + irrigation1 + irrigation2) %>%
#   select(1:6, Public, Irrigation, Industrial = industrial, Mining = mining,  Thermoelectric)
# water_use <- water_use %>%
#   tidyr::unite('fips', statefips, countyfips)









rosters <- nflfastR::fast_scraper_roster(2020) %>%
  filter(position %in% c("QB", "RB", "TE", "WR")) %>% select(gsis_id)

names <- substr(rosters$first_name, start = 1, stop = 1)

rosters2 <- rosters

rosters2$name_abbr <- paste0(substr(rosters$first_name, start = 1, stop = 1), ".")
rosters2$player_name <- paste0(rosters2$name_abbr, rosters2$last_name)

players3 <- inner_join(player, rosters2(select, position, full_name), by = "player_name")
inner_join(
  nflfastR::fast_scraper_roster(2020) %>% filter(position == "WR") %>% select(gsis_id),
  by = c("player_id" = "gsis_id")
)


team_yards <- stats %>%
  group_by(recent_team) %>%
  summarise(rec_yards = sum(receiving_yards), rush_yards = sum(rushing_yards), total = rush_yards+rec_yards)

ggplot(team_yards, aes(x = rush_yards, y = rec_yards)) +
  geom_point(aes(color = recent_team, size = total))


fantasy <- load_pbp(2020) %>%
  filter(week <= 16) %>%
  calculate_player_stats() %>%
  mutate(
    ppg = fantasy_points_ppr / games
  )  %>%
  only keep the WRs
  inner_join(
    nflfastR::fast_scraper_roster(2020) %>% filter(position == "WR") %>% select(gsis_id),
    by = c("player_id" = "gsis_id")
  ) %>%
  arrange(-ppg)
# %>%
#   select(player_name, recent_team, games, fantasy_points_ppr, ppg) %>%
#   utils::head(10) %>%
#   knitr::kable(digits = 1)


# get_after_period <- function(my_vector) {
#
#   # Return a string vector without the characters
#   # before a period (excluding the period)
#
#   # my_vector, a string vector
#
#   str_sub(my_vector, str_locate(my_vector, "\\.")[,1]+1)
#
# }
# stats$last_name <- get_after_period(stats$player_name)
# stats <- inner_join(stats, select(rosters, last_name, position), by = "last_name")




  # names <- substr(rosters$first_name, start = 1, stop = 1)
  # rosters2 <- rosters
  # rosters2$name_abbr <- paste0(substr(rosters$first_name, start = 1, stop = 1), ".")
  # rosters2$player_name <- paste0(rosters2$name_abbr, rosters2$last_name)








