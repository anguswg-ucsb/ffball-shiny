
# library(tidyverse)
# library(nflfastR)
# library(DT)
# library(plotly)
# library(billboarder)



# LOAD IN SEASON DATA
season <- load_data(2020)
# saveRDS(season, file = "data/nfl-season-2020")

################## GAME LOG YARDS + FANTASY POINTS #####################
highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Yards"),
                          # min=0,
                          # max = max(p3$receiving_air_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "Fantasy points"),
                          min=0,
                          max = 50,
                          # max = max(wr1$target_share),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_add_series(rb1, name = "Rushing yards", type = "column", yaxis = 0, hcaes(x = week, y = rushing_yards, stack = "rushing_yards")) %>%
  hc_add_series(rb1, name = "Receiving yards", type = "column", yaxis = 0, hcaes(x = week, y = receiving_yards, stack = "rushing_yards")) %>%
  hc_add_series(rb1, name = "Passing yards",type = "column", yaxis = 0, hcaes(x = week, y = passing_yards, stack = "rushing_yards")) %>%
  hc_add_series(rb1, name = "Fantasy points",type = "spline", yaxis = 1, hcaes(x = week, y = fpts_hppr))

hc_yAxis_multiples(list(title = list(text = "Yards"),
                        # min=0,
                        # max = max(p3$receiving_air_yards),
                        showFirstLabel = TRUE,
                        showLastLabel = TRUE,
                        opposite = FALSE),
                   list(title = list(text = "Fantasy points"),
                        min=0,
                        max = 50,
                        # max = max(wr1$target_share),
                        showLastLabel=FALSE,
                        opposite = TRUE))
hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_yAxis(gridLineColor = list("transparent"))
hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')



#################### PLAYERS #######################

rb1 <- get_player_data(season, "Dalvin Cook", 1, 16)

wr1 <- get_player_data(season, "Terry McLaurin", 1, 16)

qb1 <- get_player_data(season, "Josh Allen", 1, 16)



##################### AIRYARDS #####################
highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Air yards"),
                          # min=0,
                          # max = max(p3$receiving_air_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "Target %"),
                          min=0,
                          max = 1,
                          # max = max(wr1$target_share),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_add_series(wr1, name = "Air yards", type = "column",
                hcaes(x = week, y = receiving_air_yards), yAxis = 0 ) %>%
  hc_add_series(wr1, name = "Receiving yards", type = "column",
                hcaes(x = week, y = receiving_yards), yAxis = 0) %>%
  # hc_add_series(wr1, name = "Catch rate", type = "line",
  #               hcaes(x = week, y = catch_rate, yAxis = 1 )) %>%
  hc_add_series(wr1, name = "Target share", type = "spline",
                hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("darkcyan", "lightblue", "darkred"))
  # hc_yAxis(min = 0) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')


################# TARGETS/RECEPTIONS #####################
highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Targets/Receptions"),
                          # min=0,
                          # max = max(p3$receiving_air_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "Target %"),
                          min=0,
                          max = 1,
                          # max = max(wr1$target_share),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_add_series(wr1, name = "Targets", type = "column",
                hcaes(x = week, y = targets), yAxis = 0) %>%
hc_add_series(wr1, name = "Receptions", type = "column",
              hcaes(x = week, y = receptions), yAxis = 0) %>%
  hc_add_series(wr1, name = "Target share", type = "spline",
                hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("lightblue", "darkcyan", "darkred"))


################# YARDS AFTER CATCH #####################
highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Targets/Receptions"),
                          # min=0,
                          # max = max(p3$receiving_air_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "Target %"),
                          min=0,
                          max = 1,
                          # max = max(wr1$target_share),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_add_series(wr1, name = "Receiving yards", type = "column",
                hcaes(x = week, y = receiving_yards), yAxis = 0) %>%
  hc_add_series(wr1, name = "Yards after catch", type = "column",
                hcaes(x = week, y = receiving_yards_after_catch), yAxis = 0) %>%
  hc_add_series(wr1, name = "Target share", type = "spline",
                hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("darkcyan", "lightblue", "darkred"))
  # hc_yAxis(min = 0) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')


################# YARDS PER CARRY/TOUCH + FPTS PER TOUCH #####################
highchart() %>%
    hc_yAxis_multiples(list(title = list(text = "Yards per touch"),
                            # min=0,
                            # max = max(p3$receiving_air_yards),
                            showFirstLabel = TRUE,
                            showLastLabel = TRUE,
                            opposite = FALSE),
                       list(title = list(text = "FPTS per touch"),
                            min=0,
                            max = 6,
                            # max = max(wr1$target_share),
                            showLastLabel=FALSE,
                            opposite = TRUE)) %>%
    hc_add_series(wr1, name = "Yards per touch", type = "column",
                  hcaes(x = week, y = yards_per_touch, yAxis = 0)) %>%
    hc_add_series(wr1, name = "Yards per carry", type = "column",
                hcaes(x = week, y = ypc), yAxis = 0) %>%
    hc_add_series(wr1, name = "Fantasy points per touch", type = "spline",
                  hcaes(x = week, y = fpts_pt), yAxis = 1) %>%
    hc_colors(c("darkcyan", "lightblue", "darkred"))
  # hc_yAxis(min = 0) %>%
    hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')

################## GAME LOG YARDS + FANTASY POINTS ###################
    hchart(df, name = "Rushing yards", type = "column", yaxis = 1, hcaes(x = week, y = rushing_yards)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_add_series(df, name = "Receiving yards", type = "column", yaxis = 1, hcaes(x = week, y = receiving_yards)) %>%
      hc_add_series(df, name = "Passing yards",type = "column",  yaxis = 1, hcaes(x = week, y = passing_yards)) %>%
      hc_add_series(df, name = "Fantasy points",type = "spline",  yaxis = 2, hcaes(x = week, y = fpts_hppr)) %>%
      hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL) %>%
      hc_colors(c("darkcyan", "lightblue", "darkseagreen", "darkred"))

  highchart() %>%
    hc_yAxis_multiples(list(title = list(text = "Yards"),
                            # min=0,
                            # max = max(p3$receiving_air_yards),
                            showFirstLabel = TRUE,
                            showLastLabel = TRUE,
                            opposite = FALSE),
                       list(title = list(text = "Fantasy points"),
                            min=0,
                            max = 50,
                            # max = max(wr1$target_share),
                            showLastLabel=FALSE,
                            opposite = TRUE)) %>%
    # hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_add_series(qb1, name = "Rushing yards", type = "column", yaxis = 0, hcaes(x = week, y = rushing_yards)) %>%
    hc_add_series(qb1, name = "Receiving yards", type = "column", yaxis = 0, hcaes(x = week, y = receiving_yards)) %>%
    hc_add_series(qb1, name = "Passing yards",type = "column", yaxis = 0, hcaes(x = week, y = passing_yards)) %>%
    hc_add_series(qb1, name = "Fantasy points",type = "spline", hcaes(x = week, y = fpts_hppr, yaxis = 1)) %>%
    hc_colors(c("darkcyan", "lightblue", "#1aadce", "darkred"))
    hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL) %>%

    hc_yAxis_multiples(list(title = list(text = "Yards"),
                            # min=0,
                            # max = max(p3$receiving_air_yards),
                            showFirstLabel = TRUE,
                            showLastLabel = TRUE,
                            opposite = FALSE),
                       list(title = list(text = "Fantasy points"),
                            min=0,
                            max = 50,
                            # max = max(wr1$target_share),
                            showLastLabel=FALSE,
                            opposite = TRUE))
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_yAxis(gridLineColor = list("transparent"))
    hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')

  ### YPC + FPTS/touch
      highchart() %>%
    hc_yAxis_multiples(list(title = list(text = "Yards per carry"),
                            # min=0,
                            # max = max(p3$receiving_air_yards),
                            showFirstLabel = TRUE,
                            showLastLabel = TRUE,
                            opposite = FALSE),
                       list(title = list(text = "Fantasy points per touch"),
                            min=0,
                            max = 5,
                            # max = max(wr1$target_share),
                            showLastLabel=FALSE,
                            opposite = TRUE)) %>%
    hc_add_series(rb1, name = "Yards per carry", type = "column",
                  hcaes(x = week, y = ypc), yAxis = 0) %>%
    hc_add_series(rb1, name = "Fantasy points per touch", type = "line",
                  hcaes(x = week, y = fpts_pt), yAxis = 1) %>%
    hc_colors(c("darkcyan", "darkred"))


# target <- season %>%
#   group_by(recent_team, week) %>%
#   mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
#   ungroup() %>%
#   add_count() %>%
#   mutate(rush_yards_pg = (sum(rushing_yards)/nrow(.)),
#          recieve_yards_pg = (sum(receiving_yards)/nrow(.)),
#          pass_yards_pg = (sum(passing_yards)/nrow(.)),
#          tot_fpts = sum(fpts_hppr),
#          tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
#          tot_recept = sum(receptions),
#          tot_targ = sum(targets),
#          tot_carries = sum(carries),
#          tot_touch = tot_recept+ tot_carries,
#          fpts_pg = (sum(fpts_hppr)/n),
#          tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
#          tot_recept = sum(receptions),
#          tot_targ = sum(targets),
#          targ_pg = tot_targ/n,
#          recept_pg = tot_recept/n,
#          avg_dot = receiving_air_yards/targets,
#          carries_pg = tot_carries/n,
#          airyards_pg = sum(receiving_air_yards)/n,
#          fpts_pt = fpts_pg/(recept_pg + carries_pg),
#          yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
#          passyards_pg = sum(passing_yards)/n,
#          td_int_ratio = sum(passing_tds)/sum(interceptions),
#          ypc = rushing_yards/carries,
#          target_share = targets/team_targets)


rank_test <- season %>%
  filter(position == p2$position[1]) %>%
  group_by(player_id) %>%
  add_count(name = "count1") %>%
  filter(week >= 1, week <= 16) %>%
  add_count(name = "count2") %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         fpts_pg = tot_fpts/count2,
         targ_pg = tot_targ/count2,
         recept_pg = tot_recept/count2) %>%
  slice(n = 1) %>%
  arrange(-recept_pg) %>%
  ungroup() %>%
  slice(n = 1:36)

# pivot longer for billboarder plot
rank <- rank %>%
  pivot_longer(50:51, names_to = "rcpt_str", values_to = "rcpt_val")
# round values to 2 decimals
rank$rcpt_val <- round(rank$rcpt_val, 2)

billboarder::billboarder() %>%
  bb_barchart(data = rank,
              mapping = bbaes(x = full_name, y = rcpt_val, group = rcpt_str))

highchart() %>%
  hc_add_series(rank, type = "column", hcaes(x = full_name, y = recept_pg)) %>%
  hc_add_series(rank, type = "column", hcaes(x = full_name, y = targ_pg)) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%') %>%
  hc_xAxis(categories = rank$full_name)

################### PLAYER AIR YARDS + RECIEVE YARDS ####################
wr1 <- get_player_data(season, "A.J. Brown", 1, 16)
air_yards <- wr1 %>%
  pivot_longer(c(33:34, 52), names_to = "air_yards_str", values_to = "air_yards_val")

billboarder::billboarder() %>%
  bb_barchart(data = air_yards,
              mapping = bbaes(x = week, y = air_yards_val, group = air_yards_str))


####################### LEAGUE RANKINGS ##########################
####################### YPC RANK ##########################

rank_touch <- season %>%
  filter(position == "RB") %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(rush_yards_pg = (sum(rushing_yards)/nrow(.)),
         recieve_yards_pg = (sum(receiving_yards)/nrow(.)),
         pass_yards_pg = (sum(passing_yards)/nrow(.)),
         tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         tot_carries = sum(carries),
         tot_touch = tot_recept+ tot_carries,
         fpts_pg = (sum(fpts_hppr)/n),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         avg_dot = receiving_air_yards/targets,
         carries_pg = tot_carries/n,
         airyards_pg = sum(receiving_air_yards)/n,
         fpts_pt = fpts_pg/(recept_pg + carries_pg),
         yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
         passyards_pg = sum(passing_yards)/n,
         td_int_ratio = sum(passing_tds)/sum(interceptions),
         ypc = sum(rushing_yards)/tot_carries,
         yards_per_touch = (sum(rushing_yards) + sum(receiving_yards))/tot_touch,
         fpts_per_touch = tot_fpts/tot_touch) %>%
  ungroup() %>%
  filter(carries_pg >= 6.25) %>%
  # mutate(total_carries = sum(carries), std = abs(tot_carries - mean(tot_carries)) / sd(tot_carries)) %>%
  # # filter(std >= 0.6) %>%
  group_by(player_id) %>%
  slice(n = 1) %>%
  ungroup() %>%
  mutate(avg_ypc = mean(ypc), avg_ypt = mean(yards_per_touch)) %>%
  arrange(-yards_per_touch) %>%
  slice(n = 1:36)

highchart() %>%
  hc_add_series(rank_touch, name = "Yards per touch", type = "column",
                hcaes(x = full_name, y = yards_per_touch), yAxis = 0 ) %>%
  hc_add_series(rank_touch, name = "League average", type = "spline",
                hcaes(x = full_name, y = avg_ypt)) %>%
  hc_colors(c("darkcyan", "darkred")) %>%
  hc_xAxis(categories = rank_touch$full_name)
  # hc_add_series(wr1, name = "Catch rate", type = "line",
  #               hcaes(x = week, y = catch_rate, yAxis = 1 )) %>%
  hc_add_series(wr1, name = "Target share", type = "spline",
                hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("darkcyan", "lightblue", "darkred"))
# hc_yAxis(min = 0) %>%
hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')

####################### TARGET % RANK ##########################

rank_targ <- season %>%
  filter(week >= 1, week <= 16) %>%
  group_by(recent_team) %>%
  mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
  ungroup() %>%
  filter(position == wr1$position[1]) %>%
  group_by(player_id) %>%
  add_count(name = "count1") %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         fpts_pg = tot_fpts/count2,
         targ_pg = tot_targ/count2,
         recept_pg = tot_recept/count2,
         avg_targ_share = tot_targ/team_targets) %>%
  slice(n = 1) %>%
  arrange(-avg_targ_share) %>%
  ungroup() %>%
  slice(n = 1:24)

highchart() %>%
  hc_add_series(rank_targ, name = "Target share", type = "column",
                hcaes(x = full_name, y = avg_targ_share), yAxis = 0 ) %>%
  hc_colors(c("darkcyan")) %>%
  hc_xAxis(categories = rank_targ$full_name) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')
# hc_add_series(wr1, name = "Catch rate", type = "line",
#               hcaes(x = week, y = catch_rate, yAxis = 1 )) %>%
hc_add_series(wr1, name = "Target share", type = "spline",
              hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("darkcyan", "lightblue", "darkred"))
# hc_yAxis(min = 0) %>%
hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')
####################### AIR YARDS RANK ##########################

rank_airyards <- season %>%
  filter(position == wr1$position[1]) %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         tot_carries = sum(carries),
         tot_touch = tot_recept+ tot_carries,
         fpts_pg = tot_fpts/n,
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         carries_pg = tot_carries/n,
         airyards_pg = sum(receiving_air_yards)/n,
         fpts_pt = fpts_pg/(recept_pg + carries_pg),
         yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
         passyards_pg = sum(passing_yards)/n,
         td_int_ratio = sum(passing_tds)/sum(interceptions)) %>%
  slice(n = 1) %>%
  filter(tot_touch >= 100 | tot_recept >=60) %>%
  arrange(-airyards_pg) %>%
  ungroup() %>%
  mutate(rank = 1:n(),
         max = max(airyards_pg),
         med = median(airyards_pg),
         min = min(airyards_pg)) %>%
  slice(n = 1:36)

# billboarder::billboarder() %>%
#   bb_barchart(data = rank_airyards,
#               mapping = bbaes(x = full_name, y = airyards_pg)) %>%
#   bb_title(text = "AIR YARDS PER GAME") %>%
#   bb_axis(y = list(label = list(text = "YARDS", position = "middle")))
#
# ### bubble chart
# ggplotly(ggplot(rank_airyards, aes(x = fpts_pg, y = airyards_pg)) +
#            geom_point(aes(size = tot_recept, col = full_name)))





# PLAYER AIR YARDS + REC. YARDS
ggplot(p2, aes(x = week, y = receiving_yards)) +
  geom_col()


# # Fantasy points + RUSH YARDS + PASS + REC  in game in weeks
# plot_ly(player, x = ~week, y = ~rushing_yards,
#             type = 'bar',
#             textposition = 'auto',
#             name = "RUSHING",
#             marker = list(color = 'rgba(219, 64, 82, 0.7)',
#             line = list(color = 'rgba(219, 64, 82, 1.0)',
#             width = 2))) %>%
#   add_trace(player, x = ~week, y = ~receiving_yards,
#             type = 'bar',
#             name = "RECIEVING",
#             marker = list(color = 'rgba(10, 150, 24, 0.7)',
#             line = list(color = 'rgba(55, 128, 191, 0.7)',
#             width = 2)))%>%
#   add_trace(player, x = ~week, y = ~passing_yards,
#             type = 'bar',
#             name = "PASSING",
#             marker = list(color = 'rgba(55, 128, 191, 0.7)',
#             line = list(color = 'rgba(55, 128, 191, 0.7)',
#             width = 2))) %>%
#   add_trace(player, x = ~week, y = ~fpts_hppr,
#             type = "scatter",
#             mode = "lines",
#             name = "FPTS",
#             # color = I("green"),
#             line = list(color = 'rgba(0, 0, 0, 1)',
#                         width = 4)) %>%
#   # add_segments(x = min(~week), xend = max(~week), y = ~rush_per_game, yend = ~rush_per_game) %>%
#   layout(barmode = "stack")

################### PLAYER YPC & YARDS AFTER CONTACT ####################
rb1 <- get_player_data(season, "Dalvin Cook", 1, 10)
rb2 <- rb1 %>%
  mutate(fpts_per_touch = fpts_hppr/(receptions + carries))

rb2 <- rb2 %>%
  pivot_longer(c(62, 63), names_to = "ypc_str", values_to = "ypc_val")

billboarder::billboarder() %>%
  bb_linechart(data = rb2,
              mapping = bbaes(x = week, y = ypc_val, group = ypc_str))

               # , mapping = bbaes(x = week, y = carries))


billboarder::billboarder() %>%
  bb_barchart(data = air_yards,
              mapping = bbaes(x = week, y = air_yards_val, group = air_yards_str))

####################### YPC RANK ##########################

rank_touch <- season %>%
  filter(position == "RB") %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(rush_yards_pg = (sum(rushing_yards)/nrow(.)),
         recieve_yards_pg = (sum(receiving_yards)/nrow(.)),
         pass_yards_pg = (sum(passing_yards)/nrow(.)),
         tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         tot_carries = sum(carries),
         tot_touch = tot_recept+ tot_carries,
         fpts_pg = (sum(fpts_hppr)/n),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         avg_dot = receiving_air_yards/targets,
         carries_pg = tot_carries/n,
         airyards_pg = sum(receiving_air_yards)/n,
         fpts_pt = fpts_pg/(recept_pg + carries_pg),
         yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
         passyards_pg = sum(passing_yards)/n,
         td_int_ratio = sum(passing_tds)/sum(interceptions),
         ypc = sum(rushing_yards)/tot_carries,
         yards_per_touch = (sum(rushing_yards) + sum(receiving_yards))/tot_touch,
         fpts_per_touch = tot_fpts/tot_touch) %>%
  ungroup() %>%
  filter(carries_pg >= 6.25) %>%
  # mutate(total_carries = sum(carries), std = abs(tot_carries - mean(tot_carries)) / sd(tot_carries)) %>%
  # # filter(std >= 0.6) %>%
  group_by(player_id) %>%
  slice(n = 1) %>%
  ungroup() %>%
  mutate(avg_ypc = mean(ypc)) %>%
  arrange(-ypc) %>%
  slice(n = 1:36)

highchart() %>%
  hc_yAxis_multiples(list(title = list(text = "Air yards"),
                          # min=0,
                          # max = max(p3$receiving_air_yards),
                          showFirstLabel = TRUE,
                          showLastLabel = TRUE,
                          opposite = FALSE),
                     list(title = list(text = "Target %"),
                          min=0,
                          max = 1,
                          # max = max(wr1$target_share),
                          showLastLabel=FALSE,
                          opposite = TRUE)) %>%
  hc_add_series(wr1, name = "Air yards", type = "column",
                hcaes(x = week, y = receiving_air_yards), yAxis = 0 ) %>%
  hc_add_series(wr1, name = "Receiving yards", type = "column",
                hcaes(x = week, y = receiving_yards), yAxis = 0) %>%
  # hc_add_series(wr1, name = "Catch rate", type = "line",
  #               hcaes(x = week, y = catch_rate, yAxis = 1 )) %>%
  hc_add_series(wr1, name = "Target share", type = "spline",
                hcaes(x = week, y = target_share), yAxis = 1) %>%
  hc_colors(c("darkcyan", "lightblue", "darkred"))
# hc_yAxis(min = 0) %>%
hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = '100%')

# rank_ypc <- rename(rank_ypc, "Fantasy points per touch" = fpts_per_touch, "Yards per carry" = ypc, "Yards per touch" = yards_per_touch)
# rank_ypc2 <- rank_ypc %>%
#   pivot_longer(64:65, names_to = "fpts_touch_str", values_to = "fpts_touch_val")
#
# billboarder::billboarder() %>%
#   bb_barchart(data = rank_ypc2,
#               mapping = bbaes(x = full_name, y = fpts_touch_val, group = fpts_touch_str))

lmYPC <- lm(fpts_pg~airyards_pg, data = rank_ypc)

summary(lmYPC)

 rank_ypc2 <- season %>%
  filter(position == rb1$position[1]) %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         fpts_pg = tot_fpts/n,
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         ypc = rushing_yards/carries,
         tot_carries = sum(carries)) %>%
  filter(tot_carries >= 150) %>%
  slice(n = 1) %>%
  arrange(-ypc) %>%
  ungroup()
  # filter(n >= 5) %>%
  # slice(n = 1:30)
# PLAYER PROFILE INFO FUNCTION

########### YPC LEAGUE RANK #############
billboarder::billboarder() %>%
  bb_barchart(data = rank_ypc2,
              mapping = bbaes(x = full_name, y = ypc))

qb_rank <- season %>%
  filter(position == "QB") %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(total_fpts = sum(fpts_hppr),
         total_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds),
         fpts_pg = total_fpts/n) %>%
  slice(n = 1) %>%
  arrange(-fpts_pg) %>%
  ungroup() %>%
  filter(n >= 5)

jenks_df <- BAMMtools::getJenksBreaks(qb_rank$fpts_pg, 4) %>%
  data.frame() %>%
  mutate(brk = letters[1:4]) %>%
  rename(jenk = ".") %>%
  pivot_wider(jenk, names_from = "brk", values_from = "jenk")

qb_rank <- bind_cols(qb_rank, jenks_df)

qb_rank <- qb_rank %>%
  filter(full_name == "Tom Brady") %>%
  select(fpts_pg, a, b, c, d) %>%
  round(2)

############## YARDS PER GAME GAUGE ##############
rank_pass <- season %>%
  filter(position == df2$position[1]) %>%
  filter(week >= input$weekRange[1], week <= input$weekRange[2]) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         fpts_pg = tot_fpts/n,
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
         passyards_pg = sum(passing_yards)/n,
         td_int_ratio = sum(passing_tds)/sum(interceptions)) %>%
  slice(n = 1) %>%
  arrange(-passyards_pg) %>%
  ungroup() %>%
  filter(n >= 5)

jenks_df <- BAMMtools::getJenksBreaks(rank_pass$passyards_pg, 4) %>%
  data.frame() %>%
  mutate(brk = letters[1:4]) %>%
  rename(jenk = ".") %>%
  pivot_wider(jenk, names_from = "brk", values_from = "jenk")

rank_pass <- bind_cols(rank_pass, jenks_df)

rank_pass <- rank_pass %>%
  filter(player_id == df2$player_id[1]) %>%
  select(passyards_pg, a, b, c, d) %>%
  round(2)



library(BAMMtools)
rm(jenks)
rm(jenk_df)

jenks_df <- BAMMtools::getJenksBreaks(rank_yards$yards_pg, 6) %>%
  data.frame() %>%
  mutate(brk = LETTERS[1:4]) %>%
  rename(jenk = ".") %>%
  pivot_wider(jenk, names_from = "brk", values_from = "jenk")

jenk_df <- data.frame(jenks) %>%
  mutate(brk = letters[1:4]) %>%
  pivot_wider(jenks, names_from = "brk", values_from = "jenks")


jenks[1]


ggplot(wr1, aes(x = week, y = week)) +
  ggimage::geom_image(aes(image = headshot_url))


rb1 <- season %>%
  filter(full_name == "Antonio Gibson") %>%
  select(1, 43, 42, 2:39) %>%
  filter(week >= 1, week <= 16) %>%
  add_count() %>%
  mutate(rush_yards_pg = (sum(rushing_yards)/nrow(.)),
         recieve_yards_pg = (sum(receiving_yards)/nrow(.)),
         pass_yards_pg = (sum(passing_yards)/nrow(.)),
         fpts_pg = (sum(fpts_hppr)/n),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         recept_pg = tot_recept/n,
         targ_pg = tot_targ/n,
         yards_pg = (sum(rushing_yards) + sum(receiving_yards))/n)

qb1 <- player_data(season, "Kyler Murray", 1, 16)
rb1 <- player_data(season, "Josh Jacobs", 1, 16)
qb2 <- player_data(season, "Tom Brady", 1, 16)

rank_fpts <- season %>%
  filter(position == rb1$position[1]) %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         fpts_pg = tot_fpts/n,
         targ_pg = tot_targ/n,
         recept_pg = tot_recept/n,
         yards_pg = (sum(rushing_yards) + sum(receiving_yards))/n)) %>%
  slice(n = 1) %>%
  arrange(-fpts_pg) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  slice(n = 1:30)

rank_fpts$fpts_pg <- round(rank_fpts$fpts_pg, 2)
# rank_fpts <- rank_fpts %>%
#   pivot_longer(50:51, names_to = "rcpt_str", values_to = "rcpt_val")

billboarder::billboarder() %>%
  bb_barchart(data = rank_fpts,
              mapping = bbaes(x = full_name, y = fpts_pg))


highchart() %>%
  hc_add_series(ranks, type = "column", hcaes(x = full_name, y = fpts_pg)) %>%
  hc_xAxis(categories = ranks$full_name)

# find position rank
ranked <- season %>%
  filter(position == p2$position[1]) %>%
  filter(week >= 1, week <= 16) %>%
  group_by(player_id) %>%
  add_count() %>%
  mutate(total_fpts = sum(fpts_hppr),
         total_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds),
         fpts_pg = total_fpts/n) %>%
  slice(n = 1) %>%
  arrange(-fpts_pg) %>%
  ungroup() %>%
  mutate(rank = 1:n(),
         max = max(fpts_pg),
         med = median(fpts_pg),
         min = min(fpts_pg)) %>%
  filter(player_id == wr1$player_id[1]) %>%
  select(fpts_pg, min, max, med) %>%
  round(2)

ranks2$full_name2 <- factor(ranks2$full_name)

highchart() %>%
  hc_add_series(ranks2, type = "column", hcaes(x = full_name, y = fpts_pg)) %>%
  hc_xAxis(categories = ranks2$full_name)

ranks2 <- ranked %>%
  filter(n >= 5) %>%
  slice(n = 1:24)

ggplot(ranks2, aes(x = reorder(full_name, -fpts_pg), y = fpts_pg)) +
  geom_col()

ranked$med
(ranked[4] - ranked[2]) /2
max(ranked$fpts_pg)
((ranked[4] - ranked[2])/2) + 1
m <- filter(ranked, player_id == p1$player_id[1])
n <-

p_rank <- ranked %>%
  filter(player_id == p2$player_id[1]) %>%
  select(rank)


tot_fpts_rank <- ranked %>%
  arrange(-total_fpts) %>%
  ungroup() %>%
  mutate(rank = 1:n())

fppg_rank <- ranked %>%
  filter(n >= 5) %>%
  arrange(-fpts_pg) %>%
  ungroup() %>%
  mutate(rank = 1:n())

tds_rank <- ranked %>%
  arrange(-total_tds) %>%
  ungroup() %>%
  mutate(rank = 1:n())

################## PLAYER RECEPTIONS + TARGETS ####################

rb1 <- get_player_data(season, "Jonathan Taylor", 1, 16)

# plotly
plot_ly(wr1, x = ~week, y = ~targets,
        type = 'bar',
        textposition = 'auto',
        name = "targets",
        marker = list(color = 'rgba(219, 64, 82, 0.7)',
                      line = list(color = 'rgba(219, 64, 82, 1.0)',
                                  width = 2))) %>%
  add_trace(wr1, x = ~week, y = ~receptions,
            type = 'bar',
            name = "receptions",
            marker = list(color = 'rgba(10, 150, 24, 0.7)',
                          line = list(color = 'rgba(55, 128, 191, 0.7)',
                                      width = 2)))
# highcharter
highchart() %>%
  hc_add_series(wr1, type = "column", hcaes(x = week, y = targets)) %>%
  hc_add_series(wr1, type = "column", hcaes(x = week, y = receptions)) %>%
  highcharter::hc_labels()
# hc_xAxis(categories = ranks2$full_name)

# pivot longer for billboarder plot
p2 <- wr1 %>%
  pivot_longer(29:28, names_to = "rcpt_str", values_to = "rcpt_val")

billboarder::billboarder() %>%
  bb_barchart(data = p2,
              mapping = bbaes(x = week, y = rcpt_val, group = rcpt_str))

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











################## HEADSHOT + PROFILE ###############






  roster <- fast_scraper_roster(2020) %>%
    filter(position %in% c("QB", "RB", "TE", "WR"))

  roster <- df2 %>%
    select(full_name, headshot_url) %>%
    filter(full_name == df1$full_name[1])
  url <- as.character(df2[1,2])
  url

    df2 <- headshotData()
    pic <- df2
    tags$img(src = pic, width = 150, height = 150)

    roster <- roster %>%
      filter(full_name == wr1$full_name[1])
    info <- roster %>%
      select(full_name, team, position, birth_date, height, weight, college, headshot_url)

    info <- info %>%
      mutate(across(1:7, as.character)) %>%
      mutate(Photo = paste0("<img src=", dQuote(headshot_url), " height = \"52\"></img>")) %>%
      select(Photo, full_name, team, position, birth_date, height, weight, college)

    info <- info %>%
      rename("Name" = "full_name",
             "Team" = 'team',
             "Position" = "position",
             "Birth Date" = "birth_date",
             "Height" = "height",
             "Weight" = "weight",
             "College" = "college")

    info <- info %>% pivot_longer(1:8, names_to = "____", values_to = "___")

    customGreen0 = "#DeF7E9"

    customGreen = "#71CA97"

    customRed = "#ff7f7f"
formattable(info2, align =c("l","l"),
            # list( `____`= color_bar(customGreen)))
list(`____` = formatter(
  "span", style = ~ style(color = "black", font.weight = "bold")),
  `___`= color_bar(customGreen0)))
  formattable::color_bar()
DT::datatable(info, escape = FALSE)








