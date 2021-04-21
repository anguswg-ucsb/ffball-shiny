

# saveRDS(season,'C:\\Users\\angus\\OneDrive\\Desktop\\github\\ffball-shiny\\app\\nfl-season-2020.rds')
# Loads play-by-play data and calculates_player_stats for year input
load_data <- function(year) {
    pbp <- load_pbp(year)

    stats <- calculate_player_stats(pbp,weekly = T)

    player <- stats %>%
      filter(week <= 16) %>%
      mutate(fpts_hppr = fantasy_points + (receptions*0.5),
             junk1 = sum(fpts_hppr)/max(week),
             junk2 = sum(rushing_yards)/max(week))

    rosters <- fast_scraper_roster(year) %>%
             filter(position %in% c("QB", "RB", "TE", "WR"))

    player <- left_join(player, select(rosters, position, full_name, gsis_id), by = c("player_id" = "gsis_id"))
    # player <- left_join(player, select(rosters, position, full_name, gsis_id, 5, 10:14, 24), by = c("player_id" = "gsis_id"))
}

# Make data table of player's season totals
make_dt <- function(season_df) {
    player_dt <- season_df %>%
      group_by(full_name) %>%
      summarise(across(6:39, sum))
    datatable(player_dt)

}

# Get player stats to plot --- FPTS + RUSH + PASS + REC YARDS for each game in given week range
player_data <- function(df_season, name, start, end) {
  player2 <- df_season %>%
    filter(full_name == as.character(name)) %>%
    select(1, 43, 2:5, 42, 8, 20, 28, 37:40) %>%
    filter(week >= start, week <= end) %>%
    mutate(rush_per_game = (sum(rushing_yards)/nrow(.)),
           rec_per_game = (sum(receiving_yards)/nrow(.)),
           pass_per_game = (sum(passing_yards)/nrow(.)),
           fpts_per_game = (sum(fpts_hppr)/nrow(.)))
}

get_player_data <- function (df_season, name, start, end) {
  p1 <- df_season %>%
    # filter(week >= start, week <= end) %>%
    group_by(recent_team, week) %>%
    mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
    ungroup() %>%
    filter(full_name == as.character(name)) %>%
    select(1, 43, 42, 2:39, 44, 45) %>%
    filter(week >= start, week <= end) %>%
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
           fpts_pt = fpts_hppr/(receptions + carries),
           yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
           passyards_pg = sum(passing_yards)/n,
           td_int_ratio = sum(passing_tds)/sum(interceptions),
           ypc = rushing_yards/carries,
           yards_per_touch = (rushing_yards + receiving_yards)/(carries + receptions),
           catch_rate = receptions/targets,
           compl_percent = sum(completions)/sum(attempts),
           compl_percent2 = completions/attempts)

  p1 <- p1 %>%
    mutate(across(where(is.numeric), round, 2))
}

# plot player_data() data
make_player_plot <- function(df) {
  # Fantasy points + RUSH YARDS + PASS + REC  in game in weeks
  highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = "Yards and Fantasy points",
               align = "center") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_yAxis(title = list(text = "Yards/Fantasy points"), min = 0) %>%
      hc_add_series(df, name = "Rushing yards", type = "column", yaxis = 0, hcaes(x = week, y = rushing_yards)) %>%
      hc_add_series(df, name = "Receiving yards", type = "column", yaxis = 0, hcaes(x = week, y = receiving_yards)) %>%
      hc_add_series(df, name = "Passing yards",type = "column", yaxis = 0, hcaes(x = week, y = passing_yards)) %>%
      hc_add_series(df, name = "Fantasy points",type = "line", hcaes(x = week, y = fpts_hppr, yaxis = 0)) %>%
      hc_colors(c("darkcyan", "lightblue", "#1aadce", "darkred")) %>%
      hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  # highchart() %>%
  #   hc_add_theme(hc_theme_smpl()) %>%
  #   hc_yAxis_multiples(list(title = list(text = "Yards"),
  #                           showFirstLabel = TRUE,
  #                           showLastLabel = TRUE,
  #                           opposite = FALSE),
  #                      list(title = list(text = "Fantasy points"),
  #                           min=0,
  #                           max = 50,
  #                           showLastLabel=FALSE,
  #                           opposite = TRUE)) %>%
  #   # hc_plotOptions(column = list(stacking = "normal")) %>%
  #   hc_add_series(df, name = "Rushing yards", type = "column", yaxis = 0, hcaes(x = week, y = rushing_yards)) %>%
  #   hc_add_series(df, name = "Receiving yards", type = "column", yaxis = 0, hcaes(x = week, y = receiving_yards)) %>%
  #   hc_add_series(df, name = "Passing yards",type = "column", yaxis = 0, hcaes(x = week, y = passing_yards)) %>%
  #   hc_add_series(df, name = "Fantasy points",type = "spline", hcaes(x = week, y = fpts_hppr, yaxis = 1)) %>%
  #   hc_colors(c("darkcyan", "lightblue", "#1aadce", "darkred")) %>%
  #   hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

}

# make formattable table Player Profile info
make_profile = function(df){
  datatable(df, fillContainer = TRUE,
            colnames = c(" " = "class", " " = "vals"),
            options = list(dom = "t"),
            rownames = FALSE)
  # formattable(df, align =c("l","l"),
  #             # list( `____`= color_bar(customGreen)))
  #             list(`____` = formatter(
  #               "span", style = ~ style(color = "black", font.weight = "bold"))))

  # list(`Name` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold")),
  #      `Team` = color_tile("cornsilk", "darkgoldenrod1"),
  #      `Position` = color_tile("lightpink", "tomato"),
  #      `Birth Date` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold"))))

  # Make an interactive Table! with a caption
  # datatable(mydata, caption = paste('COVID-19 Statistics', myfips$state, myfips$date),
  #           options = list(paging = FALSE, searching = FALSE))
  # datatable(mydata, options = list(paging = FALSE, searching = FALSE))
}

rank_airyards <- function(df1, df2, start_week, end_week) {
  rank_airyards <- df1 %>%
  filter(position == df2$position[1]) %>%
  group_by(player_id) %>%
  add_count(name = "count1") %>%
  filter(week >= start_week, week <= end_week) %>%
  add_count(name = "count2") %>%
  mutate(tot_fpts = sum(fpts_hppr),
         tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
         tot_recept = sum(receptions),
         tot_targ = sum(targets),
         tot_carries = sum(carries),
         tot_touch = tot_recept+ tot_carries,
         fpts_pg = tot_fpts/count2,
         targ_pg = tot_targ/count2,
         recept_pg = tot_recept/count2,
         carries_pg = tot_carries/count2,
         airyards_pg = sum(receiving_air_yards)/count2,
         fpts_pt = fpts_pg/(recept_pg + carries_pg),
         yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/count2,
         passyards_pg = sum(passing_yards)/count2,
         td_int_ratio = sum(passing_tds)/sum(interceptions),
         tot_airyards = sum(receiving_air_yards)) %>%
  slice(n = 1) %>%
  # filter(tot_touch >= 100 | tot_recept >=60) %>%
  arrange(-tot_airyards) %>%
    filter(count2 >= 3) %>%
  ungroup() %>%
  slice(n = 1:24)
rank_airyards <- rank_airyards %>%
  mutate(across(where(is.numeric), round, 2))
}


# highchart() %>%
#   hc_add_theme(hc_theme_smpl()) %>%
#   hc_yAxis_multiples(list(title = list(text = "Total Air yards"),
#                           min=0,
#                           max = max(ay$tot_airyards),
#                           showFirstLabel = TRUE,
#                           showLastLabel = TRUE,
#                           opposite = FALSE),
#                      list(title = list(text ='Air yards per game'),
#                           min=0,
#                           max = max(ay$airyards_pg),
#                           # max = max(wr1$target_share),
#                           showLastLabel=FALSE,
#                           opposite = TRUE)) %>%
#   hc_add_series(ay, name = "Total air yards",  type = "column", hcaes(x = full_name, y = tot_airyards), yAxis = 0) %>%
#   hc_add_series(ay, name = "air yard/game", type = "column",
#                 hcaes(x = full_name, y = airyards_pg), yAxis = 1 ) %>%
#   hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL) %>%
#   hc_colors(c("darkcyan", "darkred")) %>%
#   hc_xAxis(categories = ay$player_name)

rank_targets <- function(df1, df2, start_week, end_week) {
  rank_targ <- df1 %>%
    add_count(name = "count1") %>%
    filter(week >= start_week, week <= end_week) %>%
    group_by(recent_team) %>%
    mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
    ungroup() %>%
    filter(position == df2$position[1]) %>%
    group_by(player_id) %>%
    add_count(name = "count2") %>%
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
  rank_targ <- rank_targ %>%
    mutate(across(where(is.numeric), round, 2))

}

rank_ypt <- function(df1, df2, start_week, end_week) {
  rank_ypc <- df1 %>%
    filter(position == df2$position[1]) %>%
    filter(week >= start_week, week <= end_week) %>%
    # filter(week >= 1, week <= 16) %>%
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
           fpts_per_touch = tot_fpts/(tot_recept + tot_carries),
           touches_pg = carries_pg + recept_pg) %>%
    ungroup() %>%
    filter(carries_pg >= 6.25 | recept_pg >= 4 | touches_pg >= 6, n >= 3) %>%
    group_by(player_id) %>%
    slice(n = 1) %>%
    ungroup() %>%
    mutate(avg_ypc = mean(ypc), avg_ypt = mean(yards_per_touch)) %>%
    arrange(-yards_per_touch) %>%
    slice(n = 1:36)
  rank_ypc <- rank_ypc %>%
    mutate(across(where(is.numeric), round, 2))

}

rank_touches <- function(df1, df2, start_week, end_week) {
  rank_touches <- df1 %>%
    filter(position == df2$position[1]) %>%
    filter(week >= start_week, week <= end_week) %>%
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
           touches_pg = tot_touch/n) %>%
    ungroup() %>%
    # filter(touches_pg >= 4) %>%
    group_by(player_id) %>%
    slice(n = 1) %>%
    ungroup() %>%
    arrange(-touches_pg) %>%
    head(75)

  rank_touches <- rank_touches %>%
    mutate(across(where(is.numeric), round, 2))
}

rank_yac <- function(df1, df2, start_week, end_week) {
  rank_after_catch <- df1 %>%
    add_count(name = "count1") %>%
    filter(week >= start_week, week <= end_week) %>%
    group_by(recent_team) %>%
    mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
    ungroup() %>%
    filter(position == df2$position[1]) %>%
    group_by(player_id) %>%
    add_count(name = "count2") %>%
    mutate(tot_fpts = sum(fpts_hppr),
           tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
           tot_recept = sum(receptions),
           tot_targ = sum(targets),
           fpts_pg = tot_fpts/count2,
           targ_pg = tot_targ/count2,
           recept_pg = tot_recept/count2,
           avg_targ_share = tot_targ/team_targets,
           tot_yards_after_catch = sum(receiving_yards_after_catch),
           yards_after_catch_pg = tot_yards_after_catch/count2) %>%
    slice(n = 1) %>%
    arrange(-tot_yards_after_catch) %>%
    ungroup() %>%
    slice(n = 1:36)

  rank_after_catch$yards_after_catch_pg <- round(rank_after_catch$yards_after_catch_pg, 2)
  rank_after_catch
}

rank_passing <- function(df1, df2, start_week, end_week) {
  rank_pass <- df1 %>%
    add_count(name = "count1") %>%
    filter(week >= start_week, week <= end_week) %>%
    group_by(recent_team) %>%
    mutate(team_targets = sum(targets), target_share = targets/team_targets) %>%
    ungroup() %>%
    filter(position == df2$position[1]) %>%
    group_by(player_id) %>%
    add_count(name = "count2") %>%
    mutate(tot_fpts = sum(fpts_hppr),
           tot_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds) + sum(special_teams_tds),
           tot_recept = sum(receptions),
           tot_targ = sum(targets),
           fpts_pg = tot_fpts/count2,
           targ_pg = tot_targ/count2,
           recept_pg = tot_recept/count2,
           avg_targ_share = tot_targ/team_targets,
           tot_yards_after_catch = sum(receiving_yards_after_catch),
           yards_after_catch_pg = tot_yards_after_catch/count2,
           passyards_pg = sum(passing_yards)/count2,
           passing_air_yards_pg = sum(passing_air_yards)/count2,
           yards_per_attempt = sum(passing_yards)/sum(completions),
           td_int_ratio = sum(passing_tds)/sum(interceptions),
           compl_percent = sum(completions)/sum(attempts),
           compl_percent2 = completions/attempts)%>%
    slice(n = 1) %>%
    arrange(-passyards_pg) %>%
    ungroup() %>%
    head(35)

  rank_pass <- rank_pass %>%
    mutate(across(where(is.numeric), round, 2))

}


# Deals with grouping of NULL data when start has no data
if_is_empty <- function(i){
  is.null(need(i, message = FALSE)
          )}


# myData <- read.csv("my_csv_file_path.csv", row.names=NULL, na.strings="", stringsAsFactors=FALSE)
#
# myList <- as.list(unique(myData[[1]])) ## The number should be the column/field in myData that has the desired list.
#
# lapply(1:num, function(i) {
#   selectInput(paste0("n_input_", i), label = paste0("n_input", i),
#               choices = myList)
# })









