

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
           catch_rate = receptions/targets)
}

# plot player_data() data
make_player_plot <- function(df) {
  # Fantasy points + RUSH YARDS + PASS + REC  in game in weeks
  hchart(df, name = "Rushing yards", type = "column", yaxis = 1, hcaes(x = week, y = rushing_yards)) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_add_series(df, name = "Receiving yards", type = "column", yaxis = 1, hcaes(x = week, y = receiving_yards)) %>%
    hc_add_series(df, name = "Passing yards",type = "column",  yaxis = 1, hcaes(x = week, y = passing_yards)) %>%
    hc_add_series(df, name = "Fantasy points",type = "spline",  yaxis = 2, hcaes(x = week, y = fpts_hppr)) %>%
    hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL) %>%
    hc_colors(c("darkcyan", "lightblue", "darkseagreen", "darkred"))

}

# make formattable table Player Profile info
make_profile = function(df){

  formattable(df, align = c("l", rep("r", NCOL(df) - 1)))
  # list(`Name` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold")),
  #      `Team` = color_tile("cornsilk", "darkgoldenrod1"),
  #      `Position` = color_tile("lightpink", "tomato"),
  #      `Birth Date` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold"))))

  # Make an interactive Table! with a caption
  # datatable(mydata, caption = paste('COVID-19 Statistics', myfips$state, myfips$date),
  #           options = list(paging = FALSE, searching = FALSE))
  # datatable(mydata, options = list(paging = FALSE, searching = FALSE))
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









