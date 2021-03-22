

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
    filter(full_name == as.character(name)) %>%
    select(1, 43, 42, 2:39) %>%
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
           fpts_pt = fpts_pg/(recept_pg + carries_pg),
           yards_pg = (sum(rushing_yards) + sum(receiving_yards) + sum(passing_yards))/n,
           passyards_pg = sum(passing_yards)/n,
           td_int_ratio = sum(passing_tds)/sum(interceptions),
           ypc = rushing_yards/carries)
}

# plot player_data() data
make_player_plot <- function(df) {

  # Fantasy points + RUSH YARDS + PASS + REC  in game in weeks
  plot_ly(df, x = ~week, y = ~rushing_yards,
          type = 'bar',
          textposition = 'auto',
          name = "RUSHING",
          marker = list(color = 'rgba(219, 64, 82, 0.7)',
                        line = list(color = 'rgba(219, 64, 82, 1.0)',
                                    width = 2))) %>%
    add_trace(df, x = ~week, y = ~receiving_yards,
              type = 'bar',
              name = "RECIEVING",
              marker = list(color = 'rgba(10, 150, 24, 0.7)',
                            line = list(color = 'rgba(55, 128, 191, 0.7)',
                                        width = 2)))%>%
    add_trace(df, x = ~week, y = ~passing_yards,
              type = 'bar',
              name = "PASSING",
              marker = list(color = 'rgba(55, 128, 191, 0.7)',
                            line = list(color = 'rgba(55, 128, 191, 0.7)',
                                        width = 2))) %>%
    add_trace(df, x = ~week, y = ~fpts_hppr,
              type = "scatter",
              mode = "lines",
              name = "FPTS",
              line = list(color = 'rgba(0, 0, 0, 1)',
                          width = 4)) %>%
    # add_segments(x = min(~week), xend = max(~week), y = ~rush_per_game, yend = ~rush_per_game) %>%
    layout(barmode = "stack")
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









