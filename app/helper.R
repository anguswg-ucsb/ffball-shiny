
# Loads play-by-play data and calculates_player_stats for year input
load_data <- function(year) {
    pbp <- load_pbp(year)

    stats <- calculate_player_stats(pbp,weekly = T)

    player <- stats %>%
      filter(week <= 16) %>%
      mutate(fpts_hppr = fantasy_points + (receptions*0.5),
             ffpt_per_game = sum(fpts_hppr)/max(week),
             rush_yards_pg = sum(rushing_yards)/max(week))

    rosters <- fast_scraper_roster(year) %>%
             filter(position %in% c("QB", "RB", "TE", "WR"))

    player <- left_join(player, select(rosters, position, full_name, gsis_id), by = c("player_id" = "gsis_id"))
}


make_dt <- function(season_df) {
    player_dt <- season_df %>%
      group_by(full_name) %>%
      summarise(across(6:39, sum))
    datatable(player_dt)

}

# myData <- read.csv("my_csv_file_path.csv", row.names=NULL, na.strings="", stringsAsFactors=FALSE)
#
# myList <- as.list(unique(myData[[1]])) ## The number should be the column/field in myData that has the desired list.
#
# lapply(1:num, function(i) {
#   selectInput(paste0("n_input_", i), label = paste0("n_input", i),
#               choices = myList)
# })









