# ### Player Profile
# ```{r}
# # htmlOutput("profilePic")
# ```

####################  MAKE PLOTLY GAME LOG GRAPH ####################
# plot_ly(df, x = ~week, y = ~rushing_yards,
#         type = 'bar',
#         textposition = 'auto',
#         name = "RUSHING",
#         marker = list(color = 'rgba(219, 64, 82, 0.7)',
#                       line = list(color = 'rgba(219, 64, 82, 1.0)',
#                                   width = 2))) %>%
#   add_trace(df, x = ~week, y = ~receiving_yards, type = 'bar', name = "RECIEVING", marker = list(color = 'rgba(10, 150, 24, 0.7)',
#                           line = list(color = 'rgba(55, 128, 191, 0.7)',
#                                       width = 2)))%>%
#   add_trace(df, x = ~week, y = ~passing_yards,
#             type = 'bar',
#             name = "PASSING",
#             marker = list(color = 'rgba(55, 128, 191, 0.7)',
#                           line = list(color = 'rgba(55, 128, 191, 0.7)',
#                                       width = 2))) %>%
#   add_trace(df, x = ~week, y = ~fpts_hppr,
#             type = "scatter",
#             mode = "lines",
#             name = "FPTS",
#             line = list(color = 'rgba(0, 0, 0, 1)',
#                         width = 4)) %>%
#   # add_segments(x = min(~week), xend = max(~week), y = ~rush_per_game, yend = ~rush_per_game) %>%
#   layout(barmode = "stack")


# ```{r context = "server"}
# # headshotPic <- eventReactive(input$submitButton, {
# #   if(is.null(input$playerSearch)) {
# #     NULL
# #   } else {
# #     df <- season()
# #     df <- player_data(df, input$playerSearch)
# #     output$profilePic <- renderImage({
# #         list(src = df$headshot_url,
# #           contentType = 'image/png',
# #           width = 224,
# #           height = 136,
# #           alt = "This is image alternate text")
# #       })
# #     }
# # })
# # output$profilePic <- renderImage({
# #     list(src = ,
# #      contentType = 'image/png',
# #      width = 224,
# #      height = 136,
# #      alt = "This is image alternate text")
# ```
#
#
#
#
#
#
#
#
#
# ```{r context = "server"}
# # seasonData <- eventReactive(input$submitButton, {
# #   make_dt(load_data(as.numeric(input$seasonDropdown)))
# #   })
# #
# # output$seasonDT <- renderDT({
# #    seasonData()
# #   })
# # observeEvent(input$submitButton, {
# #   season$df_data <<- load_data(as.numeric(input$seasonDropdown))
# # })
#
# # season & position inputs output data table
# # seasonData <- eventReactive(input$submitButton, {
# #   df <- season()
# #     season$df_data <<- load_data(as.numeric(input$seasonDropdown))
# #
# #       if (input$positionDropdown != "All") {
# #           season <- season[season$position == input$positionDropdown,]
# #       }
# #
# #     #select position, team, full_name to join with summarized season
# #     season2 <- season %>%
# #       select(3, 4, 42, 43)
# #
# #     season2 <- unique(season2)
# #
# #     season <- season %>%
# #         group_by(full_name) %>%
# #         summarise(across(6:39, sum))
# #     season <- left_join(season2, season, by = "full_name")
# #
# #     season <- season %>%
# #       select(4, 1:3, 38, 37, 36, 5:35) %>%
# #       arrange(-fpts_hppr)
# #       datatable(season, options = list(scrollX = TRUE, paging = TRUE, searching = TRUE))
# #   })
# #
# # output$seasonDT <- renderDT({
# #    seasonData()
# #   })
#
# ```
#
#
#
#
#
# ```{r}
# # observe(
# #     if(input$auto == ""){
# #       NULL
# #     } else {
# #       FIP <<- input$auto
# #       leafletProxy("covidMap") %>%
# #         zoom_to_county(counties, FIP)
# #       output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
# #       output$covidTable <- renderDT({ make_table2(today, FIP) })
# #       output$covidNewCases = renderPlotly({ daily_cases_graph(covid19, FIP) })
# #       output$covidNewDeaths <- renderPlotly({ daily_deaths_graph(covid19, FIP) })
# #       leafletProxy("covidMap2") %>%
# #           zoom_to_county(counties, FIP)
# #     })
#
# ```
#
#```{r context = "server"}
# FPTS/Game positional rank during period
# fppgRank <- eventReactive(input$submitButton, {
#   if(is.null(input$playerSearch)) {
#       NULL
#   } else {
#       # df <- season()
#       # df <- get_player_data(df, input$playerSearch, input$weekRange[1], input$weekRange[2])
#     df1 <- season()
#     df2 <- player1()
#   # find position rank --- total fpts during period
#     ranks <- df1 %>%
#       filter(position == df2$position[1]) %>%
#       filter(week >= input$weekRange[1], week <= input$weekRange[2]) %>%
#       group_by(player_id) %>%
#       add_count() %>%
#       mutate(total_fpts = sum(fpts_hppr),
#              total_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds),
#              fpts_pg = total_fpts/n) %>%
#       slice(n = 1) %>%
#       filter(n >= 5) %>%
#       arrange(-fpts_pg)
#       ungroup() %>%
#       mutate(rank = 1:n()) %>%
#       filter(player_id == df2$player_id[1]) %>%
#       select(rank)
#
#   }
# })
#
# # Output positional Fantasy points per game ranking value box to UI
# output$fppgRankBox <- renderValueBox({
#   valueBox(fppgRank(),
#            caption = "FPTS/Game rank")
# })
```
#
# ### FPTS/game rank
# ```{r}
# # valueBoxOutput("fppgRankBox")
# ```
# #

### FPTS/game value
# ```{r}
# valueBoxOutput("fppgBox")
# ```
# ```{r context="server"}
# # FPTS/Game number during period
# fppgValue <- eventReactive(input$submitButton, {
#   if(is.null(input$playerSearch)) {
#     NULL
#   } else {
#     df1 <- season()
#     df2 <- player1()
#     # find position rank --- total fpts during period
#     ranks <- df1 %>%
#       filter(position == df2$position[1]) %>%
#       filter(week >= input$weekRange[1], week <= input$weekRange[2]) %>%
#       group_by(player_id) %>%
#       add_count() %>%
#       mutate(total_fpts = sum(fpts_hppr),
#              total_tds = sum(rushing_tds) + sum(passing_tds) + sum(receiving_tds),
#              fpts_pg = total_fpts/n) %>%
#       slice(n = 1) %>%
#       arrange(-fpts_pg) %>%
#       ungroup() %>%
#       mutate(rank = 1:n()) %>%
#       filter(player_id == df2$player_id[1]) %>%
#       select(fpts_pg) %>%
#       round(2)
#   }
# })
#
# # Output position Rank value box to UI
# output$fppgBox <- renderValueBox({
#   valueBox(fppgValue(),
#            caption = "FPTS/game")
# })
# ```
