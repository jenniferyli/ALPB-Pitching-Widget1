library(dplyr)
library(cowplot)
library(gridtext)
library(gt)
library(rmarkdown)
library(knitr)

# Function takes in Trackman data, a player name, and a date range and generates a PDF

get_report_pdf <- function(data, basic_percentile_data, adv_percentile_data, player_id, date_range_1, date_range_2) {

  # Filter the data for the corresponding player_name and date_range
  
  data_for_pdf <- data %>%
    filter(BatterId == player_id) %>%
    filter(Date >= date_range_1 & Date <= date_range_2)
  
  # Component 0 - Store the info for building the Page Header
  
  player_name <- data_for_pdf %>% pull(Batter) %>% unique()
  
  page_header <- list(player_name, date_range_1, date_range_2)
  
  # Create 2 pages, 1 for VS RHP and 1 for VS LHP

  for (page in 1:2) {

    # build the page for the batter against RHPs

    if (page == 1) {

      data_vs_rhp <- data_for_pdf %>%
        filter(PitcherThrows == "Right")

      # Component 1 - Build the Batter Bio Card
      
      batter_info_rhp <- get_batter_info_card(data_vs_rhp, player_id) %>%
        gt() %>%
        tab_header(
          title = md("**Batter Info**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)

      # Component 2 - Build the Basic Stats Table
      
      basic_stats_rhp <- get_basic_stats(data_vs_rhp, basic_percentile_data, player_id, "Right", date_range_1, date_range_2, include_percentiles = FALSE) %>%
        gt() %>%
        tab_header(
          title = md("**Counting Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      # Combine the Batter Info and Basic Stats together using Cowplot
      
      info_and_stats_row_rhp <- cowplot::plot_grid(batter_info_rhp, basic_stats_rhp, nrow = 1)

      # Component 3 - Build the Spray Chart visual
      
      spray_chart_rhp <- get_spray_chart(data_vs_rhp, player_id, "Right", date_range_1, date_range_2)
      
      # Component 4 - Build the Advanced Stats Tables for Swing and Launch Stats
      
      advanced_swing_stats_rhp <- get_advanced_stats(data_vs_rhp, adv_percentile_data, player_id, "Right", date_range_1, date_range_2, include_percentiles = FALSE)$swing_stats %>%
        gt() %>%
        tab_header(
          title = md("**Swing Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      advanced_launch_stats_rhp <- get_advanced_stats(data_vs_rhp, adv_percentile_data, player_id, "Right", date_range_1, date_range_2, include_percentiles = FALSE)$launch_stats %>%
        gt() %>%
        tab_header(
          title = md("**Launch Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      # Combine the spray chart and advanced stats together using cowplot (tables vertically stacked and then the rest horizontally)
      
      advanced_stats_combined_rhp <- cowplot::plot_grid(advanced_swing_stats_rhp, advanced_launch_stats_rhp, ncol = 1)
      spray_and_advanced_row_rhp <- cowplot::plot_grid(spray_chart_rhp, advanced_stats_combined_rhp)

      # Component 5 - Build the Heatmap visuals
      
      heatmaps_list_rhp <- list()
      
      # Loop through each pitch type that the batter has seen and call the heatmap building function. Store results in a list
      
      for (type in unique(data_vs_rhp$AutoPitchType)) {
        if (type %in% c("Four-Seam", "Sinker", "Cutter", "Slider", "Curveball", "Changeup", "Splitter")) {
          if (table(data_vs_rhp$AutoPitchType)[[type]][1] >= 15) {
            heatmaps_list_rhp[[type]] <- get_batter_heatmaps(data_vs_rhp, player_id, "Right", date_range_1, date_range_2, type, for_pdf = TRUE)
          }
        }
      }
      
      # combine all of the heatmaps together using cowplot to arrange them in a grid
      
      heatmaps_row_rhp <- cowplot::plot_grid(plotlist = heatmaps_list_rhp, ncol = 1)

    } else {
      
      # do the exact same process as before but this time for VS LHP

      data_vs_lhp <- data_for_pdf %>%
        filter(PitcherThrows == "Left")

      # Component 1 - Batter Bio Card

      batter_info_lhp <- get_batter_info_card(data_vs_lhp, player_id) %>%
        gt() %>%
        tab_header(
          title = md("**Batter Info**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)

      # Component 2 - Basic Stats

      basic_stats_lhp <- get_basic_stats(data_vs_lhp, basic_percentile_data, player_id, "Left", date_range_1, date_range_2, include_percentiles = FALSE) %>%
        gt() %>%
        tab_header(
          title = md("**Counting Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      # Cowplot Batter Info and Basic Stats together
      
      info_and_stats_row_lhp <- cowplot::plot_grid(batter_info_lhp, basic_stats_lhp, nrow = 1)

      # Component 3 - Spray Chart

      spray_chart_lhp <- get_spray_chart(data_vs_lhp, player_id, "Left", date_range_1, date_range_2)

      # Component 4 - Advanced Stats

      advanced_swing_stats_lhp <- get_advanced_stats(data_vs_lhp, adv_percentile_data, player_id, "Left", date_range_1, date_range_2, include_percentiles = FALSE)$swing_stats %>%
        gt() %>%
        tab_header(
          title = md("**Swing Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      advanced_launch_stats_lhp <- get_advanced_stats(data_vs_lhp, adv_percentile_data, player_id, "Left", date_range_1, date_range_2, include_percentiles = FALSE)$launch_stats %>%
        gt() %>%
        tab_header(
          title = md("**Launch Stats**")
        ) %>%
        cols_align(align = "center") %>%
        as_gtable(text_grob = gridtext::richtext_grob)
      
      # Cowplot spray chart and advanced stats together
      
      advanced_stats_combined_lhp <- cowplot::plot_grid(advanced_swing_stats_lhp, advanced_launch_stats_lhp, ncol = 1)
      spray_and_advanced_row_lhp <- cowplot::plot_grid(spray_chart_lhp, advanced_stats_combined_lhp)

      # Component 5 - Heatmaps

      heatmaps_list_lhp <- list()
      
      for (type in unique(data_vs_lhp$AutoPitchType)) {
        if (type %in% c("Four-Seam", "Sinker", "Cutter", "Slider", "Curveball", "Changeup", "Splitter")) {
          if (table(data_vs_lhp$AutoPitchType)[[type]][1] >= 15) {
            heatmaps_list_lhp[[type]] <- get_batter_heatmaps(data_vs_lhp, player_id, "Left", date_range_1, date_range_2, type, for_pdf = TRUE)
          }
        }
      }
      
      heatmaps_row_lhp <- cowplot::plot_grid(plotlist = heatmaps_list_lhp, ncol = 1)

    }

  }

  # combine both the RHP and LHP tables and visuals into a list to pass into the PDF rendering function
  
  info_and_stats_list <- list(info_and_stats_row_rhp, info_and_stats_row_lhp)
  spray_and_advanced_list <- list(spray_and_advanced_row_rhp, spray_and_advanced_row_lhp)
  batter_heatmaps_list <- list(heatmaps_row_rhp, heatmaps_row_lhp)

  # render the PDF of all the tables and visuals and save it to the user's current directory
  
  rmarkdown::render('renderHitterPDFReport.Rmd', output_format = "pdf_document", encoding = "UTF-8", output_file = paste0(getwd(), "/", player_name, " Hitter Report.pdf"),
                    params = list(report_header = page_header,
                                  info_and_stats_viz = info_and_stats_list,
                                  spray_and_advanced_viz = spray_and_advanced_list,
                                  batter_heatmaps_viz = batter_heatmaps_list
  ))

}


########### Extra Code for Quick Testing ###################

#get_report_pdf(trackman_data, "Encarnacion, JC", "2024-06-30", "2024-07-22")


# trackman_data <- get_trackman_data() %>%
#   select(Batter, BatterTeam, BatterId, BatterSide, PitcherThrows, Date, Balls, Strikes, Outs, PitchCall, KorBB, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide, ExitSpeed, Angle, Direction, PlayID, Distance, Bearing, AutoPitchType)
# 
# player_name <- "Encarnacion, JC"
# 
# data_for_pdf <- trackman_data %>%
#   filter(Batter == player_name) %>%
#   filter(Date >= "2024-06-30" & Date <= "2024-07-22")
# 
# data_vs_rhp <- data_for_pdf %>%
#   filter(PitcherThrows == "Right")
# 
# # Component 1 - Batter Bio Card
# 
# batter_info_rhp <- get_batter_info_card(data_vs_rhp) %>%
#   gt() %>%
#   tab_header(
#     title = md("**Batter Info**")
#   ) %>%
#   cols_align(align = "center") %>%
#   as_gtable(text_grob = gridtext::richtext_grob)
# 
# # Component 2 - Basic Stats
# 
# basic_stats_rhp <- get_basic_stats(data_vs_rhp) %>%
#   gt() %>%
#   tab_header(
#     title = md("**Counting Stats**")
#   ) %>%
#   cols_align(align = "center") %>%
#   as_gtable(text_grob = gridtext::richtext_grob)
# 
# row_1 <- cowplot::plot_grid(batter_info_rhp, NULL, basic_stats_rhp, nrow = 1, rel_widths = c(1, -0.75, 1))
# 
# # Component 3 - Spray Chart
# 
# spray_chart_rhp <- get_spray_chart(data_vs_rhp)
# 
# # Component 4 - Advanced Stats
# 
# advanced_swing_stats_rhp <- get_advanced_stats(data_vs_rhp)$swing_stats %>%
#   gt() %>%
#   tab_header(
#     title = md("**Swing Stats**")
#   ) %>%
#   cols_align(align = "center") %>%
#   as_gtable(text_grob = gridtext::richtext_grob)
# 
# advanced_launch_stats_rhp <- get_advanced_stats(data_vs_rhp)$launch_stats %>%
#   gt() %>%
#   tab_header(
#     title = md("**Launch Stats**")
#   ) %>%
#   cols_align(align = "center") %>%
#   as_gtable(text_grob = gridtext::richtext_grob)
# 
# ad_stats <- cowplot::plot_grid(advanced_swing_stats_rhp, NULL, advanced_launch_stats_rhp, ncol = 1, rel_heights = c(1, -0.75, 1))
# row_2 <- cowplot::plot_grid(spray_chart_rhp, ad_stats)
# 
# # Component 5 - Heatmaps
# 
# rhp_heatmaps_list <- list()
# 
# for (type in unique(data_vs_rhp$AutoPitchType)) {
#   if (type %in% c("Four-Seam", "Sinker", "Cutter", "Slider", "Curveball", "Changeup", "Splitter")) {
#     if (table(data_vs_rhp$AutoPitchType)[[type]][1] >= 15) {
#       rhp_heatmaps_list[[type]] <- get_batter_heatmaps(data_vs_rhp, type)
#     }
#   }
# }
# 
# row_3 <- cowplot::plot_grid(plotlist = rhp_heatmaps_list, ncol = 1)
# 
# page_header <- ggplot2::ggplot(data = NULL, aes(x = 1, y = 1)) +
#   ggimage::geom_image(aes(image = "ALPB_Logo.png"),
#                       size = 1
#   ) +
#   annotate(geom="text", x = 4, y = 1.25, label = paste0(player_name, " Hitter Report"),
#            color="blue") + 
#   annotate(geom="text", x = 4, y = 0.75, label = "From Date X to Date Y",
#            color="blue") +
#   annotate(geom="text", x = 7, y = 1, label = "Versus RHP",
#            color="blue") +
#   theme_void() +
#   coord_equal(x = c(0,8.5), y = c(0,2))
# 
# source('renderHitterPDFReport.Rmd')
# 
# rmarkdown::render('renderHitterPDFReport.Rmd', output_file = paste0(player_name, " Hitter Report.pdf"), params = list(
#   report_header = page_header,
#   batter_info_viz = row_1,
#   basic_stats_viz = row_2,
#   spray_chart_viz = row_3
# ))