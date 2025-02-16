library(ggplot2)
library(MASS)
library(dplyr)
library(cowplot)

# function to create heatmaps given the selected player data, pitch_type, and optional flag for PDF rendering

get_batter_heatmaps <- function(data, id, throws, date_low, date_high, heat_pitch_type, for_pdf = FALSE) {
  
  # function that builds the actual heatmaps themselves
  
  build_heatmap <- function(filtered_df) {
    
    # if there is no data passed in, return a blank strike zone
    
    if(nrow(filtered_df) == 0) {
      
      heatmap_viz <- ggplot() +
        coord_equal(xlim = c((-16/12), (16/12)), ylim = c(1, 4)) +
        theme_minimal() +
        geom_rect(aes(xmin = -(10/12), xmax = (10/12), ymin = 1.5, ymax = 3.5), 
                  fill = NA, color = "black", linetype = "solid") +  # strike zone outline
        theme(legend.position = "none",
              legend.title = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              plot.margin = unit(c(0, 0, 0, 0), "cm"),
              plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
      
      return(heatmap_viz)
      
    }
    
    # this code generates the plot if there is enough data to build a heatmap
    
    # define a grid over the entire plot range
    
    x_grid <- seq(-1.5, 1.5, length.out = 100)
    y_grid <- seq(0, 4, length.out = 100)
    
    # create a 2D kernel density estimate (essentially frequency of points by location so we can adjust color of heat)
    
    kde <- kde2d(filtered_df$PlateLocSide, filtered_df$PlateLocHeight, h = 1, n = 100, lims = c(range(x_grid), range(y_grid)))
    
    # expand these density estimates into a dataframe and filter our densities that are very sparse
    
    df <- expand.grid(x = kde$x, y = kde$y) %>%
      mutate(z = as.vector(kde$z)) %>%
      filter(z > 0.001)
    
    # build the heatmap with gradient colors based on how large the density is by location
    
    heatmap_viz <- ggplot(df, aes(x = x, y = y, fill = z)) +
      geom_tile(color = "white") +
      geom_raster(aes(alpha = ifelse(z > 0.001, 1, 0)), interpolate = TRUE) +  
      scale_fill_gradientn(colors = c("white", "white", "blue", "green", "yellow", "red"), 
                           values = scales::rescale(c(0, 0.25, 0.5, 1)), 
                           guide = "colorbar") +
      scale_alpha_identity() +
      coord_equal(xlim = c((-16/12), (16/12)), ylim = c(1, 4)) +
      theme_minimal() +
      geom_rect(aes(xmin = -(10/12), xmax = (10/12), ymin = 1.5, ymax = 3.5), 
                fill = NA, color = "black", linetype = "solid") +  # strike zone outline
      theme(legend.position = "none",
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
    
    
    return(heatmap_viz)
    
  }
  
  # Given the input form of the pitch type (i.e. "Four-Seam" vs. "FB") adjust the form for our given needs
  
  if (heat_pitch_type %in% c("Four-Seam", "Sinker", "Cutter", "Slider", "Curveball", "Changeup", "Splitter")) {
    selected_pitch <- heat_pitch_type
  } else {
    pitch_type_map <- c("FB" = "Four-Seam", "2FB" = "Sinker", "CT" = "Cutter", "SL" = "Slider", "CB" = "Curveball", "CH" = "Changeup", "FS" = "Splitter")
    selected_pitch <- pitch_type_map[heat_pitch_type]
  }
  
  final_heatmaps <- c()
  
  # DO WE WANT A THRESHOLD ON HOW MANY ROWS OF DATA WE HAVE BEFORE DISPLAYING?
  
  # filter the incoming data to be for the proper user selected filters (id, pitcher_throws, date range)
  
  user_filtered_data <- data %>%
    filter(BatterId == id) %>%
    filter(PitcherThrows == throws) %>%
    filter(Date >= date_low & Date <= date_high)
  
  # filter the data for hard hit balls for the selected pitch type
  
  hard_hit_balls <- user_filtered_data %>%
    dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide, ExitSpeed) %>%
    filter(!is.na(ExitSpeed) & !is.na(PlateLocHeight) & (PitchCall == "InPlay")) %>%
    filter(ExitSpeed > 95) %>%
    filter(AutoPitchType == selected_pitch)
  
  # if using this function in a PDF, make some adjustments so that the axis labels and titles are correct
  # since we show a grid of every pitch in the PDF but only one pitch at a time in the Shiny app
  
  if (for_pdf) {
    label_type_map <- c("Four-Seam" = "FB", "Sinker" = "2FB", "Cutter" = "CT", "Slider" = "SL", "Curveball" = "CB", "Changeup" = "CH", "Splitter" = "FS")
    pitch_label <- label_type_map[selected_pitch]
    
    final_heatmaps[["EV > 95"]] <- build_heatmap(hard_hit_balls) + ylab(pitch_label) + theme(axis.title.y = element_text(size=15, face="bold", angle = 0, vjust=0.5, hjust=-0.5))
    
  } else {
    final_heatmaps[["EV > 95"]] <- build_heatmap(hard_hit_balls) + ggtitle("EV > 95") + theme(plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
  }
  
  # apply same logic that you did for the hard hit balls, but now for whiffs (swing and misses)
  
  whiff_locs <- user_filtered_data %>%
    dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide) %>%
    filter(!is.na(PlateLocHeight) & (PitchCall == "StrikeSwinging")) %>%
    filter(AutoPitchType == selected_pitch)
  
  if (for_pdf) {
    label_type_map <- c("Four-Seam" = "FB", "Sinker" = "2FB", "Cutter" = "CT", "Slider" = "SL", "Curveball" = "CB", "Changeup" = "CH", "Splitter" = "FS")
    pitch_label <- label_type_map[selected_pitch]
    
    final_heatmaps[["Whiff"]] <- build_heatmap(whiff_locs)
    
  } else {
    final_heatmaps[["Whiff"]] <- build_heatmap(whiff_locs) + ggtitle("Whiff") + theme(plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
  }
  
  # apply the same logic you just did, but now for chase (swings at pitches out of the zone)
  chase_locs <- user_filtered_data %>%
    dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide) %>%
    filter(!is.na(PlateLocHeight) & (PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBallNotFieldable", "FoulBallFieldable"))) %>%
    filter(!((PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) & (PlateLocSide >= -(10/12)) & (PlateLocSide <= (10/12)))) %>%
    filter(AutoPitchType == selected_pitch)
  
  if (for_pdf) {
    label_type_map <- c("Four-Seam" = "FB", "Sinker" = "2FB", "Cutter" = "CT", "Slider" = "SL", "Curveball" = "CB", "Changeup" = "CH", "Splitter" = "FS")
    pitch_label <- label_type_map[selected_pitch]
    
    final_heatmaps[["Chase"]] <- build_heatmap(chase_locs)
    
  } else {
    final_heatmaps[["Chase"]] <- build_heatmap(chase_locs) + ggtitle("Chase") + theme(plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0, size = 22))
  }
  
  # combine the 3 heatmap types together for the given pitch type using cowplot
  
  new_heatmap_list <- plot_grid(final_heatmaps[["EV > 95"]], final_heatmaps[["Whiff"]], final_heatmaps[["Chase"]], ncol = 3)
  
  # hide the page spinner in the Shiny app once this is done loading
  
  hidePageSpinner()

  return(new_heatmap_list)
  
}

############### EXTRA CODE FOR TESTING ################

# pitch_types <- c("Four-Seam", "Slider", "Curveball", "Changeup") # WHICH PITCH TYPE COLUMN DO WE WANT TO USE? DO WE WANT TO SHOW ALL PITCHES THROWN OR ONLY ONES OVER A THRESHOLD??
# pitch_types <- pitch_types[!is.na(pitch_types) & pitch_types != ""]

# for (pitch in pitch_types) {
#   hard_hit_balls <- data %>%
#     dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide, ExitSpeed) %>%
#     filter(!is.na(ExitSpeed) & !is.na(PlateLocHeight) & (PitchCall == "InPlay")) %>%
#     filter(ExitSpeed > 95) %>%
#     filter(AutoPitchType == pitch)
#   final_heatmaps[[paste0(pitch, " EV > 95")]] <- build_heatmap(hard_hit_balls)
# 
#   whiff_locs <- data %>%
#     dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide) %>%
#     filter(!is.na(PlateLocHeight) & (PitchCall == "StrikeSwinging")) %>%
#     filter(AutoPitchType == pitch)
#   final_heatmaps[[paste0(pitch, " Whiff")]] <- build_heatmap(whiff_locs)
# 
#   chase_locs <- data %>%
#     dplyr::select(BatterId, BatterSide, PitchCall, AutoPitchType, PlayResult, TaggedHitType, PlateLocHeight, PlateLocSide) %>%
#     filter(!is.na(PlateLocHeight) & (PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBallNotFieldable", "FoulBallFieldable"))) %>%
#     filter(!((PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) & (PlateLocSide >= -(10/12)) & (PlateLocSide <= (10/12)))) %>%
#     filter(AutoPitchType == pitch)
#   final_heatmaps[[paste0(pitch, " Chase")]] <- build_heatmap(chase_locs)
# }

## COWPLOT ALL OF THE HEATMAPS TOGETHER AND THEN RETURN THE COWPLOT

# ev_95_header <- ggdraw() + draw_label("EV > 95", fontface = 'bold')
# whiff_header <- ggdraw() + draw_label("Whiff", fontface = 'bold')
# chase_header <- ggdraw() + draw_label("Chase", fontface = 'bold')
# fb_header <- ggdraw() + draw_label("FB", fontface = 'bold', angle = 90)
# sl_header <- ggdraw() + draw_label("SL", fontface = 'bold', angle = 90)
# cb_header <- ggdraw() + draw_label("CB", fontface = 'bold', angle = 90)
# ch_header <- ggdraw() + draw_label("CH", fontface = 'bold', angle = 90)

# heatmap_list <- plot_grid(NULL, ev_95_header, whiff_header, chase_header,
#                       NULL, NULL, NULL, NULL,
#                       fb_header, final_heatmaps[["Four-Seam EV > 95"]], final_heatmaps[["Four-Seam Whiff"]], final_heatmaps[["Four-Seam Chase"]],
#                       NULL, NULL, NULL, NULL,
#                       sl_header, final_heatmaps[["Slider EV > 95"]], final_heatmaps[["Slider Whiff"]], final_heatmaps[["Slider Chase"]],
#                       NULL, NULL, NULL, NULL,
#                       cb_header, final_heatmaps[["Curveball EV > 95"]], final_heatmaps[["Curveball Whiff"]], final_heatmaps[["Curveball Chase"]],
#                       NULL, NULL, NULL, NULL,
#                       ch_header, final_heatmaps[["Changeup EV > 95"]], final_heatmaps[["Changeup Whiff"]], final_heatmaps[["Changeup Chase"]],
#                       ncol = 4, nrow = 9, rel_widths = c(0.1, 1, 1, 1), rel_heights = c(0.1, 0.001, 1, 0.001, 1, 0.001, 1, 0.001, 1), align = 'hv', axis = 'tblr')

# test_heatmap_list <- (plot_spacer() + plot_spacer() + plot_spacer() + ev_95_header + plot_spacer() + whiff_header + plot_spacer() + chase_header + plot_layout(widths = c(0.1, 0.5,-1.1,5, -2.1, 5, -2.1, 5), ncol = 8)) /
#   (plot_spacer() + fb_header + plot_spacer() + final_heatmaps[["Four-Seam EV > 95"]] + plot_spacer() + final_heatmaps[["Four-Seam Whiff"]] + plot_spacer() + final_heatmaps[["Four-Seam Chase"]] + plot_layout(widths = c(0.1, 0.5,-1.1,5, -2.1, 5, -2.1, 5), ncol = 8)) /
#   (plot_spacer() + sl_header + plot_spacer() + final_heatmaps[["Slider EV > 95"]] + plot_spacer() + final_heatmaps[["Slider Whiff"]] + plot_spacer() + final_heatmaps[["Slider Chase"]] + plot_layout(widths = c(0.1, 0.5,-1.1,5, -2.1, 5, -2.1, 5), ncol = 8)) /
#   (plot_spacer() + cb_header + plot_spacer() + final_heatmaps[["Curveball EV > 95"]] + plot_spacer() + final_heatmaps[["Curveball Whiff"]] + plot_spacer() + final_heatmaps[["Curveball Whiff"]] + plot_layout(widths = c(0.1, 0.5,-1.1,5, -2.1, 5, -2.1, 5), ncol = 8)) /
#   (plot_spacer() + ch_header + plot_spacer() + final_heatmaps[["Changeup EV > 95"]] + plot_spacer() + final_heatmaps[["Changeup Whiff"]] + plot_spacer() + final_heatmaps[["Changeup Chase"]] + plot_layout(widths = c(0.1, 0.5,-1.1,5, -2.1, 5, -2.1, 5), ncol = 8))
# 
#new_heatmap_list <- final_heatmaps[["EV > 95"]] + plot_spacer() + final_heatmaps[["Whiff"]] + plot_spacer() + final_heatmaps[["Chase"]] + plot_layout(widths = c(5, -2.1, 5, -2.1, 5), ncol = 5)
