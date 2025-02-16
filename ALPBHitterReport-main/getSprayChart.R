library(sportyR)

# Function to take in the selected player data and return the spray chart on the baseball field

get_spray_chart <- function(data, id, throws, date_low, date_high) {
  
  # filter the data so that it's only balls in-play
  # perform calculations to turn Bearing angle from degress to radians and then calculate Cartesian distance
  
  hit_locations <- data %>%
    filter(BatterId == id) %>%
    filter(PitcherThrows == throws) %>%
    filter(Date >= date_low & Date <= date_high) %>%
    filter(!is.na(Distance) & PitchCall == "InPlay") %>%
    dplyr::select(PlayID, PitchCall, TaggedHitType, PlayResult, Distance, Bearing) %>%
    mutate(
      bearing_rad = Bearing * pi / 180,
      outcome_type = ifelse(PlayResult %in% c("Out", "Sacrifice", "Error", "FieldersChoice"), "Out", "Hit"),
      #loc_color = ifelse(PlayResult %in% c("Out", "Sacrifice", "Error", "FieldersChoice"), "red", "lightgreen")
    ) %>%
    mutate(
      loc_color = ifelse(outcome_type == "Hit", "lightgreen", "red"),
      ball_x_loc = sin(bearing_rad) * Distance,
      ball_y_loc = cos(bearing_rad) * Distance
    )
  
  # find the max distance to set the correct size of the plot
  # if no balls hit far, default to 350 ft
  
  max_distance <- ifelse(max(hit_locations$Distance) < 350, 350, max(hit_locations$Distance))
  
  # fake DF so that the legend will appear even if a player has no hits or outs
  
  fake_hit_out_df <- data.frame(
    x = c(5,5),
    y = c(10,10),
    fake_color = c("lightgreen", "red")
  )
  
  # build the spray chart and return it
  
  spray_chart_viz <- geom_baseball("mlb", display_range = "full", ylims = c(10, max_distance + 10)) +
    geom_point(data = hit_locations, aes(ball_x_loc, ball_y_loc, color = loc_color), size = 3) +
    geom_point(data = fake_hit_out_df, aes(x, y, color = fake_color), alpha = 0, size = 3) +
    labs(color = "Outcome") +
    scale_color_manual(labels = c("Hit", "Out"), values = c("lightgreen", "red"))
  
  return(spray_chart_viz)
  
}


######### EXTRA CODE FOR TESTING ####################

# ff <- data.frame(
#   location_x = c(0,0),
#   location_y = c(100, 0)
# )
# 
# geom_baseball("mlb", display_range = "full") + geom_point(data = ff,aes(location_x, location_y))
# 
# 
# hit_locations <- test %>%
#   filter(!is.na(Distance) & PitchCall == "InPlay") %>%
#   #filter(PitchUID == "d3d499d0-2c0c-11ef-a6ae-b3e9f68a02eb") %>%
#   select(PlayID, PitchCall, TaggedHitType, PlayResult, Distance, Bearing) #%>%
#   # mutate(
#   #   new_bearing = ifelse(Bearing < 0, abs(Bearing) + 90, Bearing)
#   # ) %>%
#   # mutate(
#   #   ball_x_loc = sin(Bearing) * Distance,
#   #   ball_y_loc = cos(Bearing) * Distance
#   # )
# 
# ggplot(hit_locations, aes(x = Bearing, y = Distance)) + 
#   # drawing an area for the playground
#   annotate(geom = "rect", xmin = 45, xmax = -45, 
#            ymin = 0, ymax = Inf,
#            fill = "grey", alpha = 0.2) + 
#   # add marks for the distances
#   annotate(geom = "text", 
#            x = rep(50, 4), y = c(100, 200, 300, 400), 
#            label = c("100", "200", "300", "400")) +
#   # add lines for the distance 
#   annotate(geom = "segment",
#            x = rep(-45,4), xend = rep(45, 4), 
#            y = c(100, 200, 300, 400), yend = c(100, 200, 300, 400),
#            linetype = "dotted") + 
#   # show the results as points
#   geom_point(colour = "darkgreen", size = 3) + 
#   # convert to polar
#   coord_polar(theta = "x", start = pi, clip = "on") + 
#   theme_void() +  
#   # adjust the axis 
#   scale_x_continuous(limits = c(-180, 180))
# 
# geom_test <- hit_locations %>%
#   mutate(
#     bearing_rad = Bearing * pi / 180
#   ) %>%
#   mutate(
#     ball_x_loc = sin(bearing_rad) * Distance,
#     ball_y_loc = cos(bearing_rad) * Distance
#     )
# 
# geom_baseball("mlb", display_range = "full") + geom_point(data = geom_test,aes(ball_x_loc, ball_y_loc))

# Testing to show A and Y how Git works