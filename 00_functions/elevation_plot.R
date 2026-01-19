

elevation_plot <- function(origin = "utrecht, netherlands", 
                           destination = "chäsalp, switzerland",
                           structure = "route",
                           mode = 'bicycling',
                           max_ylim = 1100) {
  
  # generate route dataframe with lon/lat points
  route_df <- ggmap::trek(origin, destination, 
                          mode = mode,
                          structure = structure, output = 'simple')
  
  # Convert route_df to an sf object with lon/lat
  route_sf <- st_as_sf(route_df, coords = c("lon", "lat"), crs = 4326)
  
  # Get elevation data for these points (zipped to route_sf data)
  elevation_points <- get_elev_point(route_sf, src = "aws")
  
  # Combine elevation points to a dataframe
  elevation_df <- cbind(route_df, elevation = elevation_points$elevation)
  
  distances <- distGeo(route_df[-nrow(route_df), c("lon", "lat")], route_df[-1, c("lon", "lat")])
  
  # Cumulative distance in meters
  cumulative_dist <- c(0, cumsum(distances))
  
  plot_df <- data.frame(
    distance = c(0, distances),
    cumulative_distance = cumulative_dist,
    elevation = elevation_df$elevation
  )
  
  
  route_plot <- qmap(location = 'zürich', zoom = 8) +  # zoom level, the larger the value, the more detailed the map
    geom_path(
      aes(x = lon, y = lat), color = "blue", size = 1, data = route_df
    )
  
  elevation_plot <- 
    ggplot(plot_df, aes(x = cumulative_distance, y = elevation)) +
    #geom_line(color = "steelblue", size = 1) +
    geom_smooth(method = "loess", se = FALSE, span = 0.1, color = "darkred", size = 1) +
    ylim(0, max_ylim) +
    # labs(title = "Elevation Profile of Route",
    #      x = "Distance (meters)",
    #      y = "Elevation (meters)") +
    theme_bw()
  
  return(list(data_route = route_df,
              data_elevation = plot_df,
              elevation_plot = elevation_plot, 
              route_plot = route_plot))
  
}
