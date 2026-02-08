#' Create Elevation and Route Plots for a Journey from A to B
#'
#' Generates elevation profile and route map visualizations for a journey between
#' two locations. The function retrieves route information, calculates elevations
#' along the path, and creates both an elevation profile plot and a route map.
#'
#' @param origin Character string specifying the starting location (default: "utrecht, netherlands").
#'   Can be an address, city name, or other location identifier recognized by Google Maps.
#' @param destination Character string specifying the ending location (default: "chäsalp, switzerland").
#'   Can be an address, city name, or other location identifier recognized by Google Maps.
#' @param structure Character string specifying the route structure (default: "route").
#'   Passed to \code{\link[ggmap]{trek}}.
#' @param mode Character string specifying the travel mode (default: 'bicycling').
#'   Options include 'bicycling', 'driving', 'walking', or 'transit'.
#' @param max_ylim Numeric value setting the maximum y-axis limit for the elevation plot
#'   in meters (default: 1100).
#' @param add_text Logical indicating whether to add a text labels to the plot (default: FALSE).
#' @param add_box Logical indicating whether to add a box around the plot (default: FALSE).
#' @param color_route Character string specifying the color for the route line (default: "blue").
#' @param color_profile Character string specifying the color for the elevation profile line (default: "#f46331").
#' @param color_box Character string specifying the color for the box (default: "darkgrey").
#' @param background_box Character string specifying the background color for the box (default: "#f8f8f6").
#'
#' @return A list containing four elements:
#'   \item{data_route}{Data frame with route coordinates (lon, lat)}
#'   \item{data_elevation}{Data frame with distance and elevation data}
#'   \item{plot_elevation}{ggplot object showing the elevation profile with LOESS smoothing}
#'   \item{plot_route}{ggmap object showing the route on a map centered on Zürich}
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Retrieves route data using Google Maps API via \code{ggmap::trek()}
#'   \item Converts route points to spatial features (sf objects)
#'   \item Obtains elevation data from AWS for each point along the route
#'   \item Calculates cumulative distances between consecutive points
#'   \item Creates an elevation profile plot with LOESS smoothing (span = 0.1)
#'   \item Generates a route map visualization
#' }
#'
#' Requires the \code{box_plot()} function to be available when \code{add_box = TRUE}.
#'
#' @note
#' This function requires the following packages: dplyr, stringr, ggplot2, monochromeR, patchwork, ggforce, ggmap, sf, elevatr, and geosphere.
#' A API key to the Google cloud platform is needed! see tutorial https://www.appsilon.com/post/r-ggmap.
#' An active internet connection is required to retrieve route and elevation data.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' result <- elevation_plot()
#'
#' # Custom route
#' result <- elevation_plot(
#'   origin = "amsterdam, netherlands",
#'   destination = "geneva, switzerland",
#'   mode = "driving",
#'   max_ylim = 2000
#' )
#'
#' # View the plots
#' print(result$plot_elevation)
#' print(result$plot_route)
#' }
#'
#' @export

elevation_plot <- function(origin = "utrecht, netherlands", 
                           destination = "chäsalp, switzerland",
                           structure = "route",
                           mode = 'bicycling',
                           max_ylim = 1100,
                           add_text = FALSE, 
                           add_box = FALSE,
                           color_route = "blue",
                           color_profile = "#f46331",
                           color_box = "darkgrey",
                           background_box = "#f8f8f6"
                           ) {
  
  # ---------------------------- PREPARE DATA ----------------------------------
  
  # create labels based on origin and destination
  origin_label <- stringr::str_to_title(stringr::str_extract(origin, "^[^,]+"))
  destination_label <- stringr::str_extract(destination, "^[^,]+")
  if(grepl("chäsalp", destination_label)){
    destination_label <- "Chäsalp\n617 m"
  }
  
  # generate route dataframe with lon/lat points
  data_route <- ggmap::trek(origin, destination, 
                            mode = mode,
                            structure = structure, output = 'simple')
  
  # convert to sf object with lon/lat
  data_route_sf <- sf::st_as_sf(data_route, coords = c("lon", "lat"), crs = 4326)
  
  # get elevation data for these points
  elevation_points <- elevatr::get_elev_point(data_route_sf, src = "aws")
  
  # combine elevation points to a dataframe
  elevation_df <- cbind(data_route, elevation = elevation_points$elevation)
  
  distances <- geosphere::distGeo(data_route[-nrow(data_route), c("lon", "lat")], data_route[-1, c("lon", "lat")])
  
  # cumulative distance in meters
  cumulative_dist <- c(0, cumsum(distances))
  
  data_elevation <- data.frame(
    distance = c(0, distances),
    cumulative_distance = cumulative_dist,
    elevation = elevation_df$elevation
  )
  
  # --------------------------------- PLOT ROUTE -------------------------------
  
  plot_route <- ggmap::qmap(location = 'zürich', zoom = 8) +  # zoom level, the larger the value, the more detailed the map
    ggplot2::geom_path(
      ggplot2::aes(x = lon, y = lat), color = color_route, linewidth = 1, data = data_route
    )
  
  # ---------------------------- PLOT ELEVATION PROFILE ------------------------
  
  plot_elevation <- 
    ggplot2::ggplot(data_elevation, ggplot2::aes(x = cumulative_distance, y = elevation)) +
    #ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, span = 0.1, color = color_profile, size = 1) +
    ggplot2::ylim(0, max_ylim) +
    ggplot2::theme_void() 
  
  # --------------------------- ADD TEXT ANNOTATIONS ---------------------------
  
  if(add_text){ 
    
    range_elevation <- data_elevation |> 
      dplyr::summarise(min_elev = min(elevation),
                max_elev = max(elevation),
                plot_min = min_elev - 0.18 * (max_elev - min_elev),
                plot_max = max_elev + 0.8 * (max_elev - min_elev))
    
    plot_elevation <- plot_elevation + 
      ggplot2::geom_text(data = data_elevation |> dplyr::filter(distance == 0), 
                         ggplot2::aes(x = distance, y = elevation, label = paste0(origin_label, '\n', round(elevation), ' m')),
                vjust = -0.3 , hjust = 0.1, color = color_profile, size = 3.7) +
      ggplot2::geom_text(data = data_elevation |> tail(1), 
                         ggplot2::aes(x = cumulative_distance, y = elevation, label = destination_label),
                vjust = -0.3 , hjust = 0.8, color = color_profile, size = 3.7) +
      ggplot2::ylim(range_elevation$plot_min, 
           range_elevation$plot_max) + 
      ggplot2::theme_void() +
      ggplot2::coord_cartesian(clip = 'off') +  # to avoid clipping of text
      ggplot2::theme(plot.margin = ggplot2::unit(c(0.2, 0.4, 0.2, 0.4), "cm"),
            panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
            plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
            legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
            legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
      ) 
  }
  
  # ------------------------ ADD BOX & DELTA ELEVATION  ------------------------
  
  if(add_box){ 
    
    plot_box <- box_plot(color_box = color_box, background_box = background_box)
    
    # calculate elevation difference between start and end point
    delta_elevation <- tail(data_elevation$elevation, 1) - head(data_elevation$elevation, 1)
    if(grepl("chäsalp", destination)){
      delta_elevation <- 617 - head(data_elevation$elevation, 1)
    }
    delta_elevation <- paste0(delta_elevation, " m")
    
    color_profile_dark <- monochromeR::generate_palette(color_profile, modification = "go_darker", n_colors = 5)[2]
    
    plot_delta <- ggplot2::ggplot() +
      ggplot2::annotate("text",
               x = 0.5, y = 0.5,
               label = bquote(atop(Delta~Altitude, bold(.(delta_elevation)))),
               size = 5,
               color = color_profile_dark) +
      ggplot2::theme_void()
    
    plot_elev_delta <- plot_elevation + plot_delta + 
      patchwork::plot_layout(widths = ggplot2::unit(c(4, 1), c("null")))
    
    plot_elevation <- plot_box + 
      patchwork::inset_element(plot_elev_delta, left = 0.005, bottom = 0, right = 0.93, top = 1)
    
  }
  
  # ----------------------------------------------------------------------------
  
  return(list(data_route = data_route,
              data_elevation = data_elevation,
              plot_elevation = plot_elevation, 
              plot_route = plot_route))
  
}
