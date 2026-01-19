
# libraries ====================================================================
library(tidyverse)
library(magrittr)
library(elevatr)
library(sf)
library(ggmap)
library(geosphere)
library(here)

# load all function from /00_functions =========================================
function_files <- list.files(here("00_functions"), full.names = TRUE)
invisible(sapply(function_files, source))


cities <- data.frame(
  city = c("Zürich", "Basel", "Geneva", "Bern", "Utrecht"),
  country = c(rep("Switzerland", 4), "Netherlands")
)

coords <- cities %>%
  geocode(city = city, country = country, method = "osm")
print(coords)


# api key to use ggmap
# see tutorial https://www.appsilon.com/post/r-ggmap
ggmap::register_google(key = "AIzaSyDJQFPopCR3VfdxOfpOsazM7wtU3_Formg", write = TRUE)

# function in 00_functions/elevation_plot.R
route1 <- elevation_plot("utrecht, netherlands", "chäsalp, switerland")
route2 <- elevation_plot( "chäsalp, switerland", "baden, switzerland")
#route1$elevation_plot
#route2$elevation_plot
#route2$route_plot
#route1$data
# route1$elevation_plot + route2$elevation_plot

# normalize route from utrecht to chäsalp (route 1)
route1$data_elevation$norm_distance <- normalize(route1$data_elevation$distance)
# normalize route from chäsalp to baden (route 2)
route2$data_elevation$norm_distance <- normalize(route2$data_elevation$distance)

route1$data_elevation %<>% 
  mutate(norm_distance = norm_distance + (norm_distance*0.6))

# combine
combined_elevation <- rbind(route1$data_elevation, route2$data_elevation) %>% 
  mutate(cumulative_norm_distance = cumsum(norm_distance))

label_dat <- combined_elevation %>%
  filter(distance == 0 | cumulative_norm_distance == max(cumulative_norm_distance) 
         |  (distance == 0 & cumulative_distance != 0)) %>%
  mutate(elevation = ifelse(elevation == 538, 617.02, elevation),
         location = c("Utrecht", "Chäsalp", "Baden"))

col <- "#f46331"
# plot combined elevation profile
ggplot(combined_elevation, aes(x = cumulative_norm_distance, y = elevation)) +
  #geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.01, color = col, size = 1) +
  geom_text(data = label_dat[2,], aes(x = cumulative_norm_distance, y = elevation, label = paste0(location, '\n', round(elevation), ' m')),
            vjust = 0.1 , hjust = 0.1, color = col, size = 3.7) +
  geom_text(data = label_dat[3,], aes(x = cumulative_norm_distance, y = elevation, label = paste0(location, '\n', round(elevation), ' m')),
            vjust = -0.3 , hjust = 0.1, color = col, size = 3.7) +
  geom_text(data = label_dat[1,], aes(x = cumulative_norm_distance, y = elevation, label = paste0(location, '\n', round(elevation), ' m')),
            vjust = -0.8, hjust =  0.01,
            , color = col, size = 3.7) +
  ylim(0, 1050) + #xlim(0, 123) +
  #geom_hline(yintercept = 617.02, linetype = "dashed", color = "red") +
  #geom_vline(xintercept = 52.52256, linetype = "dashed", color = "red") +
  theme_void() +
  coord_cartesian(clip = 'off') +  # to avoid clipping of text
  theme(plot.margin = unit(c(0, 0.6, 0, 0), "cm"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        )  # increased right margin


# sasve
ggsave("plots/wedding_elevation.png", width = 8, height = 4, bg = "transparent")






