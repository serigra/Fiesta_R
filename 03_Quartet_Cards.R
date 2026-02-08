# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(patchwork)
library(ggforce)

library(elevatr)
library(sf)
library(ggmap)
library(geosphere)


# read data --------------------------------------------------------------------
source('01_Data_Prep.R')

d.cards.traits |>
  select(Guest, starts_with('trait')) |>
  head()


set.seed(42)

# (1) visualize traits in sliders with a box around ----------------------------
plot.A <- trait_plot(color_traits = c("#f330e1", "#f46331", "#f8c330", "#31f330", "#31f8c3")
                     ); plot.A

# (2) plot elevation profile ---------------------------------------------------
route <- elevation_plot(origin = "Baden, Switzerland", 
                        destination = "chäsalp, switzerland", 
                        add_box = TRUE, add_text = TRUE,
                        color_profile = "darkgreen")

plot.B <- route$plot_elevation; plot.B

# (3) combine plots ------------------------------------------------------------
plot.B / plot.A +
  plot_layout(heights = unit(c(2, 5), c("null")) )


# (4) save
ggsave("plots/wedding_elevation.png", width = 8, height = 4, bg = "transparent")





