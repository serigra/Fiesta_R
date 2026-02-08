


trait_plot <- function(
    color_traits = c("#D7263D", "#1B9AAA", "#F4D35E", "#6A4C93", "#3F784C"),
    add_box = TRUE,
    color_box = "darkgrey",
    background_box = "#f8f8f6"
  ){
  
  traits <- data.frame(
    trait = factor(
      c("Strength", "Intelligence", "Agility", "Charisma", "Endurance"),
      levels = rev(c("Strength", "Intelligence", "Agility", "Charisma", "Endurance"))
    ),
    value = runif(5, 0, 10)
  ) |> 
    mutate(color = color_traits)
  
  
  # ------------------------------ PREPARE DATA --------------------------------
  
  bar_segments <- traits %>%
    dplyr::rowwise() %>%
    do({
      value <- .$value
      data.frame(
        trait = .$trait,
        x = seq(0, value - 0.1, by = 0.1),
        xend = seq(0.1, value, by = 0.1),
        alpha = seq(0.1, value, by = 0.1) / 10
      )
    }) %>%
    ungroup()
  
  tick_positions <- seq(0, 10, by = 2.5)
  
  ticks <- traits %>%
    dplyr::select(trait) %>%
    tidyr::crossing(x = tick_positions)
  
  
  # ------------------------------- PLOT SLIDERS -------------------------------
  
  plot_sliders <- ggplot2::ggplot() +
    
    # 1. grey background bars (full length)
    ggplot2::geom_segment(
      data = traits,
      aes(x = 0, xend = 10, y = trait, yend = trait),
      linewidth = 7.5, color = "grey80", lineend = "round") +
    
    # d. colored gradient bars (only until value)
    ggplot2::geom_segment(
      data = bar_segments,
      aes(x = x, xend = xend, y = trait, yend = trait, color = trait, alpha = alpha),
      linewidth = 6, lineend = "round") +
    
    # 3. add ticks
    ggplot2::geom_segment(
      data = ticks,
      aes(x = x, xend = x, y = as.numeric(trait) - 0.17, yend = as.numeric(trait) - 0.10),
      color = "grey40", linewidth = 0.6, alpha = 0.7) +
    
    # 4. add value dots
    ggplot2::geom_point(
      data = traits,
      aes(x = value, y = trait),
      shape = 21, size = 6, stroke = 1.6, color = "grey50", fill = "grey80") +
    
    # 5. add text to value dot
    ggplot2::geom_text(
      data = traits,
      aes(x = value, y = trait, label = round(value, 1), color = trait),
      vjust = -1.9, size = 3.7, fontface = "bold") +
    
    # colors & format
    ggplot2::scale_color_manual(values = traits$color) +
    ggplot2::scale_alpha_continuous(range = c(0.05, 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold"),
      axis.text.x = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # ------------------------------ ADD BOX -------------------------------------
  
  if(add_box){
  
    plot_box <- box_plot(color_box = color_box, background_box = background_box)
    
    plot_sliders <- plot_box + 
      patchwork::inset_element(plot_sliders, left = 0.04, bottom = 0, right = 0.96, top = 1)
  
  }
  
  return(plot_sliders)
  
}
