#' Create Gradient Sliders for Traits
#'
#' Generates a visualization of five character traits (Strength, Intelligence,
#' Agility, Charisma, Endurance) with gradient-colored horizontal sliders showing
#' random values from 0 to 10. Optionally adds a decorative box around the plot.
#'
#' @param color_traits A character vector of length 5 specifying the colors for
#'   each trait. Default colors are: red ("#D7263D") for Strength, teal
#'   ("#1B9AAA") for Intelligence, yellow ("#F4D35E") for Agility, purple
#'   ("#6A4C93") for Charisma, and green ("#3F784C") for Endurance.
#' @param add_box Logical. If \code{TRUE} (default), wraps the plot in a
#'   decorative box using \code{box_plot()}.
#' @param color_box Character string specifying the color of the box border.
#'   Default is "darkgrey". Only used when \code{add_box = TRUE}.
#' @param background_box Character string specifying the background color of
#'   the box. Default is "#f8f8f6" (light grey). Only used when
#'   \code{add_box = TRUE}.
#'
#' @return A ggplot2 object (or patchwork composition if \code{add_box = TRUE})
#'   displaying horizontal slider-style bars for each trait with gradient
#'   coloring, tick marks, and value labels.
#'
#' @details
#' Requires the \code{box_plot()} function to be available when \code{add_box = TRUE}.
#'
#' @examples
#' # Basic trait plot with default settings
#' trait_plot()
#'
#' # Custom colors without box
#' trait_plot(
#'   color_traits = c("red", "blue", "green", "orange", "purple"),
#'   add_box = FALSE
#' )
#'
#' # Custom box styling
#' trait_plot(color_box = "black", background_box = "#ffffff")
#'
#' @export


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
    dplyr::mutate(color = color_traits)
  
  
  # ------------------------------ PREPARE DATA --------------------------------
  
  bar_segments <- traits |>
    dplyr::rowwise() |>
    dplyr::do({
      value <- .$value
      data.frame(
        trait = .$trait,
        x = seq(0, value - 0.1, by = 0.1),
        xend = seq(0.1, value, by = 0.1),
        alpha = seq(0.1, value, by = 0.1) / 10
      )
    }) |>
    dplyr::ungroup()
  
  tick_positions <- seq(0, 10, by = 2.5)
  
  ticks <- traits |>
    dplyr::select(trait) |>
    tidyr::crossing(x = tick_positions)
  
  
  # ------------------------------- PLOT SLIDERS -------------------------------
  
  plot_sliders <- ggplot2::ggplot() +
    
    # 1. grey background bars (full length)
    ggplot2::geom_segment(
      data = traits,
      ggplot2::aes(x = 0, xend = 10, y = trait, yend = trait),
      linewidth = 7.5, color = "grey80", lineend = "round") +
    
    # d. colored gradient bars (only until value)
    ggplot2::geom_segment(
      data = bar_segments,
      ggplot2::aes(x = x, xend = xend, y = trait, yend = trait, color = trait, alpha = alpha),
      linewidth = 6, lineend = "round") +
    
    # 3. add ticks
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(x = x, xend = x, y = as.numeric(trait) - 0.17, yend = as.numeric(trait) - 0.10),
      color = "grey40", linewidth = 0.6, alpha = 0.7) +
    
    # 4. add value dots
    ggplot2::geom_point(
      data = traits,
      ggplot2::aes(x = value, y = trait),
      shape = 21, size = 6, stroke = 1.6, color = "grey50", fill = "grey80") +
    
    # 5. add text to value dot
    ggplot2::geom_text(
      data = traits,
      ggplot2::aes(x = value, y = trait, label = round(value, 1), color = trait),
      vjust = -1.9, size = 3.7, fontface = "bold") +
    
    # colors & format
    ggplot2::scale_color_manual(values = traits$color) +
    ggplot2::scale_alpha_continuous(range = c(0.05, 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
  
  # ------------------------------ ADD BOX -------------------------------------
  
  if(add_box){
  
    plot_box <- box_plot(color_box = color_box, background_box = background_box)
    
    plot_sliders <- plot_box + 
      patchwork::inset_element(plot_sliders, left = 0.04, bottom = 0, right = 0.96, top = 1)
  
  }
  
  return(plot_sliders)
  
}
