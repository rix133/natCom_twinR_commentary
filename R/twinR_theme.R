#' Theme for plots
#'
#' This is the custom ggplot theme that is used.
#' The theme is built around [`theme_bw`][`ggplot2::theme_bw`] and is inspired by
#' [`theme_twin`][`twinR::theme_twin`].
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param larger a scalar indicating by how much to increase text size compared to default 
#' @param legend_pos a numeric vector of length 2 specifying the x and y coordinates of the legend position.
#' @param legend_dir a character string specifying the direction of the legend ("horizontal" or "vertical").
#'
#' @return the theme
#' @export
#' @seealso \code{\link[twinR]{theme_twin}}
base_theme <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
base_rect_size = base_size/22, larger = 0, legend_pos = c(0.3,0.9), legend_dir="horizontal") 
{
  gray <- "#4D4D4D"
 black <- "#000000"
  ggplot2::theme_bw(base_size = base_size, base_family = base_family, 
                    base_line_size = base_line_size, base_rect_size = base_rect_size) + 
  ggplot2::theme(line = ggplot2::element_line(colour = gray), 
    rect = ggplot2::element_rect(fill = "white", colour = NA), 
    text = ggplot2::element_text(colour = black), axis.ticks = ggplot2::element_line(colour = gray), 
    legend.key = ggplot2::element_rect(colour = NA), 
    legend.position = legend_pos,
    legend.direction = legend_dir,
    panel.grid.minor = element_blank(),
    panel.border = ggplot2::element_rect(colour = gray), 
    strip.background = ggplot2::element_rect(fill = "white",  colour = NA),
    title = ggplot2::element_text(colour = black, size = 7 + larger, family = base_family),
    legend.text = ggplot2::element_text(colour = black, size = 5 + larger, family = base_family),
    axis.text.x = ggplot2::element_text(colour = black, size = 5 + larger, family = base_family),
    axis.text.y = ggplot2::element_text(colour = black, size = 5 + larger, family = base_family),
    axis.title.x = ggplot2::element_text(colour = black, size = 7 + larger, family = base_family, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(colour = black, size = 7 + larger, family = base_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 10)),
    plot.margin = ggplot2::margin(3, 1, 3, 1))
}

#' Draw Figure 4C Like Plot
#'
#' This function creates a ggplot object that visualizes per-birth twinning
#'  probability by maternal age and parity like in the figure 4C in twinR package
#'
#' @param data A data frame containing the data to be plotted.
#' @param larger Numeric value specifying the size of the plot elements.
#' @param y_lims A numeric vector of length 2 specifying the y-axis limits.
#' @param legend_pos A numeric vector of length 2 specifying the 
#' x and y coordinates of the legend.
#' @param legend_dir Character string specifying the direction of the 
#' legend ("horizontal" or "vertical").
#'
#' @return A ggplot object.
#' @import ggplot2
#' @seealso \code{\link[twinR]{draw_fig_4C}}
draw_fig_4C <- function(data,
                        larger=8,
                        y_lims = c(0.01, 0.04),
                        legend_pos = c(0.2,0.8),
                        legend_dir="vertical") {
  
  ggplot2::ggplot(data) +
    ggplot2::aes(x = age,
                 y = twin,
                 col = as.factor(parity)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_viridis_d("Parity",
                                   guide = ggplot2::guide_legend(
                                     title.vjust = 0.5, ncol = 1)) +
    ggplot2::scale_y_continuous("Per-birth twin. prob.",
                                breaks = seq(0.01, 0.04, by = 0.005)) +
    ggplot2::scale_x_continuous("Maternal age",
                                breaks = seq(20, 45, by = 5)) +
    ggplot2::expand_limits(y = y_lims) +
    ggplot2::expand_limits(x = c(20, 45)) +
    base_theme(larger = larger, legend_dir = legend_dir, legend_pos = legend_pos) +
    ggplot2::theme(legend.margin = ggplot2::margin(c(0, 0, 0, 0)))
}
