#' @describeIn figures draw fig. 5
#' @export
#'
draw_fig_5 <- function(data, width_petal = 0.5) {
  
  ## extract gof info:
  gof_data <- goodness_of_fit(data)
  
  ## merge datasets and drop slopes level 2:
  dplyr::full_join(data, gof_data, by = "scenario") |>
    dplyr::group_by(.data$scenario, .data$seed) |>
    dplyr::slice(1L) |>
    dplyr::select(-.data$slopes_level2) -> data_plot
  
  ## reformat data to name and order scenarios properly:
  data_plot |>
    dplyr::mutate(scenario = ifelse(.data$scenario  == "base_model", "0", .data$scenario),
                  scenario = factor(.data$scenario,
                                    levels = c("PISH", "PIS", "PIH", "PSH", "PI", "PS", "PH",
                                               "P", "ISH", "IS", "IH", "I", "SH", "S", "H",
                                               "0"))) -> data_plot
  
  ## flower plot (an idea from us!):
  data_plot |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$scenario, y = .data$slopes_level1, fill = .data$pv_gof) +
    ggplot2::coord_polar(start = -pi/length(unique(data_plot$scenario))) +
    ggplot2::geom_violin(size = 0.1, width = width_petal) + ## change width to change the width of the petals
    ggplot2::geom_hline(yintercept = 0, colour = "black", size = 0.2) +
    ggplot2::geom_hline(yintercept = unique(data_plot$slope_observed),
                        colour = "darkgreen", linetype = "dashed", size = 0.2) +
    ggplot2::scale_fill_gradient2(low = "#0018ffff", high = "yellow",
                                  midpoint = log(0.05, 10), limits  = c(min(c(0.005, data_plot$pv_gof)), 1), trans = "log10",
                                  breaks = c(0.005, 0.05, 1),
                                  guide = ggplot2::guide_colorbar(barwidth = ggplot2::unit(3.5, "cm"),
                                                                  barheight = ggplot2::unit(0.4, "cm"),
                                                                  ticks.colour = "black",
                                                                  title.vjust = 0.8)) +
    ggplot2::labs(fill = "p-value") +
    theme_twin() +
    ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 6),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 6),
                   legend.title = ggplot2::element_text(size = 7),
                   legend.title.align = 0.5,
                   legend.margin = ggplot2::margin(c(0, 0, 0, 0)),
                   legend.box.spacing	= ggplot2::unit(0, units = "cm"))
  
}