#' Title
#'
#' @param x Design object
#' @param ... dag object
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text scale_fill_manual scale_shape_manual coord_cartesian theme guides guide_legend
#' @importFrom ggdag geom_dag_node theme_dag geom_dag_edges_arc
plot.design <- function(x, ...) {
  dots <- list(...)
  if("dag" %in% names(dots)){
    dag <- dots$dag
  } else {
    dag <- get_dag(x)
  }

  dd_dark_blue <- "#3564ED"
  dd_light_blue <- "#72B4F3"

  shapes <-
    c(
      "sampling" = 25,
      "assignment" = 23,
      "measurement" = 22,
      "unmanipulated" = 21
    )
  colors <-
    c(
      "sampling" = dd_light_blue,
      "assignment" = dd_light_blue,
      "measurement" = dd_light_blue,
      "unmanipulated" = "lightgray"
    )

  gg_df <- make_design_dag_df(x, dag)

  g <-
    gg_df %>%
    ggplot(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
    )) +
    geom_point(stroke = 0,
               aes(shape = data_strategy, fill = data_strategy),
               size = 10) +
    geom_dag_edges_arc(curvature = -0.03, edge_colour = "lightgray") +
    geom_text(color = "black",
              parse = TRUE,
              aes(label = label),
              size = 4, nudge_x = 0.02, hjust = 0) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_dag() +
    coord_cartesian(xlim = c(.8, 1.2)) +
    theme(legend.position = "none") +
    guides(fill = guide_legend(override.aes = list(size=4)))
  g
}






# plot(design) + dd_theme()
