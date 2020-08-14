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
#' @importFrom ggforce geom_regon
#' @importFrom ggtext geom_richtext
plot.design <- function(x, ...) {
  # browser()
  dots <- list(...)
  if("dag" %in% names(dots)){
    dag <- dots$dag
  } else {
    dag <- get_dag(x)
  }

  dd_dark_blue <- "#3564ED"
  dd_light_blue <- "#72B4F3"
  dd_gray <- gray(0.2)
  dd_light_gray <- gray(0.5)

  aes_df <-
    tibble(
      data_strategy = rep(c(
        "sampling",
        "assignment",
        "measurement",
        "unmanipulated"
      ), 2),
      annotation = stringr::str_to_sentence(data_strategy),
      answer_strategy = rep(c(
        "controlled",
        "uncontrolled"
      ), each = 4),
      fill = if_else(data_strategy == "unmanipulated", NA_character_, dd_light_blue),
      linetype = case_when(answer_strategy == "controlled" ~ "dashed", answer_strategy == "uncontrolled" ~ "solid"),
      size = case_when(answer_strategy == "controlled" ~ 0.2, answer_strategy == "uncontrolled" ~ 0),
      color_manipulated = if_else(data_strategy == "unmanipulated", dd_gray, dd_gray),
      size_manipulated = if_else(data_strategy == "unmanipulated", 0, 0.3),
      sides = case_when(
        data_strategy == "sampling" ~ 3,
        data_strategy == "assignment" ~ 1000,
        data_strategy == "measurement" ~ 4,
        data_strategy == "unmanipulated" ~ 4
      ),
      angle = case_when(
        data_strategy == "sampling" ~ pi,
        data_strategy == "assignment" ~ 0,
        data_strategy == "measurement" ~ 0,
        data_strategy == "unmanipulated" ~ 0
      ),
      r = case_when(
        data_strategy == "sampling" ~ .35,
        data_strategy == "assignment" ~ .25,
        data_strategy == "measurement" ~ .35,
        data_strategy == "unmanipulated" ~ .35
      ),
      shape_nudge_y = case_when(
        data_strategy == "sampling" ~ 0.05,
        data_strategy == "assignment" ~ 0,
        data_strategy == "measurement" ~ 0,
        data_strategy == "unmanipulated" ~ 0
      )
    ) %>%
    mutate(r = r * 0.65)

  # browser()

  gg_df <- make_design_dag_df(x, dag) %>%
    mutate(answer_strategy = "uncontrolled") %>%
    left_join(aes_df) %>%
    mutate(shape_y = y + shape_nudge_y)

  family <- "Helvetica"

  epsilon <- .29

  blank_df <-
    tibble(x = c(min(gg_df$x), max(gg_df$x)) + 1,
           y = c(min(gg_df$y), max(gg_df$y)))

# browser()

  ggplot(data = gg_df, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
    # edges
    geom_dag_edges_arc(
      edge_colour = dd_light_gray,
      edge_width = 0.7,
      curvature = -0.25
    ) +
    # fill
    geom_regon(
      aes(
        x0 = x,
        y0 = shape_y,
        sides = sides,
        angle = angle,
        r = r * 0.89,
        fill = fill,
        color = color_manipulated,
        size = size_manipulated
      )
    ) +

    # lty
    geom_regon(
      aes(
        x0 = x,
        y0 = shape_y,
        sides = sides,
        angle = angle,
        linetype = linetype,
        size = size,
        r = r),
      fill = NA,
      color = dd_gray
    ) +

    # Letters in nodes
    geom_richtext(
      color = dd_gray,
      aes(label = name),
      family = family,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      size = 7
    ) +
    # Annotations above (below) nodes
    geom_richtext(
      color = dd_gray,
      aes(label = annotation, x = x, y = y),
      nudge_x = 0.5,
      family = family,
      size = 4,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    geom_blank(data = blank_df, aes(xend = NULL,
                                    yend = NULL)) +
    # scales
    coord_fixed() +
    scale_fill_identity() +
    scale_linetype_identity() +
    scale_size_identity() +
    scale_color_identity() +
    theme_dag() +
    theme(legend.position = "none",
          plot.margin = unit(rep(0, 4), "cm"))



  # g <-
  #   gg_df %>%
  #   ggplot(aes(
  #     x = x,
  #     y = y,
  #     xend = xend,
  #     yend = yend,
  #   )) +
  #   geom_point(stroke = 0,
  #              aes(shape = data_strategy, fill = data_strategy),
  #              size = 10) +
  #   geom_dag_edges_arc(curvature = -0.085, edge_colour = "lightgray") +
  #   geom_text(color = "black",
  #             parse = TRUE,
  #             aes(label = name),
  #             size = 4) +
  #   scale_fill_manual(values = colors) +
  #   scale_shape_manual(values = shapes) +
  #   theme_dag() +
  #   # coord_cartesian(xlim = c(.8, 1.2)) +
  #   theme(legend.position = "none") +
  #   guides(fill = guide_legend(override.aes = list(size=4)))
  # g
}






# plot(design) + dd_theme()
