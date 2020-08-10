#' Title
#'
#' @param design
#' @param dag
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggdag geom_dag_node geom_dag_text geom_dag_edges theme_dag
plot_design <- function(design, dag) {
  gg_df <- make_design_dag_df(design, dag)
  ggplot(gg_df, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
    geom_dag_node(color = "gray", aes(shape = data_strategy)) +
    geom_dag_text(color = "black",
                  parse = TRUE,
                  aes(label = name),
                  size = 4) +
    geom_dag_edges() +
    # theme_dag() +
    scale_shape_manual(values = c(
      "sampling" = 6,
      "assignment" = 5,
      "measurement" = 1,
      "unmanipulated" = NA
    ))
}






# plot(design) + dd_theme()
