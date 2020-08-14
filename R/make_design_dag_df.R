


#' Title
#'
#' @param design A DeclareDesign design
#' @param dag a
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join mutate if_else `%>%` transmute select filter
#' @importFrom ggdag tidy_dagitty
make_design_dag_df <- function(design, dag) {

  dag_df <- tidy_dagitty(dag) %>%
    select(name, direction, to, circular) %>%
    as_tibble

  # browser()
  design_nodes_df <-
    design %>%
    get_design_nodes %>%
    filter(name %in% dag_df$name) %>%
    mutate(
      x = order(causal_order)^0.5,
      y = -1 * order(causal_order)^(1.5),
      x = (x - mean(x))/sd(x),
      y = (y - mean(y))/sd(y)
    )

  endnodes_df <-
    design_nodes_df %>%
    transmute(to = name, xend = x, yend = y)

  gg_df <-
    dag_df %>%
    left_join(design_nodes_df, by = "name") %>%
    left_join(endnodes_df, by = "to") %>%
    mutate(label = paste0(name, " (", data_strategy, ")"))

  return(gg_df)
}
