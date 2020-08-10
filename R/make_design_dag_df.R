


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
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join mutate if_else `%>%` transmute select
make_design_dag_df <- function(design, dag) {

  dag_df <- tidy_dagitty(dag) %>%
    select(name, direction, to, circular) %>%
    as_tibble

  design_nodes_df <-
    design %>%
    get_design_nodes %>%
    filter(name %in% dag_df$name) %>%
    mutate(y = -1*order(causal_order),
           x = 1
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
