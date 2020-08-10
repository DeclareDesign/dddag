


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
    as_tibble

  design_nodes_df <-
    design %>%
    get_design_nodes

    gg_df <-
    dag_df %>%
    left_join(design_nodes_df, by = "name")

  return(gg_df)
}
