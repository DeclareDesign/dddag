


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
#' @importFrom dplyr left_join mutate case_when `%>%`
make_design_dag_df <- function(design, dag) {

  dag_df <- tidy_dagitty(dag) %>% as_tibble

  design_df <- get_design_nodes(design)

  gg_df <-
    dag_df %>%
    left_join(design_df) %>%
    mutate(
      data_strategy = case_when(
        name == "S" ~ "sampling",
        TRUE ~ data_strategy
      )
    )

  return(gg_df)
}
