
#' Title
#'
#' @param design
#'
#' @return
#' @export
#'
#' @examples
#' my_design <- DesignLibrary::two_arm_designer()
#' get_design_nodes(my_design)
#'
#' @importFrom purrr pluck flatten_chr map_df map
#' @importFrom dplyr mutate case_when
#' @importFrom tibble tibble
get_design_nodes <- function(design) {
  # this function extracts the variables added to a df by a design in order
  #   along with a column indicating the step type that created the variable

  design_summary <- summary(design)

  design_summary %>%
    pluck("function_types") %>%
    flatten_chr %>%
    seq_along %>%
    map_df(
      ~ tibble(
        step_type = design_summary$function_types[.] %>% flatten_chr,
        causal_order = .,
        name = design_summary$variables_added[.] %>% map(names) %>% flatten_chr
      )
    ) %>%
    mutate(
      data_strategy = case_when(
        step_type %in% c("population", "potential_outcomes", "reveal", "custom") ~ "unmanipulated",
        TRUE ~ step_type
      ),
      name = if_else(name == "S_inclusion_prob", "S", name)
    )
}

