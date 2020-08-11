
#' Title
#'
#' @param design
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggdag dagify
#' @importFrom purrr flatten
#'
get_dag <- function(design) {
  formulae <- lapply(design, get_vars_from_step) %>% unname %>% flatten
  do.call(dagify, formulae)
}
