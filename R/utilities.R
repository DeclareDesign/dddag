#' @importFrom rlang new_formula
formula_from_vars <- function(lhs, rhs) {
  new_formula(str2lang(lhs), str2lang(paste0(rhs, collapse = "+")))
}

#' Title
#'
#' @param step
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom purrr map2
#' @importFrom rlang f_lhs f_rhs is_call is_formula call_args
get_vars_from_step <- function(step) {
  call <- attr(step, "call")
  type <- attr(step, "step_type")
  if (is_call(call) && is.character(type) && type != "wrapped") {
    formulae <- Filter(is_formula, call_args(call))
    if (length(formulae) == 1) {
      lhs <- as.character(f_lhs(formulae[[1]]))
      rhs <- all.vars(f_rhs(formulae[[1]])) %>% setdiff("N")
      if(lhs != "N" && length(rhs) > 0)
        return(list(formula_from_vars(lhs, rhs))[lhs != "N"])
      # return(list(formulae[[1]]))
    } else if (length(formulae) == 0 && type != "reveal") {
      exprs <- call_args(call)
      lhs <- as.list(names(exprs))
      # vars <- map2(lhs, exprs, function(x, y) list(x, all.vars(y)))
      has_parents <- sapply(exprs, function(x) length(all.vars(x)) != 0)
      formulae <- map2(lhs[has_parents && lhs != "N"], exprs[has_parents && lhs != "N"],
                       function(x, y) formula_from_vars(x, all.vars(y) %>% setdiff("N")))
      return(formulae)
    } else if (type == "reveal") {
      # kludge
      exprs <- call_args(call)
      formulae <- list(formula_from_vars(as.character(exprs[[1]]), as.character(exprs[[2]])))
    }
  }
  return(NULL)
}
