
library(DeclareDesign)

test_that("plot work", {
  design <-
    declare_population(N = 100,
                       x_prob = runif(N),
                       X = rbinom(N, 1, prob = x_prob),
                       # R = h(A, g(X, f(Q))),
                       Q = X) +
    declare_potential_outcomes(Y ~ 2 * X + Q + Z + Z2, assignment_variables = c(Z, Z2)) +
    declare_sampling(n = 10) +
    declare_assignment(m = 5) +
    declare_assignment(assignment_variable = "Z2") +
    declare_measurement(Yobs = Y + X)

  gg <- plot(design)

  expect_true(inherits(gg, "gg"))

})


# get_formula_from_step <- function(step) {
#   call <- attr(step, "call")
#   type <- attr(step, "step_type")
#   if (is_call(call) && is.character(type) && type != "wrapped") {
#     formulae <- Filter(is_formula, call_args(call))
#     if (length(formulae) == 1) {
#       return(formulae[[1]])
#     }
#   }
#   return(NULL)
# }
#
# get_expr_from_step <- function(step) {
#   call <- attr(step, "call")
#   type <- attr(step, "step_type")
#   if (is_call(call) && is.character(type) && type != "wrapped") {
#     exprs <- call_args(call)
#     lhs <- as.list(names(exprs))
#     vars <- map2(lhs, exprs, function(x, y) list(x, all.vars(y)))
#   }
#   return(vars)
# }

# h <- function(x) x^2
# g <- function(x) x*2
# f <- function(x) sqrt(x)
