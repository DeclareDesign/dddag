
library(DeclareDesign)
library(dddag)
library(ggdag)

# test_that("plot DAG", {
  design <-
    declare_population(N = 100,
                       X = rnorm(N),
                       Q = X) +
    declare_potential_outcomes(Y ~ X + Q + Z) +
    declare_sampling(n = 10) +
    declare_assignment() +
    declare_assignment(assignment_variable = "Z2") +
    declare_step(bob = 5, handler = fabricate) +
    declare_measurement(Yobs = Y + X)

  dag <- dagify(Y ~ X + Z + S,
                Q ~ X,
                Z ~ S,
                Yobs ~ Y)

  dddag:::get_design_nodes(design)

  # debugonce(make_design_dag_df)
  make_design_dag_df(design, dag)


   dd_dark_blue <- "#3564ED"
   dd_light_blue <- "#72B4F3"

   gg_df <- make_design_dag_df(design, dag)

   ggplot(gg_df, aes(
     x = x,
     y = y,
     xend = xend,
     yend = yend
   )) +
     geom_dag_node(color = dd_dark_blue,
                   stroke = 3,
                   size = 12,
                   internal_colour = dd_dark_blue,
                   aes(shape = data_strategy)) +
     geom_dag_text(color = "black",
                   parse = TRUE,
                   aes(label = name),
                   size = 4) +
     geom_dag_edges(edge_colour = dd_light_blue) +
     scale_shape_manual(
       values = c(
         "sampling" = 6,
         "assignment" = 5,
         "measurement" = 1,
       "unmanipulated" = NA
     )) +
     theme_dag() +
     theme(legend.position = "none",
           legend.title = element_blank())

# })

