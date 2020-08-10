


library(DeclareDesign)
library(dddag)
library(ggdag)
library(tidyverse)

# test_that("plot DAG", {
design <-
  declare_population(N = 100,
                     X = rnorm(N),
                     Q = X) +
  declare_potential_outcomes(Y ~ X + Q + Z + Z2, assignment_variables = c(Z, Z2)) +
  declare_sampling(n = 10) +
  declare_assignment() +
  declare_assignment(assignment_variable = "Z2") +
  declare_measurement(Yobs = Y + X)


dag <- dagify(Y ~ X + Z + Z2,
              Q ~ X,
              Z ~ S,
              Z2 ~ S,
              Yobs ~ Y)


dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"

shapes <-
  c(
    "sampling" = 25,
    "assignment" = 23,
    "measurement" = 22,
    "unmanipulated" = 21
  )
colors <-
  c(
    "sampling" = dd_light_blue,
    "assignment" = dd_light_blue,
    "measurement" = dd_light_blue,
    "unmanipulated" = "lightgray"
  )



gg_df <- make_design_dag_df(design, dag)

g <-
gg_df %>%
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
  geom_point(stroke = 0,
             aes(shape = data_strategy, fill = data_strategy),
             size = 10) +
  geom_dag_edges_arc(curvature = -0.03, edge_colour = "lightgray") +
  geom_text(color = "black",
                parse = TRUE,
                aes(label = label),
                size = 4, nudge_x = 0.02, hjust = 0) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  theme_dag() +
  coord_cartesian(xlim = c(.8, 1.2)) +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size=4)))

g

# ggsave("test.pdf", g, width = 6.5,height = 2)

# })

