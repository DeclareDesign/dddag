library(DeclareDesign)
library(tidyverse)
library(ggdag)


get_design_nodes <- function(design) {

  design_summary <- summary(design)

  step_types <- design_summary$function_types %>% flatten_chr


  assignment_vars <-
    tibble(
      name = design_summary$variables_added[step_types == "assignment"] %>% map(names) %>% flatten_chr,
      step_type = "assignment"
    )
  sampling_vars <-
    tibble(
      name = design_summary$variables_added[step_types == "sampling"] %>% map(names) %>% flatten_chr,
      step_type = "sampling"
    )
  measurement_vars <-
    tibble(
      name = design_summary$variables_added[step_types == "measurement"] %>% map(names) %>% flatten_chr,
      step_type = "measurement"
    )

  bind_rows(
    assignment_vars,
    sampling_vars,
    measurement_vars
  )
}


ggdd <- function(dag, design){

  dag_df <- tidy_dagitty(dag) %>% as_tibble

  design_df <- get_design_nodes(design)


  gg_df <- left_join(dag_df, design_df) %>%
    # hackity dackity doo
    mutate(step_type = case_when(name == "S" ~ "sampling",
                                 is.na(step_type) ~ "unmanipulated",
                                 TRUE ~ step_type))

  return(gg_df)
}



dag <- dagify(Y ~ X + Z + S,
              Z ~ S,
              Yobs ~ Y)

design <-
  declare_population(N = 100, X = rnorm(N)) +
  declare_potential_outcomes(Y ~ X + Z) +
  declare_sampling(n = 10) +
  declare_assignment() +
  declare_assignment(assignment_variable = "Z2") +
  declare_measurement(Yobs = Y)

gg_df <- ggdd(dag, design)





g <-
  ggplot(gg_df, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_node(color = "gray", aes(shape = step_type)) +
  geom_dag_text(color = "black",
                parse = TRUE,
                aes(label = name),
                size = 4) +
  geom_dag_edges() +
  theme_dag()
g


