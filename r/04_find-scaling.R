source("r/header.R")

df <- crossing(
  p_colonize = c(0.025, 0.05, 0.1, 0.2, 0.4),
  D = set_units(10 ^ seq(1, 3.5, by = 0.025), 1 / mm ^ 2),
  H = c(0, 0.01, 0.1)
) %>%
  rowwise() %>%
  mutate(S = find_S(p_colonize, D, H))

write_rds(df, "objects/scaling-results.rds")
