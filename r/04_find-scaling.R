source("r/header.R")

# Spatially explicit model ----
df <- crossing(
  p_colonize = c(0.025, 0.05, 0.1, 0.2, 0.4),
  D = set_units(10 ^ seq(1, 3.5, by = 0.025), 1 / mm ^ 2),
  H = c(0, 0.01, 0.1)
) %>%
  rowwise() %>%
  mutate(S = find_S(p_colonize, D, H))

write_rds(df, "objects/spatially-explicit-scaling-results.rds")

# Spatially implicit model ----
df <- crossing(
  p_colonize = c(0.25, 0.5, 0.75),
  D = set_units(10 ^ seq(1, 3.5, by = 0.025), 1 / mm ^ 2),
  H = c(0.01, 0.1)
) %>%
  rowwise() %>%
  # Multiplying by 10^6 to convert mm^2 to um^2
  mutate(S = 1e6 * p_colonize * H / (D * (1 - p_colonize + H)))

write_rds(df, "objects/spatially-implicit-scaling-results.rds")
