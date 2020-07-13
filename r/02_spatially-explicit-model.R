source("r/header.R")

df <- crossing(
  H = c(0, 0.01, 0.1),
  D = set_units(sort(c(25, 500, 10 ^ seq(1, 3.5, by = 0.025))), 1 / mm ^ 2),
  S = set_units(10 ^ seq(1, 3.5, by = 0.025), um ^ 2)
) %>%
  mutate(f_s = D * S) %>%
  filter(f_s < set_units(1/3))

# Calculate g_smax ----
# Based on Monteith and Unsworth Table A.3 (pg. 376)
D_wv <- set_units(24.9e-6, m ^ 2 / s)
v <- set_units(2.24e-2, m^3 / mol)
df %<>%
  mutate(
    b = biophysical_constant(D_wv, v),
    m = morphological_constant(0.5, 0.5, 0.5),
    g_smax = set_units(b * m * D * sqrt(S), mol / m ^ 2 / s)
  )

safe_pcolonize <- safely(p_colonize)

plan(multisession)
df$p_colonize <- df %>% 
  select(D, S, H) %>%
  furrr::future_pmap_dbl(~ {
    res <- safe_pcolonize(..1, ..2, ..3)
    if (is.null(res$result)) {
      return(NA)
    } else {
      return(res$result)
    }
  }, .progress = TRUE)

df %<>% 
  filter(!is.na(p_colonize)) %>%
  mutate_if(~ {inherits(.x, "units")}, drop_units)

write_rds(df, "objects/spatially-explicit-results.rds")
