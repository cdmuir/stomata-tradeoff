source("r/header.R")

df <- read_rds("objects/scaling-results.rds") %>%
  ungroup() %>%
  mutate(x = D * S) %>%
  mutate_if(~ {inherits(.x, "units")}, drop_units) %>%
  mutate(H = glue("H = {H}", H = H))

df_refline <- df %>%
  filter(p_colonize == median(p_colonize)) %>%
  group_by(H) %>%
  summarize(min_D = min(D), max_D = max(D), D = median(D), S = median(S)) %>%
  mutate(d = log(D), s = log(S)) %>%
  crossing(slope = c(-1, -2)) %>%
  mutate(
    intercept = s - slope * d,
         min_S = exp(intercept + slope * log(min_D)),
         max_S = exp(intercept + slope * log(max_D))
    ) %>%
  select(H, D = min_D, xend = max_D, S = min_S, yend = max_S) %>%
  mutate(p_colonize = NA)

df_labels <- df %>%
  filter(p_colonize %in% c(0.025, 0.1, 0.4)) %>%
  group_by(H, p_colonize) %>%
  filter(D == min(D)) 

fig4 <- ggplot(df, aes(D, S, group = p_colonize)) +
  facet_wrap(H ~ .) +
  geom_segment(
    data = df_refline, 
    mapping = aes(xend = xend, yend = yend),
    linetype = "dashed", color = "grey", size = 1.1
  ) +
  geom_line() +
  geom_text(
    data = df_labels,
    aes(label = p_colonize),
    hjust = 1.1, vjust = 0.5
  ) +
  scale_x_log10(breaks = 10 ^ (1:3), limits = c(1, 3500)) +
  scale_y_log10(breaks = 10 ^ (1:3)) +
  xlab(expression(paste("Stomatal density [m", m^-2, "]"))) +
  ylab(expression(paste("Stomatal size [", mu, m^2, "]"))) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

fig4

# ggsave("figures/fig4.jpeg", w = 6.5, h = 3, units = "in")
ggsave("figures/fig4.pdf", w = 6.5, h = 3, units = "in")
