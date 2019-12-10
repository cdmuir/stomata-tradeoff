source("r/header.R")

df <- read_rds("objects/spatially-explicit-results.rds")

# Effect of f_s and gmax om p_colonize ----

df1 <- df %>%
  filter(D %in% c(10, 100, 1000)) %>%
  mutate(H = glue("H = {H}", H = H))

f3a <- ggplot(df1, aes(x = f_s, y = p_colonize, linetype = as.character(D))) +
  facet_grid(H ~ .) +
  geom_line(size = 1.1, lineend = "round") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab(expression(paste("Stomatal cover (", italic(f)[s], ")"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(
    values = 3:1, 
    name = expression(paste("Stomatal density [m", m^-2, "]"))
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(0.3, 0, 0, 0, unit = "cm")),
    legend.position = "top",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

l <- get_legend(f3a)

f3a <- f3a + theme(legend.position = "none")

df2 <- df1 %>%
  filter(g_smax < 5)

f3b <- ggplot(df2, aes(x = g_smax, y = p_colonize, linetype = as.character(D))) +
  facet_wrap(H ~ ., strip.position = "left", ncol = 1) +
  geom_line(size = 1.1, lineend = "round") +
  xlab(expression(paste("Stomatal conductance (", italic(g)[list(s,max)], ") [mol ", m^-2~s^-1, "]"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(values = 3:1) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

f3b

f3 <- plot_grid(f3a, f3b, labels = "auto", align = "hv", axis = "lr")

plot_grid(l, f3, ncol = 1, rel_heights = c(0.1, 0.9))
ggsave("figures/fig3.pdf", width = 6.5, height = 6.5, units = "in")
