source("r/header.R")

df1 <- read_rds("objects/spatially-explicit-results.rds") %>%
  mutate(p_colonize = f_s * (1 + (1 - f_s) / (H + f_s))) %>%
  filter(D %in% c(100, 500)) %>%
  mutate(H = glue("H = {H}", H = H), D = factor(round(D), levels = c("100", "500")))


fS2a <- ggplot(df1, aes(x = f_s, y = p_colonize, linetype = D)) +
  facet_grid(H ~ .) +
  geom_line(size = 1.1, lineend = "round") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab(expression(paste("Stomatal cover (", italic(f)[s], ")"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(
    values = c(2, 1), 
    name = expression(paste("Stomatal density [m", m^-2, "]"))
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(0.3, 0, 0, 0, unit = "cm")),
    legend.key.size = unit(0.75, "in"),
    legend.position = "top",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

l <- get_legend(fS2a)

fS2a <- fS2a + theme(legend.position = "none")

df2 <- df1 %>%
  filter(g_smax < 5)

fS2b <- ggplot(df2, aes(x = g_smax, y = p_colonize, linetype = D)) +
  facet_wrap(H ~ ., strip.position = "left", ncol = 1) +
  geom_line(size = 1.1, lineend = "round") +
  xlab(expression(paste("Stomatal conductance (", italic(g)[list(s,max)], ") [mol ", m^-2~s^-1, "]"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

fS2 <- plot_grid(fS2a, fS2b, labels = "auto", align = "hv", axis = "lr")

plot_grid(l, fS2, ncol = 1, rel_heights = c(0.1, 0.9))
# ggsave("figures/figS2.jpeg", width = 6.5, height = 6.5, units = "in")
ggsave("figures/figS2.pdf", width = 6.5, height = 6.5, units = "in")

